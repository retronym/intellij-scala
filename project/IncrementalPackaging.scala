import org.jetbrains.sbtidea.Keys.doPatchPluginXml
import org.jetbrains.sbtidea.packaging.*
import org.jetbrains.sbtidea.packaging.PackagingKeys.{packageArtifact, packageMappings, packageOutputDir}
import org.jetbrains.sbtidea.packaging.artifact.{ClassShader, MappingArtifactBuilder, NoOpClassShader}
import sbt.*
import sbt.Keys.*
import sbt.internal.inc.HashUtil
import upickle.default.{ReadWriter, macroRW, read, write}

import java.net.URI
import java.nio.file.*
import java.nio.file.attribute.BasicFileAttributes
import java.util.Collections
import scala.collection.mutable
import scala.util.control.NonFatal

/**
 * A correct incremental implementation of `packageArtifact`.
 *
 * The stock implementation in sbt-idea-plugin (`DistBuilder` + `PersistentIncrementalCache`) decides whether to copy a
 * class file into an already existing jar by comparing its mtime with the mtime it recorded last time, and only copies
 * when the new mtime is strictly greater. That misses:
 *   - files it has never seen before: a brand-new class file is recorded on first sight and then compared with itself,
 *     so a newly added class never reaches the jar until the jar is deleted;
 *   - files whose mtime went backwards or stayed the same while their content changed: branch switches, rebases,
 *     build outputs seeded from another checkout, class files restored from the remote compilation cache;
 *   - the cache and the jars living independently of each other: a cache file from another checkout (absolute paths
 *     as keys) or an out-of-date cache next to freshly copied jars silently produces stale plugins;
 *   - entries whose source disappeared (deleted or renamed classes, bumped library versions) are never removed.
 *
 * This implementation instead keeps, per output (jar, copied jar or copied directory), the exact set of entries it
 * last wrote together with a fingerprint of each entry's input and a signature of the output itself. On every run it
 * computes the desired entries from the mappings, diffs them against the record and writes only the difference,
 * deleting entries and outputs that are no longer wanted. An output that doesn't match its record (missing, replaced,
 * or produced by something else) is rebuilt from scratch, as is the whole output directory when there is no record for
 * it at all.
 *
 * Fingerprints are mtime + size (one `stat` per file, which the directory walk gives us for free), except for the
 * patched `plugin.xml` files, which are regenerated as fresh temp files on every build and therefore use a content
 * hash. Jars merged into an output jar are fingerprinted as a whole: if the jar changes, all its entries are rewritten.
 */
object IncrementalPackaging {

  /** Drop-in replacement for the `packageArtifact` task that sbt-idea-plugin defines, keeping its ordering constraints. */
  def packageArtifactTask: Def.Initialize[Task[File]] = Def.sequential(
    packageMappings,
    doPatchPluginXml,
    Def.task {
      val outputDir = packageOutputDir.value
      val stateFile = target.value / "packaging-state.json"
      new IncrementalDistBuilder(streams.value, stateFile.toPath, outputDir.toPath).produceArtifact(packageMappings.value)
      outputDir
    }
  )

  // What we last wrote. `entries` maps an entry path relative to the output (or "" for the output itself) to the
  // fingerprint of the input it was written from. `signature` is the output file's own stamp after we last touched it,
  // or "" for directory outputs.
  final case class Input(source: String, stamp: String)
  final case class OutputRecord(signature: String, entries: Map[String, Input])
  final case class State(outputDir: String, outputs: Map[String, OutputRecord])

  implicit val inputRW: ReadWriter[Input] = macroRW
  implicit val outputRecordRW: ReadWriter[OutputRecord] = macroRW
  implicit val stateRW: ReadWriter[State] = macroRW

  private def stamp(path: Path, attrs: BasicFileAttributes): String =
    s"${attrs.lastModifiedTime().toMillis}/${attrs.size()}"

  private def stamp(path: Path): String =
    stamp(path, Files.readAttributes(path, classOf[BasicFileAttributes]))

  private def contentStamp(path: Path): String = s"hash:${HashUtil.farmHash(path)}"

  private def isJar(path: Path): Boolean = path.toString.endsWith(".jar")

  private def splitPatchTarget(to: Path): (Path, String) = {
    val Array(jar, inner) = to.toString.split("!", 2)
    (Paths.get(jar), inner.replace('\\', '/').stripPrefix("/"))
  }

  // Entries contributed by one input of a jar/directory output: a directory tree, or a whole jar.
  private sealed trait Source {
    def id: String
    def scan(exclude: ExcludeFilter): Seq[(String, Input)]
    def withFiles[A](body: (String => Path) => A): A
  }

  private final case class DirectorySource(root: Path) extends Source {
    val id: String = root.toString
    def scan(exclude: ExcludeFilter): Seq[(String, Input)] = {
      val result = Vector.newBuilder[(String, Input)]
      Files.walkFileTree(root, new SimpleFileVisitor[Path] {
        override def visitFile(file: Path, attrs: BasicFileAttributes): FileVisitResult = {
          val relative = root.relativize(file)
          if (!exclude(relative))
            result += entryName(relative) -> Input(id, stamp(file, attrs))
          FileVisitResult.CONTINUE
        }
      })
      result.result()
    }
    def withFiles[A](body: (String => Path) => A): A = body(entry => root.resolve(entry))
  }

  private final case class JarSource(jar: Path) extends Source {
    val id: String = jar.toString
    def scan(exclude: ExcludeFilter): Seq[(String, Input)] = {
      val input = Input(id, stamp(jar))
      val result = Vector.newBuilder[(String, Input)]
      withFiles { resolve =>
        val root = resolve("")
        Files.walkFileTree(root, new SimpleFileVisitor[Path] {
          override def visitFile(file: Path, attrs: BasicFileAttributes): FileVisitResult = {
            val relative = root.relativize(file)
            // The exclude filter is written against default-filesystem paths (see Common.excludePathsFromPackage).
            if (!exclude(Paths.get(entryName(relative))))
              result += entryName(relative) -> input
            FileVisitResult.CONTINUE
          }
        })
      }
      result.result()
    }
    def withFiles[A](body: (String => Path) => A): A = {
      val fs = FileSystems.newFileSystem(URI.create("jar:" + jar.toUri), Collections.emptyMap[String, Any]())
      try body(entry => fs.getPath("/" + entry))
      finally fs.close()
    }
  }

  private def entryName(relative: Path): String = {
    val separator = relative.getFileSystem.getSeparator
    relative.toString.replace(separator, "/")
  }

  private def sourceFor(from: Path): Source =
    if (Files.isDirectory(from)) DirectorySource(from)
    else if (isJar(from)) JarSource(from)
    else sys.error(s"Unsupported packaging input (expected a directory or a jar): $from")

  final class IncrementalDistBuilder(streams: TaskStreams, stateFile: Path, outputDir: Path)
    extends MappingArtifactBuilder[File] {

    private val log = streams.log

    private val previous: Map[String, OutputRecord] = loadState()
    private val current = mutable.LinkedHashMap.empty[String, OutputRecord]
    private var patchedEntries: Map[Path, Set[String]] = Map.empty
    private var filesWritten = 0
    private var entriesDeleted = 0

    private def loadState(): Map[String, OutputRecord] = {
      val loaded =
        if (!Files.exists(stateFile)) Left("no packaging state recorded yet")
        else
          try {
            val state = read[State](new String(Files.readAllBytes(stateFile), "UTF-8"))
            if (state.outputDir == outputDir.toString) Right(state.outputs)
            else Left(s"packaging state belongs to a different output directory (${state.outputDir})")
          } catch {
            case NonFatal(e) => Left(s"packaging state is unreadable (${e.getMessage})")
          }
      loaded match {
        case Right(outputs) => outputs
        case Left(reason) =>
          if (Files.exists(outputDir)) {
            log.info(s"packageArtifact: $reason; rebuilding $outputDir from scratch")
            IO.delete(outputDir.toFile)
          }
          Map.empty
      }
    }

    private def saveState(): Unit = {
      val json = write(State(outputDir.toString, current.toMap), indent = 2)
      Files.createDirectories(stateFile.getParent)
      val tmp = stateFile.resolveSibling(stateFile.getFileName.toString + ".tmp")
      Files.write(tmp, json.getBytes("UTF-8"))
      Files.move(tmp, stateFile, StandardCopyOption.REPLACE_EXISTING, StandardCopyOption.ATOMIC_MOVE)
    }

    // A record is only trusted if the output it describes is still exactly the file we left behind.
    private def trustedRecord(output: Path): Option[OutputRecord] =
      previous.get(output.toString).filter { record =>
        Files.exists(output) && (record.signature.isEmpty || record.signature == stamp(output))
      }

    override protected def mappingFilter(m: Mapping): Boolean = true

    override def produceArtifact(structure: Mappings): File = {
      patchedEntries = structure
        .filter(_.to.toString.contains("jar!"))
        .groupBy(m => splitPatchTarget(m.to.toPath)._1)
        .map { case (jar, ms) => jar -> ms.map(m => patchEntryName(m)).toSet }
      val start = System.nanoTime()
      val result = super.produceArtifact(structure)
      val millis = (System.nanoTime() - start) / 1000000
      log.info(s"packageArtifact: wrote $filesWritten and deleted $entriesDeleted entries in ${millis}ms")
      result
    }

    private def patchEntryName(m: Mapping): String = {
      val (_, inner) = splitPatchTarget(m.to.toPath)
      if (inner.endsWith("/") || inner.isEmpty) inner + m.from.getName else inner
    }

    override protected def copySingleJar(mapping: Mapping): Unit = {
      val to = mapping.to.toPath
      val input = Input(mapping.from.toString, stamp(mapping.from.toPath))
      val unchanged = trustedRecord(to).exists(_.entries.get("").contains(input))
      if (!unchanged) {
        Files.createDirectories(to.getParent)
        Files.copy(mapping.from.toPath, to, StandardCopyOption.REPLACE_EXISTING)
        filesWritten += 1
        log.debug(s"copied ${mapping.from} -> $to")
      }
      current(to.toString) = OutputRecord(stamp(to), Map("" -> input))
    }

    override protected def copyDirs(mappings: Mappings): Unit = mappings.foreach { mapping =>
      val to = mapping.to.toPath
      val from = mapping.from.toPath
      val desired: Seq[(String, Input)] =
        if (Files.isDirectory(from)) DirectorySource(from).scan(mapping.metaData.excludeFilter)
        else Seq("" -> Input(from.toString, stamp(from)))
      val record = previous.getOrElse(to.toString, OutputRecord("", Map.empty))
      val desiredMap = desired.toMap

      record.entries.keys.filterNot(desiredMap.contains).foreach { entry =>
        if (Files.deleteIfExists(to.resolve(entry))) entriesDeleted += 1
      }
      desired.foreach { case (entry, input) =>
        val target = to.resolve(entry)
        if (!record.entries.get(entry).contains(input) || !Files.exists(target)) {
          Files.createDirectories(target.getParent)
          Files.copy(from.resolve(entry), target, StandardCopyOption.REPLACE_EXISTING)
          filesWritten += 1
        }
      }
      current(to.toString) = OutputRecord("", desiredMap)
    }

    override protected def packageJar(to: Path, mappings: Mappings): Unit = {
      val exclude = ExcludeFilter.merge(mappings.map(_.metaData.excludeFilter))
      val shadePatterns = mappings.flatMap(_.metaData.shading).distinct
      val patched = patchedEntries.getOrElse(to, Set.empty)
      val sources = mappings.map(m => m.from.toPath).filter(Files.exists(_)).map(sourceFor)

      // Later mappings win, as in the stock packager; patched entries belong to the patch step.
      val desired = mutable.LinkedHashMap.empty[String, Input]
      sources.foreach(s => s.scan(exclude).foreach { case (entry, input) => if (!patched(entry)) desired(entry) = input })

      val record = trustedRecord(to).getOrElse(OutputRecord("", Map.empty))
      val toDelete = record.entries.keys.filterNot(e => desired.contains(e) || patched(e)).toSeq
      val toWrite = desired.filter { case (entry, input) => !record.entries.get(entry).contains(input) }

      // Shading can rename entries, so a shaded jar cannot be patched entry by entry; rebuild it whole instead.
      val fromScratch = record.entries.isEmpty || shadePatterns.nonEmpty
      if (toDelete.nonEmpty || toWrite.nonEmpty) {
        if (fromScratch) Files.deleteIfExists(to)
        val shader = if (shadePatterns.nonEmpty) new ClassShader(shadePatterns)(streams) else new NoOpClassShader
        val writes = if (fromScratch) desired else toWrite
        withOutputJar(to) { fs =>
          if (!fromScratch) toDelete.foreach(entry => deleteEntry(fs.getPath("/" + entry)))
          writes.groupBy(_._2.source).foreach { case (sourceId, entries) =>
            sources.find(_.id == sourceId).get.withFiles { resolve =>
              entries.keys.foreach { entry =>
                val target = fs.getPath("/" + entry)
                val parent = target.getParent
                if (parent != null) Files.createDirectories(parent)
                val from = resolve(entry)
                shader.applyShading(from, target) {
                  Files.copy(from, target, StandardCopyOption.REPLACE_EXISTING)
                }
              }
            }
          }
        }
        filesWritten += writes.size
        entriesDeleted += (if (fromScratch) 0 else toDelete.size)
        log.info(s"packageJar: ${if (fromScratch) "rebuilt" else s"updated (+${writes.size}/-${toDelete.size})"} $to")
      }
      if (Files.notExists(to)) withOutputJar(to)(_ => ()) // a module with nothing to package still gets its (empty) jar
      // Keep the patch entries we wrote last time unless the jar was rebuilt; the patch step re-validates them against
      // their content hash.
      val kept = if (fromScratch) Map.empty[String, Input] else record.entries.filter { case (entry, _) => patched(entry) }
      current(to.toString) = OutputRecord(stamp(to), desired.toMap ++ kept)
    }

    override protected def patch(to: Path, mappings: Mappings): Unit = {
      val (jar, _) = splitPatchTarget(to)
      val record = current.getOrElse(jar.toString, sys.error(s"patch target $jar was not produced by this build"))
      val writes = mappings.map { m => patchEntryName(m) -> (m.from.toPath, Input(s"patch:${patchEntryName(m)}", contentStamp(m.from.toPath))) }
        .filterNot { case (entry, (_, input)) => record.entries.get(entry).contains(input) }
      if (writes.nonEmpty) {
        withOutputJar(jar) { fs =>
          writes.foreach { case (entry, (from, _)) =>
            val target = fs.getPath("/" + entry)
            val parent = target.getParent
            if (parent != null) Files.createDirectories(parent)
            Files.copy(from, target, StandardCopyOption.REPLACE_EXISTING)
          }
        }
        filesWritten += writes.size
        log.info(s"patch: wrote ${writes.map(_._1).mkString(", ")} into $jar")
      }
      current(jar.toString) = OutputRecord(stamp(jar), record.entries ++ writes.map { case (entry, (_, input)) => entry -> input })
    }

    override protected def unknown(mappings: Mappings): Unit =
      sys.error(s"Don't know how to package $mappings")

    override protected def createResult: File = {
      // Outputs we produced on an earlier run but that no mapping asked for this time (e.g. the previous version of a
      // bumped library) must not linger on the plugin classpath.
      previous.foreach { case (output, record) =>
        if (!current.contains(output)) {
          val path = Paths.get(output)
          val files = if (record.signature.nonEmpty || record.entries.contains("")) Seq(path) else record.entries.keys.map(path.resolve).toSeq
          files.foreach(f => if (Files.deleteIfExists(f)) entriesDeleted += 1)
          log.info(s"packageArtifact: removed stale output $output")
        }
      }
      saveState()
      outputDir.toFile
    }

    // Delete a jar entry and whatever directory entries it leaves empty behind it.
    private def deleteEntry(entry: Path): Unit = {
      Files.deleteIfExists(entry)
      var dir = entry.getParent
      while (dir != null && dir.getParent != null && isEmptyDirectory(dir)) {
        Files.delete(dir)
        dir = dir.getParent
      }
    }

    private def isEmptyDirectory(dir: Path): Boolean = {
      val stream = Files.list(dir)
      try !stream.findAny().isPresent
      finally stream.close()
    }

    private def withOutputJar[A](jar: Path)(body: FileSystem => A): A = {
      Files.createDirectories(jar.getParent)
      val env = new java.util.HashMap[String, String]()
      env.put("create", String.valueOf(Files.notExists(jar)))
      val fs = FileSystems.newFileSystem(URI.create("jar:" + jar.toUri), env)
      try body(fs)
      finally fs.close()
    }
  }
}
