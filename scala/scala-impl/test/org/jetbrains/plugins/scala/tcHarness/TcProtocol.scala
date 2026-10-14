package org.jetbrains.plugins.scala.tcHarness

import com.google.gson.{Gson, GsonBuilder, JsonArray, JsonElement, JsonNull, JsonObject, JsonParser, JsonPrimitive}

import java.nio.file.{Files, Paths}
import scala.jdk.CollectionConverters.*

/**
 * The JSON protocol of the type-checking harness. See README.md in this package.
 *
 * A request names an environment (Scala version, JDK, classpath, compiler options), a set of
 * source files and optional type queries. The environment is the cache key of the warm project:
 * requests with the same one reuse it, a different one rebuilds the light project.
 */
object TcProtocol {

  final case class Env(scalaVersion: String, jdk: String, classpath: Seq[String], scalacOptions: Seq[String])

  final case class SourceFile(path: String, text: String)

  /**
   * Operands name a definition in the sources: a type alias `N` (or `__q_N`) stands for its
   * right-hand side, a val/var/def/object `N` for its (result) type, a class `N` for its class
   * (only as the second operand of `baseType`). Use unique names; the first match in file order wins.
   */
  sealed trait Query {
    def encode: String
    def toJson: JsonObject
  }
  object Query {
    final case class Conforms(a: String, b: String) extends Query {
      def encode = s"C\t$a\t$b"
      def toJson = obj("kind" -> str("conforms"), "a" -> str(a), "b" -> str(b))
    }
    final case class Equiv(a: String, b: String) extends Query {
      def encode = s"E\t$a\t$b"
      def toJson = obj("kind" -> str("equiv"), "a" -> str(a), "b" -> str(b))
    }
    final case class BaseType(a: String, cls: String) extends Query {
      def encode = s"B\t$a\t$cls"
      def toJson = obj("kind" -> str("baseType"), "a" -> str(a), "cls" -> str(cls))
    }
    final case class TypeOf(a: String) extends Query {
      def encode = s"T\t$a"
      def toJson = obj("kind" -> str("type"), "a" -> str(a))
    }

    def fromJson(o: JsonObject): Query = {
      def s(k: String) = Option(o.get(k)).map(_.getAsString).getOrElse(throw new IllegalArgumentException(s"query without '$k': $o"))
      s("kind") match {
        case "conforms" => Conforms(s("a"), s("b"))
        case "equiv"    => Equiv(s("a"), s("b"))
        case "baseType" => BaseType(s("a"), s("cls"))
        case "type"     => TypeOf(s("a"))
        case other      => throw new IllegalArgumentException(s"unknown query kind $other")
      }
    }
  }

  /** `fresh`: set the project up again for this request, to rule out state left by earlier ones. */
  final case class Request(env: Env, files: Seq[SourceFile], queries: Seq[Query], oracle: Boolean, warnings: Boolean, fresh: Boolean = false)

  /** `severity` is `error` or `warning`; `line`/`col` are 1-based. */
  final case class Diagnostic(file: String, line: Int, col: Int, severity: String, message: String) {
    def toJson: JsonObject = obj("file" -> str(file), "line" -> num(line), "col" -> num(col), "severity" -> str(severity), "message" -> str(message))
  }

  def parseRequest(json: String): Request = {
    val o = JsonParser.parseString(json).getAsJsonObject
    def strings(k: String): Seq[String] = Option(o.get(k)).toSeq.flatMap(_.getAsJsonArray.asScala.map(_.getAsString))
    def string(k: String, default: String): String = Option(o.get(k)).filterNot(_.isJsonNull).map(_.getAsString).getOrElse(default)
    def bool(k: String, default: Boolean): Boolean = Option(o.get(k)).filterNot(_.isJsonNull).map(_.getAsBoolean).getOrElse(default)
    val env = Env(
      scalaVersion = string("scalaVersion", "2.13"),
      jdk = string("jdk", "17"),
      classpath = strings("classpath"),
      scalacOptions = strings("scalacOptions")
    )
    val files = Option(o.get("files")).toSeq.flatMap(_.getAsJsonArray.asScala).map { e =>
      val f = e.getAsJsonObject
      val ref = Option(f.get("file")).map(_.getAsString)
      val path = Option(f.get("path")).map(_.getAsString).orElse(ref.map(r => Paths.get(r).getFileName.toString))
        .getOrElse(throw new IllegalArgumentException(s"file without 'path': $f"))
      val text = Option(f.get("text")).map(_.getAsString).orElse(ref.map(r => Files.readString(Paths.get(r))))
        .getOrElse(throw new IllegalArgumentException(s"file without 'text' or 'file': $f"))
      SourceFile(path, text)
    }
    require(files.nonEmpty, "no files")
    require(files.map(_.path).distinct.size == files.size, "duplicate file paths")
    val queries = Option(o.get("queries")).toSeq.flatMap(_.getAsJsonArray.asScala).map(q => Query.fromJson(q.getAsJsonObject))
    Request(env, files, queries, oracle = bool("oracle", true), warnings = bool("warnings", false), fresh = bool("fresh", false))
  }

  // tiny JSON builders
  def obj(kvs: (String, JsonElement)*): JsonObject = {
    val o = new JsonObject
    kvs.foreach { case (k, v) => o.add(k, v) }
    o
  }
  def arr(xs: Iterable[JsonElement]): JsonArray = {
    val a = new JsonArray
    xs.foreach(a.add)
    a
  }
  def str(s: String): JsonElement = if (s == null) JsonNull.INSTANCE else new JsonPrimitive(s)
  def num(n: Long): JsonElement = new JsonPrimitive(n)
  def bool(b: Boolean): JsonElement = new JsonPrimitive(b)

  private val gson: Gson = new GsonBuilder().setPrettyPrinting().disableHtmlEscaping().create()
  def render(e: JsonElement): String = gson.toJson(e)
}
