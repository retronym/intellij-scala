package org.jetbrains.plugins.scala.lang.psi.types.recursiveUpdate

import java.util.concurrent.ConcurrentHashMap
import java.util.concurrent.atomic.LongAdder

// EXPERIMENT ONLY (not for commit): counters for the asSeenFrom-vs-chain cost model.
// Enabled with -Dasf.stats=true (or env ASF_STATS=true); key tracking with ASF_STATS_KEYS=true.
object AsfStats {
  val enabled: Boolean = sys.props.get("asf.stats").orElse(sys.env.get("ASF_STATS")).contains("true")
  val keysEnabled: Boolean = enabled && sys.env.get("ASF_STATS_KEYS").contains("true")

  private val counters = new ConcurrentHashMap[String, LongAdder]()
  def inc(name: String, by: Long = 1): Unit =
    if (enabled) counters.computeIfAbsent(name, _ => new LongAdder).add(by)

  // Histogram: bucket values exactly up to 16, then by powers of two.
  def hist(name: String, v: Int): Unit = if (enabled) {
    val b = if (v <= 16) v.toString else s"<=${Integer.highestOneBit(v - 1) << 1}"
    inc(s"$name[$b]")
    inc(s"$name.sum", v)
    inc(s"$name.n")
  }

  // Inclusive time of the outermost activation per thread, so nested (re-entrant) calls aren't double counted.
  private val depths = new ConcurrentHashMap[String, ThreadLocal[Array[Int]]]()
  def timed[T](name: String)(body: => T): T =
    if (!enabled) body
    else {
      val d = depths.computeIfAbsent(name, _ => ThreadLocal.withInitial(() => Array(0))).get
      inc(s"$name.calls")
      if (d(0) > 0) inc(s"$name.nested")
      d(0) += 1
      val t0 = if (d(0) == 1) System.nanoTime() else 0L
      try body
      finally {
        d(0) -= 1
        if (d(0) == 0) inc(s"$name.topNanos", System.nanoTime() - t0)
      }
    }

  private val keys = new ConcurrentHashMap[String, ConcurrentHashMap[AnyRef, LongAdder]]()
  def key(name: String, k: => AnyRef): Unit = if (keysEnabled) {
    keys.computeIfAbsent(name, _ => new ConcurrentHashMap()).computeIfAbsent(k, _ => new LongAdder).increment()
  }

  // Experiment memo tables (cleared with the harness's caches each rep).
  @volatile var cacheBaseType: Boolean = sys.env.get("ASF_CACHE_BASETYPE").contains("true")
  @volatile var cacheWalk: Boolean = sys.env.get("ASF_CACHE_WALK").contains("true")
  val baseTypeMemo = new ConcurrentHashMap[(AnyRef, AnyRef), Option[AnyRef]]()
  val walkMemo = new ConcurrentHashMap[(AnyRef, AnyRef, AnyRef), AnyRef]()
  @volatile var cacheCanon: Boolean = sys.env.get("ASF_CACHE_CANON").contains("true")
  val canonMemo = new ConcurrentHashMap[AnyRef, AnyRef]()

  def reset(): Unit = { counters.clear(); keys.clear(); baseTypeMemo.clear(); walkMemo.clear(); canonMemo.clear() }

  def report(): String = {
    import scala.jdk.CollectionConverters._
    val cs = counters.asScala.toSeq.sortBy(_._1).map { case (k, v) =>
      val n = v.sum
      if (k.endsWith("Nanos")) f"  $k%-48s ${n / 1e6}%.1f ms" else f"  $k%-48s $n"
    }
    val ks = keys.asScala.toSeq.sortBy(_._1).map { case (k, m) =>
      val total = m.values.asScala.map(_.sum).sum
      f"  KEYS $k%-42s distinct=${m.size} total=$total repeat=${if (total == 0) 0.0 else 1.0 - m.size.toDouble / total}%.3f"
    }
    (cs ++ ks).mkString("\n")
  }
}
