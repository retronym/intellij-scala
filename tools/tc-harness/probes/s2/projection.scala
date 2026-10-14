trait Graph { type Node; type Edge; def nodes: List[Node]; def edges(n: Node): List[Edge] }
object G1 extends Graph { type Node = Int; type Edge = (Int, Int); def nodes = List(1); def edges(n: Int) = List((n, n)) }
object Pj {
  def first[G <: Graph](g: G): Option[g.Node] = g.nodes.headOption
  val n: Option[Int] = first(G1)
  def proj(e: Graph#Edge): Graph#Edge = e
  val e: G1.Edge = (1, 2)
  val pe = proj(e)
  type __q_n = G1.Node
}
// ? C G1.Node ; Graph#Node
// ? E n ; Int
