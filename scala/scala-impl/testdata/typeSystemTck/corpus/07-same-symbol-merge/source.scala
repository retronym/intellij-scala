// Concept: same-symbol base-type merge. When several parents contribute the same
// class symbol with different arguments, the arguments are merged by variance —
// covariant -> glb, contravariant -> lub (mergePrefixAndArgs, docs/SPEC-GAPS.md §3). This is
// the core of baseTypeSeq construction and is barely exercised elsewhere.
trait Animal
class Dog extends Animal
class Box[+A]    // covariant  -> args merge by glb
class Sink[-A]   // contravariant -> args merge by lub
