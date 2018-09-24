case class Node(
   name: String
   , path: String
   , parent: Option[Node] = None
   , var sub: Option[Seq[Node]] = None
   , var files: Option[Seq[File]] = None)
