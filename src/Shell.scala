import java.util.Scanner
import Constants._

/*
TODO: refactoring to implement complex functions. Implement grep, locate and move functionality
 */
object Shell extends App {

  val root = Node("root", "/")

  val scanner = new Scanner(System.in)
  var cmd = " "
  var curr = root

  while (!cmd.equalsIgnoreCase("exit")) {

    cmd = scanner.nextLine()

    cmd match {
      case LS =>
        val subD = curr.sub match {
          case Some(sb) => sb.map(_.name)
          case _ => Seq()
        }

        val files = curr.files match {
          case Some(f) => f.map(_.name)
          case _ => Seq()
        }

        (subD ++ files).foreach(println)

      case PWD =>
        println(curr.path)


      case CD =>
        curr = root

      case _ => None

    }

    /*
    move across directories
    TODO: Implement backward navigation
     */
    cmd.contains(CD) && !cmd.equals(CD) && !cmd.endsWith("..") match {
      case true =>
        val directory = cmd.split(" ")(1)
        val dir = directory.charAt(0) match {
          case '/' =>
            curr = root
            directory.substring(1)
          case _ => directory
        }

        dir.contains("/") match {
          case true =>
            val paths = dir.split("/")
            val temp = fetchNode(paths, curr)
            curr = temp match {
              case None =>
                println("Incorrect Path Provided")
                curr
              case Some(nd) => nd
            }
          case false =>
            val temp = fetchNode(Seq(dir), curr)
            curr = temp match {
              case None =>
                println("Incorrect Path Provided")
                curr
              case Some(nd) => nd
            }
        }
      case false => None
    }

    cmd.contains(CD) && cmd.endsWith("..") match {
      case true =>
        curr = curr.parent match {
          case Some(p) => p
          case None => root
        }
      case false => None
    }

    /*
    create a directory
     */
    cmd.contains(MKDIR) match {
      case true =>
        val directoryName = cmd.split(" ")(1)
        val newNode = Node(directoryName, curr.path + directoryName + "/", Some(curr))
        curr.sub = curr.sub match {
          case Some(sb) => Some(sb ++ Seq(newNode))
          case None => Some(Seq(newNode))
        }
      case false => None
    }

    /*
    read file present in current directory
    TODO: allow read across all directories
     */
    cmd.contains(READ) match {
      case true =>
        val fileName = cmd.split(" ")(1)
        fileName.contains("/") match {
          case false =>
            curr.files match {
              case Some(fi) =>
                val filtered = fi.filter(_.name == fileName)
                filtered.isEmpty match {
                  case true => println("No such file found")
                  case false => println(filtered.head.content)
                }
              case None => println("No such file found")
            }
          case true =>
            val paths = fileName.split("/")
            val file = reachDirectoryAndFetchFile(paths, curr)
            file match {
              case Some(f) => println(f.content)
              case None => println("Incorrect Path Provided")
            }
        }

      case false => None
    }

    /*
    delete file/directory
     */
    cmd.contains(DELETE) match {
      case true =>
        val resource = cmd.split(" ")(1)
        checkAndDeleteDirectory (curr, resource.split("/").tail) match {
          case true => println ("Deleted")
          case false => println("Incorrect Input")
        }
      case false => None
    }

    /*
    create file in current directory
    TODO: allow file creation in any directory
     */
    cmd.contains(CREATE) match {
      case true =>
        val fileName = cmd.split(" ")(1)
        val fileContent = cmd.split(" ")(2)
        fileName.contains("/") match {
          case false =>
            curr.files = curr.files match {
              case None => Some(Seq(File(fileName, fileContent)))
              case Some(fi) => Some(fi ++ Seq(File(fileName, fileContent)))
            }
          case true =>
            val paths = fileName.split("/")
            reachDirectoryAndCreateFile(paths, curr, File(paths.last, fileContent))
        }
      case false => None
    }
  }

  def reachDirectoryAndCreateFile(paths: Seq[String], node: Node, file: File): Boolean = {
    paths.length == 1 match {
      case true =>
        node.files = node.files match {
          case Some(fi) => Some(fi ++ Seq(file))
          case None => Some(Seq(file))
        }
        true
      case false =>
        node.sub match {
          case None => false
          case Some(sb) =>
            val sd = sb.find(_.name == paths.head)
            sd match {
              case None => false
              case Some(s) =>
                reachDirectoryAndCreateFile(paths.tail, s, file)
            }
        }
    }
  }

  def reachDirectoryAndFetchFile(paths: Seq[String], node: Node): Option[File] = {
    paths.length == 1 match {
      case true =>
        node.files match {
          case Some(fi) => fi.find(_.name == paths.head)
          case None => None
        }
      case false =>
        node.sub match {
          case None => None
          case Some(sb) =>
            val sd = sb.find(_.name == paths.head)
            sd match {
              case None => None
              case Some(s) =>
                reachDirectoryAndFetchFile(paths.tail, s)
            }
        }
    }
  }

  def checkAndDeleteFile(curr: Node, fileName: String): Boolean = {
    val temp = curr.files match {
      case Some(fi) =>
        val fileCount = fi.length
        val filtered = fi.filter(_.name == fileName)
        filtered.isEmpty match {
          case false =>
            fileCount match {
              case 1 => None
              case _ => Some(fi.filter(_.name != fileName))
            }
          case true => curr.files
        }
      case None => None
    }

    val status = (temp, curr.files) match {
      case (None, None) => false
      case (None, Some(value)) => value.length == 1
      case (Some(f1), Some(f2)) => f1.length == f2.length
      case (_, _) => false
    }

    curr.files = temp
    status

  }

  def checkAndDeleteDirectory(node: Node, paths: Seq[String]): Boolean = {
    paths.length == 1 match {
      case true =>
        val status = checkAndDeleteFile(node, paths.head)
        status match {
          case true => true
          case false =>
            node.sub match {
              case None => false
              case Some(sb) =>
                val subList = sb.filter(_.name == paths.head)
                subList.isEmpty match {
                  case true => false
                  case false =>
                    node.sub = Some(sb.filter(s => s.name != paths.head))
                    true
                }
            }
        }

      case false =>
        node.sub match {
          case None => false
          case Some(sb) =>
            val filtered = sb.filter(_.name == paths.head)
            val next = filtered.isEmpty match {
              case false => filtered.head
              case true => return false
            }
            checkAndDeleteDirectory(next, paths.tail)
        }
    }

  }

  def fetchNode(paths: Seq[String], node: Node): Option[Node] = {
    paths.isEmpty match {
      case true => Some(node)
      case false =>
        node.sub match {
          case None => None
          case Some(sb) =>
            val filtered = sb.filter(_.name == paths.head)
            val next = filtered.isEmpty match {
              case false => filtered.head
              case true => return None
            }
            fetchNode(paths.tail, next)
        }
    }
  }

}