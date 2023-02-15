package library
object Application extends App{

object HtmlExample {
  val exemple = Tag("html", List(),
    List(
      Tag("head", List(),
        List(
          Tag("meta", List(("charset", "utf-8")), List()),
          Tag("title", List(), List(Texte("My Page"))))),
      Tag("body", List(), List(
        Tag("center", List(), List(
          Tag("a", List(("href", "http://www.irisa.fr")),
            List(Texte("Lien")))))))))
}


 def filtreAnnonce(h: Html): List[String] = {
  var stUrls: List[String] = List()
  //val attrgs : List[(String , String)] = h.attributes
  h match {
    case Tag(_, attributes, enfants) => {
      for(attr <- attributes) {
        attr match {
          case ("href", lien) =>   stUrls =  lien :: stUrls
          case _ => {}
        }
      }
      for(child <- enfants) {
        stUrls = stUrls ++ filtreAnnonce(child)
      }
    }
    case _ => {}
  }
  stUrls
}


println(filtreAnnonce(HtmlExample.exemple))

}
