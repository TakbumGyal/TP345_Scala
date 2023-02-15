package test
import org.Junit.Assert._
import org.Junit.Test

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

@Test
def test1() : List[String] = {
    val h = HtmlExample

    assertEquals(filtreAnnonce(h) , "http://www.irisa.fr")
    assertEquals("http://www.irisa.fr" , filtreAnnonce(h))
} 
