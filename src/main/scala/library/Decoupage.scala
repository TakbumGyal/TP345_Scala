package library

trait AnalysePage {

  /** A partir d'une URL de requête sur le site de référence et d'une expression
    * exp, retourne de pages issues de la requête et satisfaisant l'expression.
    *
    * @param url
    *   l'URL de la requête sur le site de référence
    * @param exp
    *   l'expression à vérifier sur les pages trouvées
    * @return
    *   la liste des couples (titre,ref) où ref est l'URL d'une page
    *   satisfaisant l'expression et titre est son titre.
    */
  val objFiltrageUrls: FiltrageURLs
  val objFiltrageHtml: FiltrageHtml


  def resultats(url: String, exp: Expression): List[(String, String)] = {
    def fonctionRecursif(urls: List[String]) = {
      urls match {
        case hd :: tl => {  
          if (filtreHtml(obtenirHtml(hd) , exp)) {
          resultList = resultList :: ( getTitre(obtenirHtml(hd)), hd) 
          resultList
          }
          else {
            fonctionRecursif(tl)
          }
        }
        }
      }

    val urls_filtrees = FiltrageURLs.filtreAnnonce(HtmlExample.exemple) // url

    var resultList: List[(String, String)] = List()
   // val page_html = obtenirHtml(url)

    fonctionRecursif(urls_filtrees)

  }
}

def getTitre (htmlDoc : html) : String ={
  html match {
    case tag(title , _ , child) =>  title
    case _ => {}
  }
}

trait FiltrageURLs {

  /** A partir d'un document Html h, rend la liste des URLs accessibles à partir
    * de h (ces URLs sont des hyperliens h) tels que ces URLs sont tous des URLs
    * d'annonces du site de référence
    *
    * @param h
    *   le document Html
    * @return
    *   la liste des URLs d'annonces contenues dans h
    */
  def filtreAnnonce(h: Html): List[String] = {
    var stUrls: List[String] = List()
    h match{
       case Tag(_, attributes, enfants) => {
        for(attr <- attributes){
          attr match{
            case("href", lien) => stUrls = lien :: stUrls
            case _ => {}
           }
          }
        for(child <- enfants){
          stUrls = stUrls ++ filtreAnnonce(child)
        }
      }
      case _ => {}
      }
      stUrls
    }
  }
  
  
object FiltrageHtml {

  /** A partir d'un document Html h et d'une requête e, dit si le document
    * satisfait l'expression e
    *
    * @param h
    *   le document Html
    * @param e
    *   l'expression
    * @return
    *   true si le document satisfait l'expression e
    */

  def filtreMap(m: Map[String, Boolean], e: Expression): Boolean = {
    e match {
      case Mot(s: String) => m.getOrElse(s, false)
      case Et(e1, e2)     => filtreMap(m, e1) && filtreMap(m, e2)
      case Ou(e1, e2)     => filtreMap(m, e1) || filtreMap(m, e2)
    }
  }

  def trouverLesMots(h: Html, m: Map[String, Boolean]): Unit = {
    h match {
      case Texte(txt) => {
        for (mot <- m.keys) {
          m.put(mot, m.getOrElse(mot, false) || txt.contains(mot))
        }
      }
      case Tag(name, attributes, children) => {
        for (child <- children) {
          trouverLesMots(child, m)
        }
      }
    }
  }

  def mapMots(m: Map[String, Boolean], e: Expression): Unit = {
    e match {
      case Mot(s: String) => m.put(s, false)
      case Et(e1, e2)     => mapMots(m, e1); mapMots(m, e2)
      case Ou(e1, e2)     => mapMots(m, e1); mapMots(m, e2)
    }
  }

  def filtreHtml(h: Html, e: Expression): Boolean = {
    val map_des_mots: Map[String, Boolean] = Map()
    mapMots(map_des_mots, e)
    trouverLesMots(h, map_des_mots)
    filtreMap(map_des_mots, e)
  }
}

trait ProductionResultat {

  /** A partir d'une liste de couples (titre,URL), produit un document Html, qui
    * liste les solutions sous la forme de liens cliquables
    *
    * @param l
    *   la liste des couples solution (titre,URL)
    * @return
    *   le document Html listant les solutions
    */
  def resultatVersHtml(l: List[(String, String)]): Html = {
    val result : List[Html] = l.foldright[Html](0) { (a,b) =>
      Tag("li", List(), List(
        Tag("a",List("href",b),List(
          Texte(a)
        ))))
    } 

    var list :Html = Tag("ul",List(), result)
    var ma_page: Html = Tag("html", List(), List(list))
  }
}

trait HtmlVersString {

  /** Produit la chaîne de caractère correspondant à un document Html
    *
    * @param h
    *   le document Html
    * @return
    *   la chaîne de caractère représentant h
    */
  def traduire(h:Html):String = {
    h match {
      case Tag(name, att, children) => genLigne(name, true, att)+tradChil(children)+genLigne(name, false, att)
      case Texte(content) => content
    }
  }

  def genLigne(s:String, b:Boolean, l:List[(String, String)]):String ={
    (l, b) match {
      case (Nil, true) => chevrons(s, true, "", "")
      case (Nil, false) => chevrons(s, false, "", "")
      case ((s1, s2) :: Nil, true) => chevrons(s, true, s1, s2)
      case ((s1, s2) :: Nil, false) => chevrons(s, false, s1, s2)
      case ((s1, s2) :: rest, true) => chevrons(s, true, s1, s2) + genLigne(s, true, rest)
      case ((s1, s2) :: rest, false) => chevrons(s, false, s1, s2) + genLigne(s, false, rest)

    }
  }

  def tradChil(l:List[Html]):String={
    l match {
      case Nil => ""
      case x :: Nil => x match {
        case Tag(_, _, _) => traduire(x)
        case Texte(content) => content
      }
      case x :: rest => x match {
        case Tag(_, _, _) => traduire(x) + tradChil(rest)
        case Texte(content) => content + tradChil(rest) 
        }
      }
    }

  def chevrons(s:String, b:Boolean, s1:String, s2:String):String={
    (s, b, s1, s2) match {
      case ("title",true, "", "") => "<"+s+">"
      case ("title",false, "", "") => "</"+s+">"+"\n"
      case (_,true, "", "") => "<"+s+">"+"\n"
      case (_,false, "", "") => "</"+s+">"+"\n"
      case ("meta",true, _, _) => "<"+s+" "+s1+"=\""+s2+"\"/>"+"\n"
      case ("meta",false, _, _) => ""
      case (_,false, _, _) => "</"+s+">"+"\n"
      case (_,true, _, _) => "<"+s+" "+s1+"=\""+s2+"\">"
    }
  }
}