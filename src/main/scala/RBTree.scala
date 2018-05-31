package pw.elka.jps.rbtree
package rbt

/**
  * Abstrakcja klasy koloru dla kolorów zkonkretyzowanych.
  */
abstract sealed class Color

/**
  * Klasa dla koloru czerwonego.
  */
case object Red extends Color

/**
  * Klasa dla koloru czarnego.
  */
case object Black extends Color

/** Reprezentacja drzewa czerwono-czarnego oraz węzła tego drzewa. Klasa ta pełni rolę
  * klasy abstrakcyjnej (interfejsu) dla klasy węzła (Node) oraz liścia (Leaf) drzewa.
  *
  * @param cmp jest parametrem typu implicit wymuszającym na typie K spełnienie funkcjonalności Ordered.
  * @tparam K typ klucza według którego porządkowane są węzły.
  * @tparam V typ wartości przechowywanej w węźle.
  */
abstract class RBTree[K, V] (implicit cmp : K => Ordered[K]) {

  /** Przekształca do napisu dany węzeł drzewa oraz rekursywnie jego lewe i prawe poddrzewo.
    * Wewnętrzna metoda pomocnicza klasy.
    *
    * @param node węzeł początkowy poddrzewa które jest przekształcone do napisu (String)
    * @param ident wcięcie formatujące dany poziom drzewa wewnątrz napisu dla wizualizacji poziomów drzewa
    * @return napis typu String zawierający opis i strukturę wejściowego poddrzewa
    */
  private def printTree (node : RBTree[K, V], ident : String = "") : String = {
    node match{
      case Node(c, l, k, v, r) => {
        ident + "(" + c + ")" + v + "\n" +
        ident + printTree(l, ident + "  ") +
        ident + printTree(r, ident + "  ")
      }
      case Leaf() =>
        ident + "Leaf" + "\n"
    }
  }

  /** Przekształca do napisu dany węzeł drzewa oraz rekursywnie jego lewe i prawe poddrzewo.
    * Wewnętrzna metoda pomocnicza klasy.
    *
    * @return napis typu String zawierający opis (wartości i kolor węzłów) i strukturę drzewa.
    */
  def printTree() : String = {
    printTree(this, "")
  }

  /** Zwraca wejściowy węzeł ze zmienionym kolorem na czarny.
    *
    * @param node węzeł wejściowy do zmiany koloru na czarny
    * @return kopia węzła wejściowego ze zmienionym kolorem na kolor czarny
    */
  private def turnBlack (node : RBTree[K,V])  : RBTree[K,V] = {
    node match {
      case Leaf() => node
      case Node(_,l,k,v,r) => Node(Black,l,k,v,r)
    }
  }

  /** Równoważy dane poddrzewo poczynając od węzła podanego przez argumenty - wykonuje rotację i
    * przywraca kolory do porządku czerwono-czarnego.
    *
    * @param c kolor węzła początkowego.
    * @param l węzeł z lewej strony węzła początkowego.
    * @param k wartość klucza w wężle.
    * @param v wartość w wężle.
    * @param r węzeł z prawej strony węzła początkowego.
    * @return węzeł będący początkiem poddrzewa po przywróceniu właściwości czerwono-czarnych (zrównoważeniu).
    */
  protected def balance (c : Color, l : RBTree[K,V], k : K, v : Option[V], r : RBTree[K,V]) : RBTree[K,V] = {
    (c,l,k,v,r) match {
      case (Black,Node(Red,Node(Red,a,k1,v1,b),k2,v2,c),k3,v3,d)
      => Node(Red,Node(Black,a,k1,v1,b),k2,v2,Node(Black,c,k3,v3,d))  // pojedyncza rotacja w prawo
      case (Black,a,k1,v1,Node(Red,b,k2,v2,Node(Red,c,k3,v3,d)))
      => Node(Red,Node(Black,a,k1,v1,b),k2,v2,Node(Black,c,k3,v3,d))  // pojedyncza rotacja w lewo
      case (Black,Node(Red,a,k1,v1,Node(Red,b,k2,v2,c)),k3,v3,d)
      => Node(Red,Node(Black,a,k1,v1,b),k2,v2,Node(Black,c,k3,v3,d))  // podwojna rotacja w prawo
      case (Black,a,k1,v1,Node(Red,Node(Red,b,k2,v2,c),k3,v3,d))
      => Node(Red,Node(Black,a,k1,v1,b),k2,v2,Node(Black,c,k3,v3,d))  // podwojna rotacja w lewo
      case (c,a,k,v,b) => Node(c,a,k,v,b)
    }
  }

  /** Pomocnicza metoda do modyfikacji węzłów używana np. do wstawiania, modyfikacji i usuwania.
    *
    * @param k klucz węzła który chcemy zmodyfikować.
    * @param fun funkcja modyfikująca wartość w węźle przyjmująca klucz i obiekt Option
    *          dla wartości i zwracająca Option dla wartości.
    * @return zmodyfikowany węzeł
    */
  private[rbt] def modifyNode (k : K, fun : (K, Option[V]) => Option[V]) : RBTree[K,V]

  /** Znajduje obiekt Option z wartością dla podanego klucza.
    *
    * @param k klucz węzła, z którego chcemy pobrać wartość.
    * @return obiekt Option dla wartości w wężle
    */
  def get(k : K) : Option[V]

  /** Funkcja pomocnicza konwertująca typ Option na typ Boolean.
    *
    * @param optV obiekt Option do konwersji.
    * @tparam T typ zawarty w Option.
    * @return true jeśli obiekt Option jest Some, false jeśli jest None.
    */
  implicit private def valueOptionToBool[T](optV : Option[T]) : Boolean = {
    optV match{
      case Some(v)  => true
      case None     => false
    }
  }

  /** Sprawdza czy podany klucz jest zwarty w drzewie.
    *
    * @param key poszukiwany w drzewie klucz.
    * @return true jeżeli klucz istnieje w drzewie, false w przeciwnym przypadku.
    */
  def exists(key : K) : Boolean = {
    get(key)
  }

  /** Szuka podanej wartości w drzewie.
    *
    * @param v wartość poszukiwana.
    * @return obiekt Some dla węzła, w którym została znaleziona wartość lub None jeśli jej nie znaleziono.
    */
  def find(v : V) : Option[RBTree[K, V]]

  /** Wstawia do drzewa lub aktualizuje wartość dla podanego klucza.
    *
    * @param k klucz, dla którego dodajemy/aktualizujemy wartość.
    * @param v wartość do wstawienia/zaktualizowania.
    * @return zmodyfikowany węzeł.
    */
  def insert (k : K, v : V) = turnBlack(modifyNode(k, (_,_) => Some(v)))

  /** Usuwa węzeł o podanym kluczu z drzewa zmieniając jego wartość na None.
    *
    * @param k klucz, którego węzeł ma być usunięty.
    * @return węzeł usunięty z drzewa
    */
  def remove (k : K) = turnBlack(modifyNode(k, (_,_) => None))

  /** Dodaje podany zbiór do tego zbioru.
    *
    * @param rBTree zbiór (drzewo) do dodania.
    * @return zbiór (drzewo) wynikowy.
    */
  def +(rBTree: RBTree[K, V]) : RBTree[K, V] = {
    rBTree match{
      case Node(c, l, k, Some(v), r) => {
        val tmpTree = this+(r)+(l);
        tmpTree.insert(k, v);
      }
      case Leaf() => this
    }
  }

  /** Pomocnicza funkcja rekursywna do akumulacji pośrednich wyników przecięcia dwóch zbiorów.
    *
    * @param remaining zbiór (drzewo) do dopasowania (znalezienia części wspólnej) z tym (this) zbiorem.
    * @param accumulated zbiór z dotychczasowymi elementami należącymi do przecięcia. Do dalszego rozszerzenia.
    * @return Część zbioru będąca przecięciem tego zbioru (this) ze zbiorem remaining.
    */
  private def intersectionIn(remaining: RBTree[K, V], accumulated: RBTree[K, V]): RBTree[K, V] = {
    remaining match{
      case Node(c, l, k, v, r) => {
        val tmpAcc =
        this.get(k) match{
          case Some(value) => accumulated.insert(k, value)
          case None     => accumulated
        }
        intersectionIn(r, intersectionIn(l, tmpAcc))
      }
      case Leaf() => accumulated
    }
  }

  /** Zwraca zbiór będący przecięciem według klucza tego (this) zbioru i zbioru podanego jako argument.
    *
    * @param rBTree zbiór, którego przecięcie z tym zbiorem (this) jest poszukiwane.
    * @return zbiór (drzewo) będący przecięciem tego zbioru (this) ze zbiorem rBTree).
    */
  def intersection(rBTree: RBTree[K, V]) : RBTree[K, V] = {
    val accumulated : RBTree[K, V] = Leaf()
    intersectionIn(rBTree, accumulated)
  }
}



/** Reprezentuje niepusty węzeł drzewa. Zawiera klucz, wartość, kolor oraz prawy i lewy węzeł.
  *
  * @param c kolor węzła (Red lub Black).
  * @param l lewy węzeł drzewa (dziecko - lewe poddrzewo).
  * @param k klucz w węźle.
  * @param v wartość w węźle.
  * @param r prawy węzeł drzewa (dziecko - prawe poddrzewo).
  * @param cmp jest parametrem typu implicit wymuszającym na typie K spełnienie funkcjonalności Ordered.
  * @tparam K typ klucza według którego porządkowane są węzły.
  * @tparam V typ wartości przechowywanej w węźle.
  */
private case class Node[K, V](c : Color, l : RBTree[K,V], k : K, v : Option[V], r : RBTree[K,V])
                             (implicit cmp : K => Ordered[K]) extends RBTree[K,V] {
  /** Znajduje obiekt Option z wartością dla podanego klucza.
    * Zwraca obiekt Some z wartością (get() dla niepustego węzła).
    *
    * @param k klucz węzła, z którego chcemy pobrać wartość.
    * @return obiekt Option dla wartości w wężle.
    */
  def get(k : K) : Option[V] = {
    if (k < this.k) l.get(k)
    else if (k > this.k) r.get(k)
    else
      v
  }

  /** Szuka podanej wartości w poddrzewie zaczynającym się od tego węzła.
    * Zwraca Some dla wartości w węźle lub None gdy nie znaleziono wartości w drzewie.
    *
    * @param v wartość poszukiwana.
    * @return obiekt Some dla węzła, w którym została znaleziona wartość lub None jeśli jej nie znaleziono.
    */
  def find(v : V) : Option[RBTree[K, V]] = {
    if(Option[V](v) == this.v) Option(this)
    else {
      val lFind = l.find(v)
      val rFind = r.find(v)
      if(lFind != None) lFind else rFind
    }
  }

  /** Pomocnicza metoda do modyfikacji węzłów używana np. do wstawiania, modyfikacji i usuwania.
    * Przeszukuje drzewo w poszukiwaniu węzła o danym kluczu. Gdy znajdzie szukany węzeł wykonuje na nim
    * podaną funkcję.
    *
    * @param k klucz węzła który chcemy zmodyfikować.
    * @param fun funkcja modyfikująca wartość w węźle przyjmująca klucz i obiekt Option
    *          dla wartości i zwracająca Option dla wartości.
    * @return zmodyfikowany węzeł
    */
  private[rbt] def modifyNode (k : K, fun : (K, Option[V]) => Option[V]) : RBTree[K,V] = {
    if (k <  this.k) (balance (c, l.modifyNode(k,fun), this.k, this.v, r))
    else if (k == this.k) (Node(c,l,k,fun(this.k,this.v),r))
    else (balance (c, l, this.k, this.v, r.modifyNode(k,fun)))
  }
}


/** Reprezentuje pusty węzeł drzewa.
  *
  * @param cmp jest parametrem typu implicit wymuszającym na typie K spełnienie funkcjonalności Ordered.
  * @tparam K typ klucza według którego porządkowane są węzły.
  * @tparam V typ wartości przechowywanej w węźle.
  */
private case class Leaf[K, V] (implicit cmp : K => Ordered[K]) extends RBTree[K,V]  {
  /** Znajduje obiekt Option z wartością dla podanego klucza. Zwraca obiekt None (get() dla liścia).
    *
    * @param k klucz węzła, z którego chcemy pobrać wartość.
    * @return obiekt None.
    */
  def get(k : K) : Option[V] = None

  /** Szuka podanej wartości w poddrzewie zaczynającym się od tego węzła. Zwraca None (find() dla liścia).
    *
    * @param v wartość poszukiwana.
    * @return obiekt None.
    */
  def find(v : V) : Option[RBTree[K, V]] = None

  /** Pomocnicza metoda do modyfikacji węzłów używana np. do wstawiania, modyfikacji i usuwania.
    *
    * @param k klucz węzła który chcemy zmodyfikować.
    * @param fun funkcja modyfikująca wartość w węźle przyjmująca klucz i obiekt Option
    *          dla wartości i zwracająca Option dla wartości.
    * @return zmodyfikowany węzeł
    */
  private[rbt] def modifyNode (k : K, fun : (K, Option[V]) => Option[V]) : RBTree[K,V] = {
    Node(Red, this, k, fun(k,None), this)
  }
}


/** Fabryka instancji typu [[RBTree]]. */
object RBTree {

  /** Tworzy pusty zbiór (drzewo) o jednym liściu.
    *
    * @tparam K typ klucza według którego porządkowane są węzły.
    * @tparam V typ wartości przechowywanej w węźle.
    * @return nowo utworzona instancja drzewa (węzeł korzenia).
    */
  def createEmpty[K, V](implicit cmp : K => Ordered[K]) : RBTree[K,V] = Leaf()((k : K) => k)

  /** Tworzy nowe drzewo z podanej listy par (klucz, wartość).
    *
    * @param args sekwencja par (krotek) o strukturze (klucz, wartość)
    * @param cmp jest parametrem typu implicit wymuszającym na typie K spełnienie funkcjonalności Ordered.
    * @tparam K typ klucza według którego porządkowane są węzły.
    * @tparam V typ wartości przechowywanej w węźle.
    * @return nowo utworzona instancja drzewa (węzeł korzenia).
    */
  def initialize[K, V](args : (K,V)*)(implicit cmp : K => Ordered[K]) : RBTree[K,V] = {
    var tmpTree : RBTree[K,V] = Leaf()
    for ((k,v) <- args) {
      tmpTree = tmpTree.insert(k,v)
    }
    tmpTree
  }

  /** Tworzy nowe drzewo z podanej kolekcji iterowalnej zawierającej krotki (klucz, wartość).
    *
    * @param list iterowalna kolekcja par (krotek) o strukturze (klucz, wartość)
    * @param cmp jest parametrem typu implicit wymuszającym na typie K spełnienie funkcjonalności Ordered.
    * @tparam K typ klucza według którego porządkowane są węzły.
    * @tparam V typ wartości przechowywanej w węźle.
    * @return nowo utworzona instancja drzewa (węzeł korzenia).
    */
  def initialize[K, V](list : Iterable[(K, V)])
                      (implicit cmp : K => Ordered[K]) : RBTree[K, V] = {
    var tmpTree : RBTree[K,V] = Leaf()
    for ((k,v) <- list) {
      tmpTree = tmpTree.insert(k,v)
    }
    tmpTree
  }
}