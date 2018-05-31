package pw.elka.jps.rbtree

import rbt.RBTree

object Run{

  def main(args : Array[String]) : Unit = {

    val t1 = RBTree.initialize((1, "Piotr"), (2, "Kowalski"), (3, "inne"), (4, "ADS"), (5, "Kolorowa"))
    val t2 = RBTree.createEmpty[Int, String].insert(14, "Miarka").insert(19, "Pokład")

    Console.println(t2.printTree())

    // Usuniecie danych
    val tmp = t1.remove(2)

    Console.println(tmp.printTree())

    // Przygotowanie danych dla późniejszego przecięcia zbiorów.
    val t3 = t2.insert(3, "Jablko").insert(5, "Akwarium")

    // Dodanie dwóch zbiorów.
    val t1t2 = t1+t2;
    Console.println(t1t2.printTree())

    // Przecięcie dwóch zbiorów według klucza.
    val t1t3 = t1.intersection(t3)
    Console.println(t1t3.printTree())
  }
}
