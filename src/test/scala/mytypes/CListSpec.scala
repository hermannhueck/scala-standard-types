package mytypes

import org.scalatest._
import org.scalatest.prop.PropertyChecks

class CListSpec extends FlatSpec with Matchers with PropertyChecks {

  "Empty CList.toList" should "be equal to empty List" in {
    CList().toList shouldEqual List()
  }

  "CList(x, y, z).toList" should "be equal to List(x, y, z)" in {
    CList(1, 2, 3).toList shouldEqual List(1, 2, 3)
    forAll { (list: List[Int]) =>
      val clist = CList.fromList(list)
      clist.toList shouldEqual list
    }
    CList('x', 'y', 'z').toList shouldEqual List('x', 'y', 'z')
    forAll { (list: List[Char]) =>
      val clist = CList.fromList(list)
      clist.toList shouldEqual list
    }
    CList("x", "y", "z").toList shouldEqual List("x", "y", "z")
    forAll { (list: List[String]) =>
      val clist = CList.fromList(list)
      clist.toList shouldEqual list
    }
  }

  "CList(x, y, z).toSeq" should "be equal to Seq(x, y, z)" in {
    CList(1, 2, 3).toSeq shouldEqual Seq(1, 2, 3)
    CList('x', 'y', 'z').toSeq shouldEqual Seq('x', 'y', 'z')
    CList("x", "y", "z").toSeq shouldEqual Seq("x", "y", "z")
  }

  "CList(x, y, z).toSet" should "be equal to Set(x, y, z)" in {
    CList(1, 2, 3).toSet.size shouldEqual 3
    CList(1, 2, 3).toSet.contains(1) shouldEqual true
    CList(1, 2, 3).toSet.contains(2) shouldEqual true
    CList(1, 2, 3).toSet.contains(3) shouldEqual true
    CList('x', 'y', 'z').toSet.size shouldEqual 3
    CList('x', 'y', 'z').contains('x') shouldEqual true
    CList('x', 'y', 'z').contains('y') shouldEqual true
    CList('x', 'y', 'z').contains('z') shouldEqual true
    CList("x", "y", "z").toSet.size shouldEqual 3
    CList("x", "y", "z").toSet.contains("x") shouldEqual true
    CList("x", "y", "z").toSet.contains("y") shouldEqual true
    CList("x", "y", "z").toSet.contains("z") shouldEqual true
  }

  "fromList(List(x, y, z)).toList" should "be equal to List(x, y, z)" in {
    import CList.fromList
    fromList(List(1, 2, 3)).toList shouldEqual List(1, 2, 3)
    fromList(List('x', 'y', 'z')).toList shouldEqual List('x', 'y', 'z')
    fromList(List("x", "y", "z")).toList shouldEqual List("x", "y", "z")
  }

  "fromSeq(Seq(x, y, z)).toList" should "be equal to List(x, y, z)" in {
    import CList.fromSeq
    fromSeq(Seq(1, 2, 3)).toList shouldEqual List(1, 2, 3)
    fromSeq(Seq('x', 'y', 'z')).toList shouldEqual List('x', 'y', 'z')
    fromSeq(Seq("x", "y", "z")).toList shouldEqual List("x", "y", "z")
  }

  "fromSet(Set(x, y, z)).toList" should "be equal to List(x, y, z)" in {
    import CList.fromSet
    fromSet(Set(1, 2, 3)).toList.sorted shouldEqual List(1, 2, 3)
    fromSet(Set('x', 'y', 'z')).toList.sorted shouldEqual List('x', 'y', 'z')
    fromSet(Set("x", "y", "z")).toList.sorted shouldEqual List("x", "y", "z")
  }

  "Cons" should "be able to prepend an element" in {
    Cons(15, CList()) shouldEqual CList(15)
    Cons(15, CList(1, 2, 3)) shouldEqual CList(15, 1, 2, 3)
    forAll { (head: Int, tail: CList[Int]) =>
      Cons(head, tail).toList shouldEqual head :: tail.toList
    }
  }

  "CList.head" should "return the 1st element if any or otherwise throw a NoSuchElementException" in {
    CList(1, 2, 3).head shouldEqual 1
    the[NoSuchElementException] thrownBy CList().head should have message "head of empty CList"
    forAll { (cl: CList[Int]) =>
      whenever(cl.nonEmpty) {
        cl.head shouldEqual cl.toList.head
      }
    }
  }

  "CList.tail" should "return a CList containing all elements except the 1st one if any or otherwise throw a NoSuchElementException" in {
    CList(1, 2, 3).tail shouldEqual CList(2, 3)
    the[NoSuchElementException] thrownBy CList().tail should have message "tail of empty CList"
    forAll { (cl: CList[Int]) =>
      whenever(cl.nonEmpty) {
        cl.tail.toList shouldEqual cl.toList.tail
      }
    }
  }

  "CList.headOption" should "return Some(1st element) or None if the CList is empty" in {
    CList(1, 2, 3).headOption shouldEqual Some(1)
    CList().headOption shouldEqual None
  }

  "CList.length" should "return the length of the CList" in {
    CList().length shouldEqual 0
    CList(1, 2, 3).length shouldEqual 3
  }

  "CList.isDefinedAt(index)" should "return true if an element exists at the given index, otherwise false" in {
    CList().isDefinedAt(0) shouldEqual false
    CList(1, 2, 3).isDefinedAt(-1) shouldEqual false
    CList(1, 2, 3).isDefinedAt(0) shouldEqual true
    CList(1, 2, 3).isDefinedAt(1) shouldEqual true
    CList(1, 2, 3).isDefinedAt(2) shouldEqual true
    CList(1, 2, 3).isDefinedAt(3) shouldEqual false
  }

  "CList.contains" should "return true if the CList contains the given element, otherwise false" in {
    CList().contains("fun") shouldEqual false
    CList("Scala", "is", "fun").contains("Scala") shouldEqual true
    CList("Scala", "is", "fun").contains("is") shouldEqual true
    CList("Scala", "is", "fun").contains("fun") shouldEqual true
    CList("Scala", "is", "fun").contains("Java") shouldEqual false
  }

  "CList.exists(predicate)" should "return true if at least one element exists for which the predicate is true, otherwise false" in {
    CList[String]().exists(_.length == 0) shouldEqual false
    CList("Scala", "is", "fun").exists(_.length == 0) shouldEqual false
    CList("Scala", "is", "fun").exists(_.length == 1) shouldEqual false
    CList("Scala", "is", "fun").exists(_.length == 2) shouldEqual true
    CList("Scala", "is", "fun").exists(_.length == 3) shouldEqual true
    CList("Scala", "is", "fun").exists(_.length == 4) shouldEqual false
    CList("Scala", "is", "fun").exists(_.length == 5) shouldEqual true
    CList("Scala", "is", "fun").exists(_.length == 6) shouldEqual false
  }

  "CList.forall(predicate)" should "return true if the predicate is true for all elements, otherwise false" in {
    CList[String]().forall(_.length == -27) shouldEqual true
    CList("Scala", "is", "fun").forall(_.length == 0) shouldEqual false
    CList("Scala", "is", "fun").forall(_.length == 1) shouldEqual false
    CList("Scala", "is", "fun").forall(_.length == 2) shouldEqual false
    CList("Scala", "is", "fun").forall(_.length == 3) shouldEqual false
    CList("Scala", "is", "fun").forall(_.length == 5) shouldEqual false
    CList("Scala", "is", "fun").forall(_.length > 1) shouldEqual true
  }

  "CList.reverse" should "return the CList with the elements in reversed order" in {
    CList[String]().reverse shouldEqual CList[String]()
    CList("Scala", "is", "fun").reverse shouldEqual CList("fun", "is", "Scala")
    CList("Scala", "is", "fun").reverse.reverse shouldEqual CList("Scala", "is", "fun")
    forAll { (cl: CList[Int]) =>
      cl.reverse.toList shouldEqual cl.toList.reverse
      cl.reverse.reverse shouldEqual cl
    }
  }

  "CList.isPalindrom" should "return true if the reversed CList equals the original CList, otherwise false" in {
    CList[String]().isPalindrom shouldEqual true
    CList("live", "is", "live").isPalindrom shouldEqual true
    CList("live", "is", "love").isPalindrom shouldEqual false
    CList(3, 20, 12, 20, 3).isPalindrom shouldEqual true
    CList(3, 20, 12, 20, 4).isPalindrom shouldEqual false
    CList(3, 20, 20, 3).isPalindrom shouldEqual true
    CList(3, 20, 20, 4).isPalindrom shouldEqual false
  }

  "CList.takeWhile(p)" should "return a CList containing the starting elements, as long as the predicate p is true" in {
    CList[Int]().takeWhile(_ < 4) shouldEqual CList()
    CList(1).takeWhile(_ < 4) shouldEqual CList(1)
    CList(1, 2).takeWhile(_ < 4) shouldEqual CList(1, 2)
    CList(1, 2, 3).takeWhile(_ < 4) shouldEqual CList(1, 2, 3)
    CList(1, 2, 3, 4).takeWhile(_ < 4) shouldEqual CList(1, 2, 3)
    CList(1, 2, 3, 4, 5).takeWhile(_ < 4) shouldEqual CList(1, 2, 3)
  }

  "CList.dropWhile(p)" should "return a CList containing the remaining elements, after the predicate p has become false" in {
    CList[Int]().dropWhile(_ < 4) shouldEqual CList()
    CList(1).dropWhile(_ < 4) shouldEqual CList()
    CList(1, 2).dropWhile(_ < 4) shouldEqual CList()
    CList(1, 2, 3).dropWhile(_ < 4) shouldEqual CList()
    CList(1, 2, 3, 4).dropWhile(_ < 4) shouldEqual CList(4)
    CList(1, 2, 3, 4, 5).dropWhile(_ < 4) shouldEqual CList(4, 5)
  }

  "CList.take(n)" should "return a CList containing the 1st n elements, if n <= length else all elements" in {
    CList(1, 2, 3, 4, 5).take(-1) shouldEqual CList()
    CList(1, 2, 3, 4, 5).take(0) shouldEqual CList()
    CList(1, 2, 3, 4, 5).take(3) shouldEqual CList(1, 2, 3)
    CList(1, 2, 3, 4, 5).take(5) shouldEqual CList(1, 2, 3, 4, 5)
    CList(1, 2, 3, 4, 5).take(6) shouldEqual CList(1, 2, 3, 4, 5)
  }

  "CList.drop(n)" should "return a CList containing the remaining elements after the 1st n elements have been chopped of, if n <= length else no elements" in {
    CList(1, 2, 3, 4, 5).drop(-1) shouldEqual CList(1, 2, 3, 4, 5)
    CList(1, 2, 3, 4, 5).drop(0) shouldEqual CList(1, 2, 3, 4, 5)
    CList(1, 2, 3, 4, 5).drop(3) shouldEqual CList(4, 5)
    CList(1, 2, 3, 4, 5).drop(5) shouldEqual CList()
    CList(1, 2, 3, 4, 5).drop(6) shouldEqual CList()
  }

  "CList.filter(predicate)" should "return a CList containing all elements for which the given predicate is true" in {
    val even: Int => Boolean = x => x % 2 == 0
    CList().filter(even) shouldEqual CList()
    CList().filterNot(even) shouldEqual CList()
    CList(1, 2, 3, 4, 5).filter(even) shouldEqual CList(2, 4)
    CList(1, 2, 3, 4, 5).filterNot(even) shouldEqual CList(1, 3, 5)
    forAll { (cl: CList[Int]) =>
      cl.filter(even).toList shouldEqual cl.toList.filter(even)
      cl.filterNot(even).toList shouldEqual cl.toList.filterNot(even)
    }
  }

  "CList.map(f)" should "return a CList containing the results of processing all elements with the function f" in {
    CList().asInstanceOf[CList[Int]].map(_ + 1) shouldEqual CList()
    CList().asInstanceOf[CList[Int]].map(x => x * x) shouldEqual CList()
    CList().asInstanceOf[CList[String]].map(_.toUpperCase) shouldEqual CList()
    CList().asInstanceOf[CList[String]].map(_.length) shouldEqual CList()
    CList().asInstanceOf[CList[String]].map(s => (s, s.length)) shouldEqual CList()
    CList(1, 2, 3).map(_ + 1) shouldEqual CList(2, 3, 4)
    CList(1, 2, 3).map(x => x * x) shouldEqual CList(1, 4, 9)
    CList("Scala", "is", "fun").map(_.toUpperCase) shouldEqual CList("SCALA", "IS", "FUN")
    CList("Scala", "is", "fun").map(_.length) shouldEqual CList(5, 2, 3)
    CList("Scala", "is", "fun").map(s => (s, s.length)) shouldEqual CList(("Scala", 5), ("is", 2), ("fun", 3))
    forAll { (cl: CList[Int]) =>
      cl.map(_ + 1).toList shouldEqual cl.toList.map(_ + 1)
      cl.map(x => x * x).toList shouldEqual cl.toList.map(x => x * x)
    }
    forAll { (cl: CList[String]) =>
      cl.map(_.toUpperCase).toList shouldEqual cl.toList.map(_.toUpperCase)
      cl.map(_.length).toList shouldEqual cl.toList.map(_.length)
      cl.map(s => (s, s.length)).toList shouldEqual cl.toList.map(s => (s, s.length))
    }
  }

  "CList.flatMap(f)" should "return a CList containing the flattened results of processing all elements with the function f" in {
    CList().asInstanceOf[CList[Int]].flatMap(x => CList(x + 1)) shouldEqual CList()
    CList().asInstanceOf[CList[Int]].flatMap(x => CList(x * x)) shouldEqual CList()
    CList().asInstanceOf[CList[String]].flatMap(s => CList(s.toUpperCase)) shouldEqual CList()
    CList().asInstanceOf[CList[String]].flatMap(s => CList(s.length)) shouldEqual CList()
    CList().asInstanceOf[CList[String]].flatMap(s => CList(s, s.length)) shouldEqual CList()
    CList(1, 2, 3).flatMap(x => CList(x + 1)) shouldEqual CList(2, 3, 4)
    CList(1, 2, 3).flatMap(x => CList(x * x)) shouldEqual CList(1, 4, 9)
    CList("Scala", "is", "fun").flatMap(s => CList(s.toUpperCase)) shouldEqual CList("SCALA", "IS", "FUN")
    CList("Scala", "is", "fun").flatMap(s => CList(s.length)) shouldEqual CList(5, 2, 3)
    CList("Scala", "is", "fun").flatMap(s => CList((s, s.length))) shouldEqual CList(("Scala", 5), ("is", 2), ("fun", 3))
    forAll { (cl: CList[Int]) =>
      cl.flatMap(x => CList(x + 1)).toList shouldEqual cl.toList.flatMap(x => List(x + 1))
      cl.flatMap(x => CList(x * x)).toList shouldEqual cl.toList.flatMap(x => List(x * x))
    }
    forAll { (cl: CList[String]) =>
      cl.flatMap(s => CList(s.toUpperCase)).toList shouldEqual cl.toList.flatMap(s => List(s.toUpperCase))
      cl.flatMap(s => CList(s.length)).toList shouldEqual cl.toList.flatMap(s => List(s.length))
      cl.flatMap(s => CList((s, s.length))).toList shouldEqual cl.toList.flatMap(s => List((s, s.length)))
    }
  }

  "CList.flattenOld" should "flatten a CList of CLists, but fail if the CList contains elements other than CLists" in {
    // flatten1() can flatten a CList[CList[String]] into a CList[String]
    CList(CList("Scala"), CList("is"), CList("fun")).flattenOld shouldEqual CList("Scala", "is", "fun")
    // flatten1() cannot flatten  a CList[String]
    the[IllegalStateException] thrownBy CList("Scala", "is", "fun").flattenOld should have message "CList cannot be flattened, it is not a CList of CLists."
  }

  "CList.flatten" should "flatten a CList of CLists, but fail if the CList contains elements other than CLists" in {
    // flatten() can flatten a CList[CList[ELEM]] into a CList[ELEM]
    CList(CList("Scala"), CList("is"), CList("fun")).flatten shouldEqual CList("Scala", "is", "fun")
    CList(CList(1, 2), CList(3), CList(4, 5, 6)).flatten shouldEqual CList(1, 2, 3, 4, 5, 6)
  }

  "CList.concatOld" should "concatenate a List of CLists into one CList, but fail if the CList contains elements other than CLists" in {
    val clistOfCLists = CList(CList(1, 2, 3), CList(4, 5), CList(11, 12, 13))
    clistOfCLists.concatOld shouldEqual CList(1, 2, 3, 4, 5, 11, 12, 13)
    CList(CList("Scala", "is"), CList("great", "fun"), CList(".")).concatOld shouldEqual CList("Scala", "is", "great", "fun", ".")
    the[IllegalStateException] thrownBy CList("Scala", "is", "fun").concatOld should have message "CList cannot be flattened, it is not a CList of CLists."
  }

  "CList.concat" should "concatenate a List of CLists into one CList, but fail if the CList contains elements other than CLists" in {
    val clistOfCLists = CList(CList(1, 2, 3), CList(4, 5), CList(11, 12, 13))
    clistOfCLists.concat shouldEqual CList(1, 2, 3, 4, 5, 11, 12, 13)
    CList(CList("Scala", "is"), CList("great", "fun"), CList(".")).concat shouldEqual CList("Scala", "is", "great", "fun", ".")
  }

  "CList.foldRight" should "return the expected result of the right fold catamorphism" in {
    // for the empty CList we ignore f using _, but we must specify the type: (Int, Int) => Int
    CList().foldRight(0)(_: (Int, Int) => Int) shouldEqual 0 // length
    CList(1, 2, 3, 4, 5).foldRight(0)((_, acc) => 1 + acc) shouldEqual 5
    CList[Int]().foldRight(0)(_ + _) shouldEqual 0 // sum
    CList(1, 2, 3, 4, 5).foldRight(0)(_ + _) shouldEqual 15
    CList[Int]().foldRight(1)(_ * _) shouldEqual 1 // product
    CList(1, 2, 3, 4, 5).foldRight(1)(_ * _) shouldEqual 120
    CList[Int]().foldRight((0, 0))((elem, acc) => (elem + acc._1, 1 + acc._2)) shouldEqual(0, 0) // (nearly) avg
    CList(1, 2, 3, 4, 5).foldRight((0, 0))((elem, acc) => (elem + acc._1, 1 + acc._2)) shouldEqual(15, 5)
  }

  "CList.foldLeft" should "return the expected result of the left fold catamorphism" in {
    // for the empty CList we ignore f using _, but we must specify the type: (Int, Int) => Int
    CList().foldLeft(0)(_: (Int, Int) => Int) shouldEqual 0 // length
    CList(1, 2, 3, 4, 5).foldLeft(0)((acc, _) => 1 + acc) shouldEqual 5
    CList[Int]().foldLeft(0)(_ + _) shouldEqual 0 // sum
    CList(1, 2, 3, 4, 5).foldLeft(0)(_ + _) shouldEqual 15
    CList[Int]().foldLeft(1)(_ * _) shouldEqual 1 // product
    CList(1, 2, 3, 4, 5).foldLeft(1)(_ * _) shouldEqual 120
    CList[Int]().foldLeft((0, 0))((acc, elem) => (elem + acc._1, 1 + acc._2)) shouldEqual(0, 0) // (nearly) avg
    CList(1, 2, 3, 4, 5).foldLeft((0, 0))((acc, elem) => (elem + acc._1, 1 + acc._2)) shouldEqual(15, 5)
  }

  "CList.fold" should "return the expected result of the binary fold operator" in {
    CList().fold(0)((_, acc) => 1 + acc) shouldEqual 0 // length
    CList(1, 2, 3, 4, 5).fold(0)((_, acc) => 1 + acc) shouldEqual 5
    CList[Int]().fold(0)(_ + _) shouldEqual 0 // sum
    CList(1, 2, 3, 4, 5).fold(0)(_ + _) shouldEqual 15
    CList[Int]().fold(1)(_ * _) shouldEqual 1 // product
    CList(1, 2, 3, 4, 5).fold(1)(_ * _) shouldEqual 120
  }

  "CList.reduceRightOption" should "return the expected result of right reducing a non-empty CList as an Option" in {
    CList[Int]().reduceRightOption(_: (Int, Int) => Int) shouldEqual None
    CList(1, 2, 3, 4, 5).reduceRightOption((_, acc) => 1 + acc) shouldEqual Some(5)
    CList(1, 2, 3, 4, 5).reduceRightOption(_ + _) shouldEqual Some(15)
    CList(1, 2, 3, 4, 5).reduceRightOption(_ * _) shouldEqual Some(120)
  }

  "CList.reduceRight" should "return the expected result of right reducing a non-empty CList" in {
    the[UnsupportedOperationException] thrownBy CList[Int]().reduceRight(_: (Int, Int) => Int) should have message "operation not supported on empty CList"
    CList(1, 2, 3, 4, 5).reduceRight((_, acc) => 1 + acc) shouldEqual 5 // length
    CList(1, 2, 3, 4, 5).reduceRight(_ + _) shouldEqual 15 // sum
    CList(1, 2, 3, 4, 5).reduceRight(_ * _) shouldEqual 120 // product
  }

  "CList.reduceLeftOption" should "return the expected result of left reducing a non-empty CList as an Option" in {
    CList[Int]().reduceLeftOption(_: (Int, Int) => Int) shouldEqual None
    CList(1, 2, 3, 4, 5).reduceLeftOption((acc, _) => 1 + acc) shouldEqual Some(5)
    CList(1, 2, 3, 4, 5).reduceLeftOption(_ + _) shouldEqual Some(15)
    CList(1, 2, 3, 4, 5).reduceLeftOption(_ * _) shouldEqual Some(120)
  }

  "CList.reduceLeft" should "return the expected result of left reducing a non-empty CList" in {
    the[UnsupportedOperationException] thrownBy CList[Int]().reduceLeft(_: (Int, Int) => Int) should have message "operation not supported on empty CList"
    CList(1, 2, 3, 4, 5).reduceLeft((acc, _) => 1 + acc) shouldEqual 5 // length
    CList(1, 2, 3, 4, 5).reduceLeft(_ + _) shouldEqual 15 // sum
    CList(1, 2, 3, 4, 5).reduceLeft(_ * _) shouldEqual 120 // product
  }

  "CList.reduce" should "return the expected result of reducing a non-empty CList" in {
    the[UnsupportedOperationException] thrownBy CList[Int]().reduce(_: (Int, Int) => Int) should have message "operation not supported on empty CList"
    CList(1, 2, 3, 4, 5).reduce((_, acc) => 1 + acc) shouldEqual 5 // length
    CList(1, 2, 3, 4, 5).reduce(_ + _) shouldEqual 15 // sum
    CList(1, 2, 3, 4, 5).reduce(_ * _) shouldEqual 120 // product
  }

  "CList.append (++, mappend, <>)" should "concatenate two CLists into one" in {
    CList("Scala", "is") append CList("great", "fun") shouldEqual CList("Scala", "is", "great", "fun")
    CList("Scala", "is") ++ CList("great", "fun") shouldEqual CList("Scala", "is", "great", "fun")
    CList(1, 2, 3) append CList(4, 5) shouldEqual CList(1, 2, 3, 4, 5)
    CList(1, 2, 3) ++ CList(4, 5) shouldEqual CList(1, 2, 3, 4, 5)
  }

  "CList.zipWith(thatCList)(f)" should "process corresponding elements in 2 CLists and return a CList of results" in {
    val mkTuple: (Int, String) => (Int, String) = (_, _)
    CList(1, 2, 3).zipWith(CList("Scala", "is", "fun"))(mkTuple) shouldEqual CList((1, "Scala"), (2, "is"), (3, "fun"))
    CList(1, 2, 3, 4).zipWith(CList("Scala", "is", "fun"))(mkTuple) shouldEqual CList((1, "Scala"), (2, "is"), (3, "fun"))
    CList(1, 2, 3).zipWith(CList("Scala", "is", "great", "fun"))(mkTuple) shouldEqual CList((1, "Scala"), (2, "is"), (3, "great"))
    CList().zipWith(CList("Scala", "is", "great", "fun"))(mkTuple) shouldEqual CList()
    CList(1, 2, 3, 4).zipWith(CList())(mkTuple) shouldEqual CList()
    CList(1, 2, 3, 4).zipWith(CList(1, 2, 3, 4))(_ + _) shouldEqual CList(2, 4, 6, 8)
    CList(1, 2, 3, 4).zipWith(CList(1, 2, 3, 4))(_ * _) shouldEqual CList(1, 4, 9, 16)
    forAll { (cl1: CList[Int], cl2: CList[Int]) =>
      cl1.zipWith(cl2)(_ + _).toList shouldEqual (cl1.toList, cl2.toList).zipped.map(_ + _)
      cl1.zipWith(cl2)(_ * _).toList shouldEqual (cl1.toList, cl2.toList).zipped.map(_ * _)
    }
    forAll { (cl1: CList[Int], cl2: CList[String]) =>
      cl1.zipWith(cl2)(mkTuple).toList shouldEqual (cl1.toList, cl2.toList).zipped.map(mkTuple)
    }
  }

  "CList.zip(thatCList)" should "return a CList of pairs with the elements of this CList and that CList" in {
    CList(1, 2, 3).zip(CList("Scala", "is", "fun")) shouldEqual CList((1, "Scala"), (2, "is"), (3, "fun"))
    CList(1, 2, 3, 4).zip(CList("Scala", "is", "fun")) shouldEqual CList((1, "Scala"), (2, "is"), (3, "fun"))
    CList(1, 2, 3).zip(CList("Scala", "is", "great", "fun")) shouldEqual CList((1, "Scala"), (2, "is"), (3, "great"))
    CList().zip(CList("Scala", "is", "great", "fun")) shouldEqual CList()
    CList(1, 2, 3, 4).zip(CList()) shouldEqual CList()
    forAll { (cl1: CList[Int], cl2: CList[String]) =>
      cl1.zip(cl2).toList shouldEqual cl1.toList.zip(cl2.toList)
    }
  }

  "CList.zipWithIndex" should "return a CList of pairs with the elements of this CList and the indeces of the elements" in {
    CList().zipWithIndex shouldEqual CList()
    CList(1, 2, 3).zipWithIndex shouldEqual CList((1, 0), (2, 1), (3, 2))
    CList("Scala", "is", "fun").zipWithIndex shouldEqual CList(("Scala", 0), ("is", 1), ("fun", 2))
    forAll { (cl: CList[Int]) =>
      whenever(cl.nonEmpty) {
        val withIndices = cl.zipWithIndex.toList
        withIndices.head._2 shouldEqual 0
        withIndices.last._2 shouldEqual cl.length - 1
      }
    }
    forAll { (cl: CList[String]) =>
      whenever(cl.nonEmpty) {
        val withIndices = cl.zipWithIndex.toList
        withIndices.head._2 shouldEqual 0
        withIndices.last._2 shouldEqual cl.length - 1
      }
    }
  }

  "CList.sum" should "return the sum of all elements of a CList of numeric values" in {
    CList(1, 2, 3, 4, 5).sum shouldEqual 15
    CList(1.01, 2.02, 3.03, 4.04, 5.05).sum shouldEqual 15.15 +- 0.000001
  }

  "CList.avg" should "return the average of all elements of a CList of numeric values" in {
    CList(1, 2, 3, 4, 5).avg shouldEqual 3.0
    CList(1.01, 2.02, 3.03, 4.04, 5.05).avg shouldEqual 3.03 +- 0.000001
  }

  "CList.min" should "return the minimum of all elements of a CList of ordered values" in {
    CList(1, 2, 3, 4, 5).min shouldEqual 1
    CList(4, 2, 3, 1, 5).min shouldEqual 1
    CList(5, 2, 3, 4, 1).min shouldEqual 1
    CList(3, 2, 1, 4, 5).min shouldEqual 1
    CList(1.01, 2.02, 3.03, 4.04, 5.05).min shouldEqual 1.01
    CList("scala", "is", "a", "powerful", "language").min shouldEqual "a"
  }

  "CList.max" should "return the maximum of all elements of a CList of ordered values" in {
    CList(1, 2, 3, 4, 5).max shouldEqual 5
    CList(4, 2, 3, 1, 5).max shouldEqual 5
    CList(5, 2, 3, 4, 1).max shouldEqual 5
    CList(3, 2, 1, 4, 5).max shouldEqual 5
    CList(1.01, 2.02, 3.03, 4.04, 5.05).max shouldEqual 5.05
    CList("scala", "is", "a", "powerful", "language").max shouldEqual "scala"
  }
}
