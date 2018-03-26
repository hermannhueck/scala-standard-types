package mytypes

import org.scalatest._
import org.scalatest.prop.PropertyChecks

class MaybeSpec extends FlatSpec with Matchers with PropertyChecks {

  "Nothingg.toOption" should "be equal to None" in {
    Nothingg.toOption shouldEqual None
  }

  "Just(value).toOption" should "be equal to Some(value)" in {
    Just(3).toOption shouldEqual Option(3)
    Just('x').toOption shouldEqual Option('x')
    Just("xyz").toOption shouldEqual Option("xyz")

    def test[T]: Option[T] => Assertion = opt =>
      Maybe.fromOption(opt).toOption shouldEqual opt

    forAll(test[Int])
    forAll(test[Char])
    forAll(test[String])
  }

  "fromOption(Option(x, y, z)).toOption" should "be equal to Option(x, y, z)" in {
    import Maybe.fromOption
    fromOption(Option(3)).toOption shouldEqual Option(3)
    fromOption(Option('x')).toOption shouldEqual Option('x')
    fromOption(Option("xyz")).toOption shouldEqual Option("xyz")
  }

  "Maybe.get" should "return the value if any or otherwise throw a NoSuchElementException" in {
    Just(3).get shouldEqual 3
    the[NoSuchElementException] thrownBy Nothingg.get should have message "get value of Nothingg"
    forAll { (mb: Maybe[Int]) =>
      whenever(mb.isDefined) {
        mb.get shouldEqual mb.toOption.get
      }
    }
  }

  "Maybe.getOrElse" should "return the value if any or otherwise return the specified default value" in {
    Just(3).getOrElse(0) shouldEqual 3
    Nothingg.getOrElse(0) shouldEqual 0
  }

  "Maybe.length" should "return the length of the Maybe: 0 or 1" in {
    Nothingg.length shouldEqual 0
    Just(3).length shouldEqual 1
  }

  "Maybe.isDefined" should "return true for a Just or false for a Nothingg" in {
    Nothingg.isDefined shouldEqual false
    Just(3).isDefined shouldEqual true
  }

  "Maybe.notDefined" should "return false for a Just or true for a Nothingg" in {
    Nothingg.notDefined shouldEqual true
    Just(3).notDefined shouldEqual false
  }

  "Maybe.contains" should "return true if the Maybe contains the given element, otherwise false" in {
    Nothingg.contains("fun") shouldEqual false
    Just("Scala").contains("Scala") shouldEqual true
    CList("Scala").contains("Java") shouldEqual false
  }

  "Maybe.exists(predicate)" should "return true if the Maybe is a Just and the predicate is true, otherwise false" in {
    Nothingg.asInstanceOf[Maybe[String]].exists(_.length == 0) shouldEqual false
    Just("Scala").exists(_.length < 5) shouldEqual false
    Just("Scala").exists(_.length == 5) shouldEqual true
    Just("Scala").exists(_.length > 5) shouldEqual false
  }

  "Maybe.forall(predicate)" should "return true if the predicate is true for all elements, otherwise false" in {
    Nothingg.asInstanceOf[Maybe[String]].forall(_.length == -27) shouldEqual true
    Just("Scala").forall(_.length < 5) shouldEqual false
    Just("Scala").forall(_.length == 5) shouldEqual true
    Just("Scala").forall(_.length > 5) shouldEqual false
  }

  "Maybe.filter(predicate)" should "return a Maybe containing all elements for which the given predicate is true" in {
    val even: Int => Boolean = x => x % 2 == 0
    Nothingg.filter(even) shouldEqual Nothingg
    Nothingg.filterNot(even) shouldEqual Nothingg
    Just(3).filter(even) shouldEqual Nothingg
    Just(4).filter(even) shouldEqual Just(4)
    Just(3).filterNot(even) shouldEqual Just(3)
    Just(4).filterNot(even) shouldEqual Nothingg
    forAll { (mb: Maybe[Int]) =>
      mb.filter(even).toOption shouldEqual mb.toOption.filter(even)
      mb.filterNot(even).toOption shouldEqual mb.toOption.filterNot(even)
    }
  }

  "Maybe.map(f)" should "return a Maybe containing the result of processing its value with the function f" in {
    Nothingg.asInstanceOf[Maybe[Int]].map(_ + 1) shouldEqual Nothingg
    Nothingg.asInstanceOf[Maybe[Int]].map(x => x * x) shouldEqual Nothingg
    Nothingg.asInstanceOf[Maybe[String]].map(_.toUpperCase) shouldEqual Nothingg
    Nothingg.asInstanceOf[Maybe[String]].map(_.length) shouldEqual Nothingg
    Nothingg.asInstanceOf[Maybe[String]].map(s => (s, s.length)) shouldEqual Nothingg
    Just(3).map(_ + 1) shouldEqual Just(4)
    Just(3).map(x => x * x) shouldEqual Just(9)
    Just("Scala").map(_.toUpperCase) shouldEqual Just("SCALA")
    Just("Scala").map(_.length) shouldEqual Just(5)
    Just("Scala").map(s => (s, s.length)) shouldEqual Just(("Scala", 5))
    forAll { (mb: Maybe[Int]) =>
      mb.map(_ + 1).toOption shouldEqual mb.toOption.map(_ + 1)
      mb.map(x => x * x).toOption shouldEqual mb.toOption.map(x => x * x)
    }
    forAll { (mb: Maybe[String]) =>
      mb.map(_.toUpperCase).toOption shouldEqual mb.toOption.map(_.toUpperCase)
      mb.map(_.length).toOption shouldEqual mb.toOption.map(_.length)
      mb.map(s => (s, s.length)).toOption shouldEqual mb.toOption.map(s => (s, s.length))
    }
  }

  "Maybe.flatMap(f)" should "return a Maybe containing the flattened result of processing its value with the function f" in {
    Nothingg.asInstanceOf[Maybe[Int]].flatMap(x => Just(x + 1)) shouldEqual Nothingg
    Nothingg.asInstanceOf[Maybe[Int]].flatMap(x => Just(x * x)) shouldEqual Nothingg
    Nothingg.asInstanceOf[Maybe[String]].flatMap(s => Just(s.toUpperCase)) shouldEqual Nothingg
    Nothingg.asInstanceOf[Maybe[String]].flatMap(s => Just(s.length)) shouldEqual Nothingg
    Nothingg.asInstanceOf[Maybe[String]].flatMap(s => Just(s, s.length)) shouldEqual Nothingg
    Just(3).flatMap(x => Just(x + 1)) shouldEqual Just(4)
    Just(3).flatMap(x => Just(x * x)) shouldEqual Just(9)
    Just("Scala").flatMap(s => Just(s.toUpperCase)) shouldEqual Just("SCALA")
    Just("Scala").flatMap(s => Just(s.length)) shouldEqual Just(5)
    Just("Scala").flatMap(s => Just((s, s.length))) shouldEqual Just(("Scala", 5))
    forAll { (mb: Maybe[Int]) =>
      mb.flatMap(x => Just(x + 1)).toOption shouldEqual mb.toOption.flatMap(x => Some(x + 1))
      mb.flatMap(x => Just(x * x)).toOption shouldEqual mb.toOption.flatMap(x => Some(x * x))
    }
    forAll { (mb: Maybe[String]) =>
      mb.flatMap(s => Just(s.toUpperCase)).toOption shouldEqual mb.toOption.flatMap(s => Some(s.toUpperCase))
      mb.flatMap(s => Just(s.length)).toOption shouldEqual mb.toOption.flatMap(s => Some(s.length))
      mb.flatMap(s => Just((s, s.length))).toOption shouldEqual mb.toOption.flatMap(s => Some((s, s.length)))
    }
  }

  "Maybe.flatten" should "flatten a Maybe of Maybes, but fail if the Maybe contains elements other than Maybes" in {
    // flatten() can flatten a Maybe[Maybe[String]] into a Maybe[String]
    Just(Just("Scala")).flatten shouldEqual Just("Scala")
    // flatten() cannot flatten  a Maybe[String]
    the[IllegalStateException] thrownBy Just("Scala").flatten should have message "Maybe cannot be flattened, it is not a Maybe of Maybes."
  }

  "Maybe.foldRight" should "return expected result of the right fold catamorphism" in {
    // for the empty Maybe we ignore f using _, but we must specify the type: (Int, Int) => Int
    Nothingg.foldRight(0)(_: (Int, Int) => Int) shouldEqual 0 // length
    Just(3).foldRight(0)((_, acc) => 1 + acc) shouldEqual 1
    Nothingg.asInstanceOf[Maybe[Int]].foldRight(0)(_ + _) shouldEqual 0 // sum
    Just(3).foldRight(0)(_ + _) shouldEqual 3
    Nothingg.asInstanceOf[Maybe[Int]].foldRight(1)(_ * _) shouldEqual 1 // product
    Just(3).foldRight(1)(_ * _) shouldEqual 3
    Nothingg.asInstanceOf[Maybe[Int]].foldRight((0, 0))((elem, acc) => (elem + acc._1, 1 + acc._2)) shouldEqual(0, 0) // (nearly) avg
    Just(3).foldRight((0, 0))((elem, acc) => (elem + acc._1, 1 + acc._2)) shouldEqual(3, 1)
  }

  "Maybe.foldLeft" should "return expected result of the left fold catamorphism" in {
    // for the empty Maybe we ignore f using _, but we must specify the type: (Int, Int) => Int
    Nothingg.foldLeft(0)(_: (Int, Int) => Int) shouldEqual 0 // length
    Just(3).foldLeft(0)((acc, _) => 1 + acc) shouldEqual 1
    Nothingg.asInstanceOf[Maybe[Int]].foldLeft(0)(_ + _) shouldEqual 0 // sum
    Just(3).foldLeft(0)(_ + _) shouldEqual 3
    Nothingg.asInstanceOf[Maybe[Int]].foldLeft(1)(_ * _) shouldEqual 1 // product
    Just(3).foldLeft(1)(_ * _) shouldEqual 3
    Nothingg.asInstanceOf[Maybe[Int]].foldLeft((0, 0))((acc, elem) => (elem + acc._1, 1 + acc._2)) shouldEqual(0, 0) // (nearly) avg
    Just(3).foldLeft((0, 0))((acc, elem) => (elem + acc._1, 1 + acc._2)) shouldEqual(3, 1)
  }

  "Maybe.fold" should "return expected result of the binary fold operator" in {
    Nothingg.asInstanceOf[Maybe[Int]].fold(0)(1 + _) shouldEqual 0 // length
    Just(3).fold(0)(1 + _) shouldEqual 4
  }

  "Maybe.zipWith(thatMaybe)(f)" should "process corresponding elements in 2 Maybes and return a Maybe containing the result" in {
    val mkTuple: (Int, String) => (Int, String) = (_, _)
    Just(3).zipWith(Just("Scala"))(mkTuple) shouldEqual Just((3, "Scala"))
    Nothingg.zipWith(Just("Scala"))(mkTuple) shouldEqual Nothingg
    Just(3).zipWith(Nothingg)(mkTuple) shouldEqual Nothingg
    Just(3).zipWith(Just(3))(_ + _) shouldEqual Just(6)
    Just(3).zipWith(Just(3))(_ * _) shouldEqual Just(9)
    forAll { (mb1: Maybe[Int], mb2: Maybe[Int]) =>
      mb1.zipWith(mb2)(_ + _).toOption.toList shouldEqual (mb1.toOption, mb2.toOption).zipped.map(_ + _).toList
      mb1.zipWith(mb2)(_ * _).toOption.toList shouldEqual (mb1.toOption, mb2.toOption).zipped.map(_ * _).toList
    }
    forAll { (mb1: Maybe[Int], mb2: Maybe[String]) =>
      mb1.zipWith(mb2)(mkTuple).toOption.toList shouldEqual (mb1.toOption, mb2.toOption).zipped.map(mkTuple).toList
    }
  }

  "Maybe.zip(thatMaybe)" should "return a Maybe of pairs with the elements of this Maybe and that Maybe" in {
    Just(3).zip(Just("Scala")) shouldEqual Just((3, "Scala"))
    Nothingg.zip(Just("Scala")) shouldEqual Nothingg
    Just(3).zip(Nothingg) shouldEqual Nothingg
    forAll { (mb1: Maybe[Int], mb2: Maybe[String]) =>
      mb1.zip(mb2).toOption.toList shouldEqual mb1.toOption.zip(mb2.toOption).toList
    }
  }

  "Maybe.zipWithIndex" should "return a Maybe of a pair with the value of this Maybe and the index 0 if this Maybe is a Just, otherwise Nothingg" in {
    Nothingg.zipWithIndex shouldEqual Nothingg
    Just(3).zipWithIndex shouldEqual Just((3, 0))
    Just("Scala").zipWithIndex shouldEqual Just(("Scala", 0))
    forAll { (mb: Maybe[Int]) =>
      whenever(mb.isDefined) {
        mb.zipWithIndex.toOption.get._2 shouldEqual 0
      }
    }
    forAll { (mb: Maybe[String]) =>
      whenever(mb.isDefined) {
        mb.zipWithIndex.toOption.get._2 shouldEqual 0
      }
    }
  }
}
