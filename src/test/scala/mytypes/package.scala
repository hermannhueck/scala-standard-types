import org.scalacheck.Arbitrary
import org.scalacheck.Arbitrary.arbitrary

package object mytypes {

  implicit def arbitraryCList[T](implicit at: Arbitrary[T]): Arbitrary[CList[T]] = Arbitrary {
    arbitrary[List[T]].map(list => CList.fromList(list))
  }

  implicit def arbitraryMaybe[T](implicit at: Arbitrary[T]): Arbitrary[Maybe[T]] = Arbitrary {
    arbitrary[Option[T]].map(Maybe.fromOption)
    // Gen.oneOf(Gen.const(Nothingg), arbitrary[T].map(Just(_))) // alternative impl
  }
}
