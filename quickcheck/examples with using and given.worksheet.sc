// sort a list of addresses by zipcode and then by streetname

final case class Address(zipcode: Int, street: String)

val addresses = List(
    Address(2, "z"),
    Address(1, "a"),
    Address(2, "a"),
    Address(1, "c"),
    Address(1, "b"),
)


given reverseAddressOrdering: Ordering[Address] with {
    def compare(a1: Address, a2: Address) = -1 * (if a1.zipcode < a2.zipcode then -1
      else if a2.zipcode < a1.zipcode then +1
      else if a1.street < a2.street then -1
      else if a2.street < a1.street then +1
      else 0)
}

given reverseTupleOrdering[A, B](using orda: Ordering[A], ordb: Ordering[B]): Ordering[(A, B)] with {
    def compare(t1: (A, B), t2: (A, B)) = 
        val c = orda.compare(t1._1, t2._1)
        -1 * (if c != 0 then c else ordb.compare(t1._2, t2._2))
}


println(addresses.sorted)

println(addresses.map(address => (address.zipcode, address.street)).sorted)