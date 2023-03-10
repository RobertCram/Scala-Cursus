import codecs._
import scala.concurrent.{ Future }
import scala.concurrent.ExecutionContext.given

val jsonStr = Util.parseJson(""" "hello world" """)

jsonStr.flatMap(json => json.decodeAs[String])

for{
  json <- jsonStr
  value <- json.decodeAs[String]
} yield value
  

val p1 = Person("Robert", 60)

val obj1 = ObjectEncoder.field[String]("name").encode(p1._1)
Decoder.field[String]("name").decode(obj1)

val obj2 = ObjectEncoder.field[Person]("rc").encode(p1)
 
Person.given_Encoder_Person.encode(p1)

summon[Encoder[Person]].encode(p1)

def encodePerson(p: Person)(using enc: Encoder[Person]) =
  enc.encode(p)

def decodePerson(p: Json)(using dec: Decoder[Person]) =
  dec.decode(p)


encodePerson(p1)

val jsonPerson = Util.parseJson(""" { "name": "Bob", "age": 10 } """)

jsonPerson.flatMap(json => decodePerson(json))

summon[Encoder[List[Person]]].transform[Contacts](contacts => contacts.people).encode(Contacts(List(p1)))


Contacts(List())

val listEncoder1 = summon[Encoder[Contacts]] 
val listDecoder1 = summon[Decoder[Contacts]] 


val jsonContacts = listEncoder1.encode(Contacts(List(p1)))
listDecoder1.decode(jsonContacts)

ObjectEncoder.field[Contacts]("people")
