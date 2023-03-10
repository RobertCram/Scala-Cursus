type KeyMap = Map[Char, Set[Char]] 
val KeyMap = Map

given KeyMap = KeyMap(
  '2' -> Set('a', 'b', 'c'),
  '3' -> Set('d', 'e', 'f'),
  '4' -> Set('g', 'h', 'i'),
  '5' -> Set('j', 'k', 'l'),
  '6' -> Set('m', 'n', 'o'),
  '7' -> Set('p', 'q', 'r', 's'),
  '8' -> Set('t', 'u', 'v'),
  '9' -> Set('w', 'x', 'y', 'z'),
)

type Dictionary = Set[String]
val Dictionary = Set

given Dictionary = Set(
  "very",
  "tepx",
  "epy",
  "u",
  "dry",
  "invalid",
  "scala",
  "rocks",
  "fuck",
)

type Digits = String
type BrokenupPhoneNumber = List[Digits]
type Word = String
type Mnenomic = List[Word]
val Mnenomic = List


def phoneNumberToBrokenupPhoneNumbers(phoneNumber: String)(using keyMap: KeyMap): Set[BrokenupPhoneNumber] = 
  val validPhoneNumber = phoneNumber.filter(c => keyMap.contains(c))
  validPhoneNumber.foldLeft(Set[BrokenupPhoneNumber]())(
    (results, char) => if results.isEmpty then Set(List(char.toString)) else results.map(result => result ++ List(char.toString)) ++ results.map(result => result.dropRight(1) :+ (result.last :+ char))
  )

phoneNumberToBrokenupPhoneNumbers("8379")


def digitsToDictionaryWords(digits: Digits)(using dictionary: Dictionary, keyMap: KeyMap): List[Word] =

  def addCharsToWords(digit: Char, words: List[Word]): List[Word] =
    keyMap(digit).flatMap(c => words.map(word => word + c)).toList; 

  if !dictionary.map(_.length).contains(digits.length) then List()
  else digits.foldLeft(List[Word](""))((acc, digit) => addCharsToWords(digit, acc)).filter(phrase => dictionary.contains(phrase))
   


def brokenupPhoneNumberToMnenomics(digits: List[Digits])(using dictionary: Dictionary): List[Mnenomic] =

  def addWordToSentences(word: String, sentences: List[Mnenomic]): List[Mnenomic] =
    sentences.map(sentence => word :: sentence)

  def addDigitsToSentences(digits: Digits, sentences: List[Mnenomic]): List[Mnenomic] =
      for {
        word <- digitsToDictionaryWords(digits)
        sentence <- addWordToSentences(word, sentences)
      } yield sentence

  digits.foldRight(List(Mnenomic.empty))(addDigitsToSentences)


digitsToDictionaryWords("8379")
digitsToDictionaryWords("8")
digitsToDictionaryWords("379")

digitsToDictionaryWords("72252")
digitsToDictionaryWords("76257")

brokenupPhoneNumberToMnenomics(List("8", "379"))
brokenupPhoneNumberToMnenomics(List("8379"))


def phoneNumberToMnenomics(phoneNumber: String)(using dictionary: Dictionary, keyMap: KeyMap): Set[String] =
  for {
    brokenupPhoneNumber <- phoneNumberToBrokenupPhoneNumbers(phoneNumber)
    mnenomic <- brokenupPhoneNumberToMnenomics(brokenupPhoneNumber)
  } yield mnenomic.toString

"hello world!"    

phoneNumberToMnenomics("8379")
phoneNumberToMnenomics("111382-5722-527-6257")

