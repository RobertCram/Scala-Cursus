enum Grade:
  case Bad, Mediocre, Inadequate, Passable, Good, VeryGood, Excellent

object Grade:
  /**
   * @return The median grade of a collection of grades.
   *
   * The median grade can be computed by sorting the collection
   * and taking the element in the middle. If there are an even
   * number of grades, any of the two grades that are just before
   * or after the middle of the sequence are correct median values.
   *
   * Grades can be compared by using their `ordinal` method.
   *
   * Hints: use the following operations:
   * - `sortBy` and `ordinal` to sort the collection of grades,
   * - `size` to compute the number of elements,
   * - `apply` to select an element at a specific index.
   */
  def median(grades: Seq[Grade]): Grade =
    grades
      .sortBy(grade => grade.ordinal)
      .apply(grades.size / 2)
end Grade

import Grade._

case class Candidate(name: String)

val gradesPerCandidate = Map(
  Candidate("1") -> List(Mediocre, Inadequate, Good, Bad, VeryGood, Mediocre, Passable, Mediocre, Good, Mediocre, Good, Mediocre, VeryGood, VeryGood, Good, Bad, Inadequate, Passable, Good, Excellent, Inadequate, Inadequate, Inadequate, Passable, Good, Passable, Good, Bad, VeryGood, Good, Inadequate, Good, Inadequate, Good, Mediocre, Excellent, Mediocre, Bad, Bad, Mediocre, Inadequate, Mediocre, VeryGood, Good, Good, Mediocre, Bad, Good, Bad, Excellent), 
  Candidate("3") -> List(Excellent, Excellent, Mediocre, Mediocre, Excellent, Bad, Passable, Inadequate, Passable, Good, VeryGood, Excellent, Good, Good, Excellent, Inadequate, Inadequate, Mediocre, Bad, Inadequate, Inadequate, Excellent, Mediocre, Passable, Passable, Inadequate, Good, Good, VeryGood, Good, Good, Excellent, Good, Excellent, Excellent, Bad, Passable, Inadequate, Inadequate, Bad, Mediocre, Good, Passable, VeryGood, Inadequate, Inadequate, Bad, Inadequate, Inadequate, VeryGood), 
  Candidate("2") -> List(Inadequate, Bad, Passable, Mediocre, Excellent, Passable, Good, Good, VeryGood, Good, Mediocre, Excellent, Excellent, Bad, Excellent, Good, Good, VeryGood, Inadequate, Passable, Inadequate, Bad, Good, Mediocre, Passable, Good, Inadequate, Bad, Excellent, Bad, Inadequate, Bad, Passable, Inadequate, Mediocre, Passable, Good, Passable, Excellent, Inadequate, Good, Bad, Inadequate, VeryGood, Passable, Bad, Mediocre, Mediocre, Good, Mediocre))

val bestMedianGrade =
  gradesPerCandidate
    .values
    .filter(_.nonEmpty)
    .map(Grade.median(_))
    .maxBy(_.ordinal)

val bestCandidates: Map[Candidate, Seq[Grade]] =
  gradesPerCandidate
    .filter((candidate, grades) => median(grades) == bestMedianGrade)

val bestCandidatesMinusOneMedianGrade: Map[Candidate, Seq[Grade]] =
  bestCandidates
    .map((candidate, grades) => candidate -> (grades diff List(bestMedianGrade)))

