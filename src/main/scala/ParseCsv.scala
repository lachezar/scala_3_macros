import java.time.LocalDate
import scala.deriving.Mirror
import scala.compiletime.*
import scala.util.Try

// exercise based on this article: https://yadukrishnan.live/a-generic-approach-to-parsing-csv-into-case-classes-in-scala-3

object ParseCsv {

  trait CsvParser[T]:
    def parse(value: String): T

  given CsvParser[String] with
    def parse(value: String): String = value.trim

  given CsvParser[Int] with
    def parse(value: String): Int = value.trim.toInt

  given CsvParser[LocalDate] with
    def parse(value: String): LocalDate = LocalDate.parse(value.trim)

  given [T](using parser: CsvParser[T]): CsvParser[Option[T]] with
    def parse(value: String): Option[T] = Option.when(!value.isBlank())(parser.parse(value))

  inline def summonParsers[T <: Tuple]: List[CsvParser[?]] =
    inline erasedValue[T] match
      case _: (t *: ts)  => summonInline[CsvParser[t]] :: summonParsers[ts]
      case _: EmptyTuple => Nil

  inline def fromCsvRow[A](row: Vector[String])(using m: Mirror.ProductOf[A]): Either[String, A] =
    Try {
      val parsers: List[CsvParser[?]] = summonParsers[m.MirroredElemTypes]
      require(
        row.length == parsers.length,
        s"Number of columns in CSV (${row.length}) does not match the number of fields in case class (${parsers.length})"
      )
      val parsedValues: Vector[Any] = row.zip(parsers).map { (v, p) => p.parse(v) }
      val t: m.MirroredElemTypes = Tuple.fromArray(parsedValues.toArray).asInstanceOf[m.MirroredElemTypes]
      m.fromTuple(t)
    }.toEither.left.map(_.getMessage)
}

class GenericCsvParser[T <: Product] {
  extension [A](v: Vector[A])
    def traverse[B](f: A => Either[String, B]): Either[String, Vector[B]] =
      v.foldLeft[Either[String, Vector[B]]](Right(Vector.empty)) { (acc, x) =>
        acc.flatMap(v => f(x).map(v :+ _))
      }

  inline def read(csv: Vector[Vector[String]])(using m: Mirror.ProductOf[T]): Either[String, Vector[T]] =
    csv.traverse(fromCsvRow(_))
  inline def fromCsvRow[A](row: Vector[String])(using m: Mirror.ProductOf[A]): Either[String, A] =
    ParseCsv.fromCsvRow(row)
}

val csv: Vector[Vector[String]] = Vector(
  "foo,12,2022-12-12,TypeA",
  "bar,23,2022-12-13,TypeA",
  "BAZ zzz,99,2022-12-14,TypeB"
  // "err,99,2022-12-xy,TypeB"
  // "enum err,99,2022-12-14,TypeC"
).map(_.split(",").toVector)

final case class Test(str: String, num: Int, date: LocalDate, t: String)

enum Type:
  case TypeA, TypeB

final case class Test2(str: String, num: Int, date: LocalDate, t: Type)

@main
def main = {
  val parser: GenericCsvParser[Test] = GenericCsvParser[Test]
  val csvData: Either[String, Vector[Test]] = parser.read(csv)
  println(csvData)

  val parserWithEnum: GenericCsvParser[Test2] = GenericCsvParser[Test2]
  given ParseCsv.CsvParser[Type] with
    def parse(value: String): Type = Type.valueOf(value)
  val csvDataWithEnum: Either[String, Vector[Test2]] = parserWithEnum.read(csv)
  println(csvDataWithEnum)
}
