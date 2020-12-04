package exercises

import util.DayN
import util.StringUtils._

object Day4 extends DayN {

  override val num = 4

  trait FromString[A] {
    def parse(s: String): Either[String, A]
  }

  object FromString {
    def apply[A](f: String => Either[String, A]): FromString[A] = { s => f(s) }
  }

  sealed trait PassportField

  case class BirthYear(value: Int) extends PassportField
  object BirthYear {
    implicit lazy val fromString: FromString[BirthYear] = FromString { s =>
      val year = s.toInt
      if ((year >= 1920) && (year <= 2002)) Right(BirthYear(year))
      else Left("BirthYear not within 1920 - 2002")
    }
  }

  case class IssueYear(value: Int) extends PassportField
  object IssueYear {
    implicit lazy val fromString: FromString[IssueYear] = FromString { s =>
      val year = s.toInt
      if ((year >= 2010) && (year <= 2020)) Right(IssueYear(year))
      else Left("IssueYear not within 2010 - 2020")
    }
  }

  case class ExpirationYear(value: Int) extends PassportField
  object ExpirationYear {
    implicit lazy val fromString: FromString[ExpirationYear] = FromString { s =>
      val year = s.toInt
      if ((year >= 2020) && (year <= 2030)) Right(ExpirationYear(year))
      else Left("ExpirationYear not within 2020 - 2030")
    }
  }

  sealed trait MeasureSystem
  case object CM extends MeasureSystem
  case object Inch extends MeasureSystem

  case class Height(value: Int, system: MeasureSystem) extends PassportField
  object Height {
    implicit lazy val fromString: FromString[Height] = FromString { s =>
      val (heightString, system) = s.partition(_.isDigit)
      val height = heightString.toInt
      system match {
        case "cm" =>
          if ((height >= 150) && (height <= 193)) Right(Height(height, CM))
          else Left("Height not within 150 - 193")
        case "in" =>
          if ((height >= 59) && (height <= 76)) Right(Height(height, CM))
          else Left("Height not within 59 - 76")
        case _ => Left("Must provide cm / in for height")
      }
    }
  }

  case class HairColor(value: String) extends PassportField
  object HairColor {
    implicit lazy val fromString: FromString[HairColor] = FromString { s =>
      val regex = """#[0-9a-f]{6}""".r
      s match {
        case regex() => Right(HairColor(s))
        case _ => Left("HairColor invalid format")
      }
    }
  }

  case class EyeColor(value: String) extends PassportField
  object EyeColor {
    val validColors = Set("amb", "blu", "brn", "gry", "grn", "hzl", "oth")
    implicit lazy val fromString: FromString[EyeColor] = FromString { s =>
      if (validColors.contains(s)) Right(EyeColor(s))
      else Left(s"$s not a valid eye colour")
    }
  }

  case class PassportId(value: String) extends PassportField
  object PassportId {
    implicit lazy val fromString: FromString[PassportId] = FromString { s =>
      val regex = """[0-9]{9}""".r
      s match {
        case regex() => Right(PassportId(s))
        case _ => Left("PassportId must be 9 digits")
      }
    }
  }

  case class CountryId(value: String) extends PassportField
  object CountryId {
    implicit lazy val fromString: FromString[CountryId] = FromString { s => Right(CountryId(s)) }
  }

  case class Parser[A](fields: Map[String, String], matchedFields: List[Either[String, A]]) {
    def required[Field <: A](key: String)(implicit fs: FromString[Field]) = {
      val validation = fields.get(key).map(fs.parse).getOrElse(Left(s"Missing required key: $key"))
      Parser(fields, validation :: matchedFields)
    }

    def optional[Field <: A](key: String)(implicit fs: FromString[Field]) = {
      val validation = fields.get(key).map(fs.parse)
      Parser(fields, if (validation.isDefined) validation.get :: matchedFields else matchedFields)
    }

    def run: Either[String, List[A]] =
      matchedFields.partitionMap(identity) match {
        case (Nil, rights) => Right(rights)
        case (lefts, _)    => Left(lefts.mkString(", "))
      }
  }

  object Parser {
    def apply[A](strings: Map[String, String]): Parser[A] = new Parser(strings, Nil)
  }

  val passportStrings = lines.mkString("\n").split("\n\n")
  val passportFieldStrings = passportStrings.map(_.split("""\s"""))

  part1(passportFieldStrings.count(array => (array.length == 8) || ((array.length == 7) && (!array.exists(_.startsWith("cid"))))))

  def parseFields(map: Map[String, String]): Either[String, List[PassportField]] = Parser[PassportField](map)
    .required[BirthYear]("byr")
    .required[IssueYear]("iyr")
    .required[ExpirationYear]("eyr")
    .required[Height]("hgt")
    .required[HairColor]("hcl")
    .required[EyeColor]("ecl")
    .required[PassportId]("pid")
    .optional[CountryId]("cid")
    .run

  val passportFieldMaps = passportFieldStrings.map(_.map(_.splitOn(':')).toMap)
  part2(passportFieldMaps.map(parseFields).count(_.isRight))
}
