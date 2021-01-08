package com.advent.day4

object ValidatePassports {
  val mandatoryFields = Seq("byr", "iyr", "eyr", "hgt", "hcl", "ecl", "pid")
  val validPartOne: String => Boolean = passport =>
    mandatoryFields.forall(field => passport.contains(field))

  val validPartTwo: String => Boolean = passport => {
    val fields = passport
      .replaceAll("\n", " ")
      .split(" ")

    val map = fields.map { field =>
      val pairs = field.split(":")
      (pairs(0), pairs(1))
    }.toMap
    validByr(map) && validIyr(map) &&
    validEyr(map) && validHgt(map) &&
    validEcl(map) && validPid(map) && validHcl(map)
  }

  def countValidPasswords(
      lines: Iterator[String],
      validFunc: String => Boolean
  ) = {
    getPassports(lines.toSeq.mkString("\n")).count(validFunc)
  }

  def validHcl(map: Map[String, String]): Boolean =
    map.get("hcl").exists { pid =>
      pid.matches("^#(\\d|[a-f]){6}$")
    }

  private[day4] def validPid(map: Map[String, String]): Boolean =
    map.get("pid").exists { pid =>
      pid.matches("^(\\d{9})$")
    }

  private[day4] def validHgt(map: Map[String, String]): Boolean =
    map
      .get("hgt")
      .exists { hgt =>
        if (hgt.contains("in")) {
          val hgtNo: Int = hgt.replace("in", "").toInt
          (59 <= hgtNo) && (76 >= hgtNo)
        } else if (hgt.contains("cm")) {
          val hgtNo = hgt.replace("cm", "").toInt
          ((150 <= hgtNo) && (193 >= hgtNo))
        } else {
          false
        }
      }

  val validEcls = List("amb", "blu", "brn", "gry", "grn", "hzl", "oth")
  private[day4] def validEcl(map: Map[String, String]) =
    map.get("ecl").exists { ecl =>
      validEcls.contains(ecl)
    }

  private[day4] def validEyr(map: Map[String, String]) = {
    map.get("eyr").map(_.toInt).exists(a => (2020 <= a) && (a <= 2030))
  }

  private def validIyr(map: Map[String, String]) = {
    map.get("iyr").map(_.toInt).exists(a => (2010 <= a) && (a <= 2020))
  }

  private def validByr(map: Map[String, String]): Boolean = {
    map.get("byr").map(_.toInt).exists(a => (1920 <= a) && (a <= 2002))
  }

  private def getPassports(str: String): Seq[String] = {
    str.split("\n\n")
  }
}
