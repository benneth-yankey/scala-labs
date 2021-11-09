package io.turntabl.day1

import java.text.SimpleDateFormat

object LabOne {
  def main(args: Array[String]): Unit = {
    // 1.
    // 12.8 degrees to Fahrenheit
    println(celsiusToFahrenheit(12.8))

    // 2.
    // converts 01/02/15 to 1st February 2015
    println(dateConverter("01/02/15"))

    // 3.
    fieldExtractor(passwd)
  }
}



// 1.
// converts degree celsius to fahrenheit
def celsiusToFahrenheit (c: Double): Double = (c * 9 / 5) + 32

// 2.
// converts date from dd/mm/yy to day{ordinal} month year
def dateConverter (dateStr: String) = {
  val date = new SimpleDateFormat("dd/MM/yy").parse(dateStr)
  val formattedDate = new SimpleDateFormat("d MMMM YYYY").format(date)
  val res = formattedDate.split(" ", 2)
  s"${res(0)}${getOrdinal(res(0).toInt)} ${res(1)}"
}

// helper function to get ordinal(suffix) for day
def getOrdinal(d: Int) = {
  d % 10 match
    case 1 => "st"
    case 2 => "nd"
    case 3 => "rd"
    case _ => "th"
}

// 3.
// extract fields using regex
val passwd =
  """
    |root:x:0:0:root:/root:/bin/bash
    |bin:x:1:1:bin:/bin:/sbin/nologin
    |daemon:x:2:2:daemon:/sbin:/sbin/nologin
    |adm:x:3:4:adm:/var/adm:/sbin/nologin
    |lp:x:4:7:lp:/var/spool/lpd:/sbin/nologin
    |sync:x:5:0:sync:/sbin:/bin/sync
    |""".stripMargin

def fieldExtractor (password: String) = {
  val pattern = raw"(/?\w+)+".r
  for matched <- pattern.findAllMatchIn(password) do
    println(matched)
}