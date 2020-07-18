package pirate.example

import java.io.File

import pirate._
import Pirate._
import scalaz._
import Scalaz._
import effect.IO
import hedgehog._
import hedgehog.runner.{example => exampleTest, _}

sealed trait Cut
case class ByteCut(list: String, split: Boolean, files: List[File]) extends Cut
case class CharCut(list: String, files: List[File]) extends Cut
case class FieldCut(list: String, suppress: Boolean, delimiter: Char, files: List[File]) extends Cut

// FIX support multi arguments, i.e. files
// FIX flag type inference, what even is that?
// FIX sub-type handling
// FIX support explicit read, i.e. 'list'
// FIX descriptions on fields
object CutMain extends PirateMainIO[Cut] {
  val byte: Parse[Cut] = (ByteCut |*| ((
    flag[String](short('b'), metavar("list") |+| description("The list specifies byte positions."))
  , switch(short('n'), description("Do not split multi-byte characters.")).not
  , arguments[File](metavar("file"))
  ))).map(x => x)

  val char: Parse[Cut] = (CharCut |*| ((
    flag[String](short('c'), metavar("list") |+| description("The list specifies character positions."))
  , arguments[File](metavar("file"))
  ))).map(x => x)

  val field: Parse[Cut] = (FieldCut |*| ((
    flag[String](short('f'), metavar("list") |+| description("The list specifies fields, separated in the input by the field delimiter character (see the -d option.)  Output fields are separated by a single occurrence of the field delimiter character."))
  , switch(short('s'), description("Suppress lines with no field delimiter characters."))
  , flag[Char](both('d', "delimiter"), metavar("delim") |+| description("Use delim as the field delimiter character instead of the tab character.")).default('\t')
  , arguments[File](metavar("file"))
  ))).map(x => x)

  override def prefs = DefaultPrefs().copy(separateTopLevels = true)

  def command: Command[Cut] =
    (byte ||| char ||| field) ~ "cut" ~~
     "This is a demo of the unix cut utility"

  def run(c: Cut) = c match {
    case ByteCut(list, split, files) =>
      IO.putStrLn(s"""cut -b $list ${if (split) "" else "-n "}$files""")
    case CharCut(list, files) =>
      IO.putStrLn(s"""cut -c $list $files""")
    case FieldCut(list, suppress, delimiter, files) =>
      IO.putStrLn(s"""cut -f $list ${if (suppress) "" else "-s "}${if (delimiter == '\t') "" else "-d '" + delimiter + "'"}$files""")
  }
}

object CutExample extends Properties {

  override def tests: List[Test] = List(
      //  Cut Examples
      exampleTest("cat -b 1 one", byte)
    , exampleTest("cut -b 2 -n two", noSplit)
    , exampleTest("cut -c 3 three", char)
    , exampleTest("cut -f 4 four", field)
    , exampleTest("cut -f 5 -s five", supress)
    , exampleTest("cut -f 6 -d x six", delim)
    , exampleTest("cut -f 7 -d x -s seven", delimSupress)
    , exampleTest("cut -f 7 -s -d x seven", supressDelim)
    , exampleTest("cut -b 1 many files", manyFiles)
    , exampleTest("cut with invalid args fails", invalid)
    //  Cut Checks
    , exampleTest("Name is set", CutMain.command.name ==== "cut")
    , exampleTest("Description is set", CutMain.command.description ==== Some("This is a demo of the unix cut utility"))
  )

  def run(args: String*): ParseError \/ Cut =
    Interpreter.run(CutMain.command.parse, args.toList, DefaultPrefs())._2

  def byte: Result =
    run("-b", "1", "one") ====
      ByteCut("1", true, new File("one").pure[List]).right

  def noSplit: Result =
    run("-b", "2", "-n", "two") ====
      ByteCut("2", false, new File("two").pure[List]).right

  def char: Result =
    run("-c", "3", "three") ====
      CharCut("3", new File("three").pure[List]).right

  def field: Result =
    run("-f", "4", "four") ====
      FieldCut("4", false, '\t', new File("four").pure[List]).right

  def supress: Result =
    run("-f", "5", "-s", "five") ====
      FieldCut("5", true, '\t', new File("five").pure[List]).right

  def delim: Result =
    run("-f", "6", "-d", "x", "six") ====
      FieldCut("6", false, 'x', new File("six").pure[List]).right

  def delimSupress: Result =
    run("-f", "7", "-d", "x", "-s", "seven") ====
      FieldCut("7", true, 'x', new File("seven").pure[List]).right

  def supressDelim: Result =
    run("-f", "8", "-s", "-d", "x", "eight") ====
      FieldCut("8", true, 'x', new File("eight").pure[List]).right

  def manyFiles: Result =
    run("-b", "1", "many", "files") ====
      ByteCut("1", true, List(new File("many"), new File("files"))).right

  def invalid: Result =
    Result.assert(Interpreter.run(CutMain.command.parse, nil, DefaultPrefs())._2.toEither.isLeft)
}
