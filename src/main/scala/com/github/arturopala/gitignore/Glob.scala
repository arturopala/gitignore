/*
 * Copyright 2020 Artur Opala
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package com.github.arturopala.gitignore

/** Globbing pathnames.
  *
  * Wildcard pattern matcher implementing the same rules as
  * https://www.man7.org/linux/man-pages/man7/glob.7.html
  *
  * Note that wildcard patterns are not regular expressions, although
  * they are a bit similar.  First of all, they match filenames,
  * rather than text, and secondly, the conventions are not the same:
  * for example, in a regular expression '*' means zero or more
  * copies of the preceding thing.
  *
  * Pattern syntax:
  *
  * A string is a wildcard pattern if it contains one of the
  * characters '?', '*' or '['.  Globbing is the operation that
  * expands a wildcard pattern into the list of pathnames matching
  * the pattern. Matching is defined by:
  *
  * A '?' (not between brackets) matches any single character.
  *
  * A '*' (not between brackets) matches any string, including the
  *       empty string.
  *
  * A '/' in a pathname cannot be matched by a '?' or '*' wildcard,
  *       or by a range like "[.-0]". A range containing an explicit
  *       '/' character is syntactically incorrect.
  *
  * An expression "[...]" where the first character after the leading
  * '[' is not an '!' matches a single character, namely any of the
  * characters enclosed by the brackets.  The string enclosed by the
  * brackets cannot be empty; therefore ']' can be allowed between
  * the brackets, provided that it is the first character.  (Thus,
  * "[][!]" matches the three characters '[', ']' and '!'.)
  *
  * There is one special convention: two characters separated by '-'
  * denote a range.  (Thus, "[A-Fa-f0-9]" is equivalent to
  * "[ABCDEFabcdef0123456789]".)  One may include '-' in its literal
  * meaning by making it the first or last character between the
  * brackets.  (Thus, "[]-]" matches just the two characters ']' and
  * '-', and "[--0]" matches the three characters '-', '.', '0',
  * since '/' cannot be matched.)
  *
  * Now that regular expressions have bracket expressions where the
  * negation is indicated by a '^', POSIX has declared the effect of
  * a wildcard pattern "[^...]" to be undefined.
  *
  * An expression "[!...]" matches a single character, namely any
  * character that is not matched by the expression obtained by
  * removing the first '!' from it.  (Thus, "[!]a-]" matches any
  * single character except ']', 'a' and '-'.)
  *
  * One can remove the special meaning of '?', '*' and '[' by
  * preceding them by a backslash, or, in case this is part of a
  * shell command line, enclosing them in quotes.  Between brackets
  * these characters stand for themselves.  Thus, "[[?*\]" matches
  * the four characters '[', '?', '*' and '\'.
  */
object Glob {

  final def isWildcardPattern(pattern: String): Boolean =
    pattern.foldLeft(false) { (a, c) =>
      a || (c match {
        case '?' | '*' | '[' => true
        case _               => false
      })
    }

  /** Compile pattern expression as re-usable [[Pattern]] instance. */
  final def compile(pattern: String): Pattern = {

    val (patterns, remaining, _, outsideBracket) = pattern
      .foldLeft((List.empty[Pattern], "", true, true)) { case ((ps, acc, notEscaped, outsideBracket), c) =>
        if (c == '?' && notEscaped && outsideBracket) {
          if (acc.isEmpty)
            (AnySingleCharacterPattern :: ps, "", true, true)
          else
            (AnySingleCharacterPattern :: LiteralPattern(acc) :: ps, "", true, true)
        } else if (c == '*' && notEscaped && outsideBracket) {
          if (acc.isEmpty)
            ps match {
              case AnyStringPattern :: ps2 =>
                (AnythingPattern :: ps2, "", true, true)
              case _ =>
                (AnyStringPattern :: ps, "", true, true)
            }
          else
            (AnyStringPattern :: LiteralPattern(acc) :: ps, "", true, true)
        } else if (c == '[' && notEscaped && outsideBracket) {
          if (acc.isEmpty) (ps, "", true, false)
          else (LiteralPattern(acc) :: ps, "", true, false)
        } else if (c == ']' && notEscaped && !outsideBracket && acc.nonEmpty) {
          (new BracketPattern(acc) :: ps, "", true, true)
        } else {
          if (notEscaped)
            (ps, acc + c, c != '\\', outsideBracket)
          else
            (ps, acc.dropRight(1) + c, true, outsideBracket)
        }
      }

    val remaining2 =
      if (outsideBracket) remaining else "[" + remaining

    // add remaining string as a literal pattern
    val patterns2 = if (remaining2.nonEmpty) {
      patterns match {
        case Nil => LiteralPattern(remaining2) :: Nil
        case _   => LiteralPattern(remaining2) :: patterns
      }
    } else patterns

    patterns2 match {
      case Nil      => throw new Exception(s"Glob compilation failed for pattern: $pattern")
      case p :: Nil => p
      case _        => CompositePattern(patterns2.reverse)
    }

  }

  /** A compiled representation of a glob pattern. */
  sealed trait Pattern {

    /** Creates a matcher that will match the given input against this pattern. */
    final def matcher(input: CharSequence): Matcher =
      Matcher(input, this)

    /** A minimum width of the string to be considered a match for this pattern. */
    def minWidth: Int

    /** Returns the regular expression from which this pattern was compiled. */
    val pattern: String

  }

  /** Character check defined between brackets, either class or range. */
  sealed trait CharacterCheck {

    /** Check if given character can be accepted. */
    def check(c: Char): Boolean
  }

  /** A type of pattern with variable match length,
    * which can possibly consume nothing or all the remaining input.
    */
  sealed trait WildcardPattern extends Pattern {
    override val minWidth: Int = 0
  }

  /** A type of pattern matching single character only. */
  sealed trait SingleCharacterPattern extends Pattern with WildcardPattern {
    override val minWidth: Int = 1
    def matches(c: Char): Boolean
  }

  /** A pattern consisting of a sequence of nested patterns. */
  final case class CompositePattern(patterns: List[Pattern]) extends Pattern {
    override val minWidth: Int = patterns.foldLeft(0)(_ + _.minWidth)
    override val pattern: String = patterns.foldLeft("")(_ + _.pattern)
  }

  /** A pattern matching literally, without any wildcards. */
  final case class LiteralPattern(pattern: String) extends Pattern {
    override def minWidth: Int = pattern.length()
  }

  /** A wildcard pattern matching anything but path separator '/' character. */
  final case object AnyStringPattern extends Pattern with WildcardPattern {
    override val pattern: String = "*"
  }

  /** A wildcard pattern matching anything, inluding path separator. */
  final case object AnythingPattern extends Pattern with WildcardPattern {
    override val pattern: String = "**"
  }

  /** A wildcard pattern matching any single character except path separator '/' character. */
  final object AnySingleCharacterPattern extends Pattern with SingleCharacterPattern {
    override def matches(c: Char): Boolean = c != '/'
    override val pattern: String = "?"
  }

  /** A wildcard pattern matching either class or range of characters. */
  final case class BracketPattern(pattern: String) extends Pattern with SingleCharacterPattern {
    val characterCheck: CharacterCheck = CharacterCheck.compile(pattern)
    override def matches(c: Char): Boolean = characterCheck.check(c)
  }

  /** Support for character classes and ranges. */
  final object CharacterCheck {

    /** Compile expression between the brackets into a [[CharacterCheck]]. */
    def compile(pattern: String): CharacterCheck =
      if (pattern.isEmpty()) throw new Exception("A character check pattern cannot be empty.")
      else if (pattern.head == '!') {
        val checks = CharacterCheck.compileInternal(pattern.drop(1))
        NegatedCompositeCharacterCheck(checks)
      } else
        compileInternal(pattern) match {
          case ch :: Nil => ch
          case checks    => CompositeCharacterCheck(checks)
        }

    def compileInternal(pattern: String): List[CharacterCheck] = {
      val (list, remaining, _) = pattern
        .foldLeft((List.empty[CharacterCheck], "", false)) { case ((ls, acc, isRange), c) =>
          if (c == '/')
            throw new Exception(
              "A character check range containing an explicit '/' character is syntactically incorrect."
            )
          else if (c == '-' && acc.nonEmpty)
            (
              if (acc.length() > 1) CharacterClassCheck(acc.dropRight(1)) :: ls else ls,
              acc.takeRight(1) + '-',
              true
            )
          else if (isRange)
            (CharacterRangeCheck(acc.head, c) :: ls, "", false)
          else
            (ls, acc + c, false)
        }
      if (remaining.nonEmpty) CharacterClassCheck(remaining) :: list
      else list
    }
  }

  /** Composite check nesting a sequence of positive checks. */
  final case class CompositeCharacterCheck(checks: List[CharacterCheck]) extends CharacterCheck {
    override def check(c: Char): Boolean =
      c != '/' && checks.foldLeft(false)((a, ck) => a || ck.check(c))
  }

  /** Composite check nesting a sequence of negative checks. */
  final case class NegatedCompositeCharacterCheck(checks: List[CharacterCheck]) extends CharacterCheck {
    override def check(c: Char): Boolean =
      c != '/' && !checks.foldLeft(false)((a, ck) => a || ck.check(c))
  }

  /** Checks if the character is of any of the provided characters.
    *
    * The string cannot be empty; therefore ']' can be allowed,
    * provided that it is the first character.
    * (Thus, "][!" matches the three characters '[', ']' and '!'.)
    */
  final case class CharacterClassCheck(characters: String) extends CharacterCheck {
    override def check(c: Char): Boolean =
      c != '/' && characters.contains(c)
  }

  /** Checks if the character is between provided range.
    *
    * There is one special convention: two characters separated by '-'
    * denote a range.  (Thus, "[A-Fa-f0-9]" is equivalent to
    * "[ABCDEFabcdef0123456789]".)  One may include '-' in its literal
    * meaning by making it the first or last character between the
    * brackets.  (Thus, "[]-]" matches just the two characters ']' and
    * '-', and "[--0]" matches the three characters '-', '.', '0',
    * since '/' cannot be matched.)
    */
  final case class CharacterRangeCheck(from: Char, to: Char) extends CharacterCheck {
    override def check(c: Char): Boolean =
      c != '/' && c >= from && c <= to
  }

  /** An engine that performs match operations on a character sequence by interpreting a Pattern. */
  sealed trait Matcher {

    /** Attempts to find the next subsequence of the input sequence that matches the pattern. */
    def find(): Boolean

    /** Returns the start index of the previous match.
      * @throws IllegalStateException - If no match has yet been attempted, or if the previous match operation failed
      */
    def start(): Int

    /** Returns the offset after the last character matched.
      * @throws IllegalStateException - If no match has yet been attempted, or if the previous match operation failed
      */
    def end(): Int
  }

  final object Matcher {

    final def apply(value: CharSequence, pattern: Pattern): Matcher =
      new Matcher {

        var r = false
        var s = Int.MaxValue
        var e = Int.MinValue

        override def find(): Boolean = {
          val (r1, s1, e1) =
            Matcher.find(value, pattern, if (s == Int.MaxValue) 0 else s + 1)
          r = r1
          s = s1
          e = e1
          r
        }

        override def start(): Int =
          if (r && s != Int.MaxValue) s
          else throw new IllegalStateException

        override def end(): Int =
          if (r && e != Int.MinValue) e
          else throw new IllegalStateException
      }

    final def find(value: CharSequence, pattern: Pattern, startPosition: Int = 0): (Boolean, Int, Int) = {
      Debug.debug(s"Finding ${pattern.pattern} as $pattern in $value\n----------------")
      val zoom = Zoom(value, startPosition)
      val possible = computeContour(zoom, pattern) &&
        zoom.closeUpFrameAndResetContour
      val (minFrom, maxTo) = zoom.frame
      var result = possible && findA(zoom, pattern, leftToRight = true, adjacent = false, level = 0)
      var continue = possible && !result
      if (continue && computeNextFrame(zoom.setFrame(minFrom, maxTo), pattern, maxTo)) {
        Debug.debug(s"Max to $maxTo\n   $zoom")
        while (continue) {
          Debug.debug("-" * 32)
          val (prevFrom, prevTo) = zoom.frame
          result = findA(zoom, pattern, leftToRight = true, adjacent = false, level = 0)
          continue = !result && computeNextFrame(zoom.setFrame(prevFrom, prevTo), pattern, maxTo)
        }
      }
      Debug.debug(result, s"${if (result) "Found" else "Not found"} at (${zoom.start()},${zoom.end()})")
      (result, zoom.start(), zoom.end())
    }

    final def computeNextFrame(zoom: Zoom, pattern: Pattern, maxTo: Int): Boolean = {
      var exists = true
      var possible = false
      while (exists && !possible) {
        exists = zoom.squeezeRightOrLeft(1, pattern.minWidth, maxTo)
        possible = computeContour(zoom, pattern) &&
          zoom.closeUpFrameAndResetContour
      }
      exists && possible
    }

    final def computeContour(zoom: Zoom, pattern: Pattern): Boolean = {
      val r = pattern match {
        case CompositePattern(Nil) =>
          false

        case cp @ CompositePattern(pattern :: patterns) =>
          def next(p: List[Pattern]): Zoom = p match {
            case Nil =>
              zoom
            case p :: Nil =>
              val z = zoom.copy
              computeContour(z, p)
              z
            case p :: ps =>
              val z = zoom.copy
              computeContour(z, p) &&
              z.unionContour(next(ps))
              z
          }
          computeContour(zoom, pattern) &&
          zoom.unionContour(next(patterns)) &&
          zoom.contourLength >= cp.minWidth

        case LiteralPattern(literal) =>
          zoom.lookupFor(literal)
        case p: SingleCharacterPattern =>
          zoom.lookupWhile(p.matches, maxSteps = 1)
        case AnyStringPattern =>
          zoom.takeAll()
        case AnythingPattern =>
          zoom.takeAll()
      }
      Debug.debug(r, s"Match ${pattern.pattern} is ${if (r) "possible" else "not possible"} in\n   $zoom")
      r
    }

    private def findA(zoom: Zoom, pattern: Pattern, leftToRight: Boolean, adjacent: Boolean, level: Int): Boolean = {
      Debug.debug(
        level,
        s"findA ${pattern.pattern} ${if (leftToRight) ">>>" else "<<<"} ${if (adjacent) "adjacent" else ""} in\n   $zoom"
      )
      pattern match {
        case CompositePattern(patterns) =>
          findB(zoom, patterns, leftToRight, adjacent, level)

        case LiteralPattern(literal) =>
          if (leftToRight) zoom.lookupRightFor(literal, if (adjacent) 0 else Int.MaxValue)
          else zoom.lookupLeftFor(literal, if (adjacent) 0 else Int.MaxValue)

        case p: SingleCharacterPattern =>
          if (leftToRight) zoom.lookupRightWhile(p.matches, 1)
          else zoom.lookupLeftWhile(p.matches, 1)

        case AnyStringPattern =>
          if (leftToRight) zoom.lookupRightUntil(_ == '/', minSteps = 0)
          else zoom.lookupLeftUntil(_ == '/', minSteps = 0)

        case AnythingPattern =>
          if (leftToRight) zoom.takeAllFromLeft()
          else zoom.takeAllFromRight()
      }
    }

    private def findB(
      zoom: Zoom,
      patterns: List[Pattern],
      leftToRight: Boolean,
      adjacent: Boolean,
      level: Int
    ): Boolean = {
      Debug.debug(
        level,
        s"findB P${patterns.size} L$level ${if (leftToRight) ">>>" else "<<<"} ${if (adjacent) "adjacent"
        else ""} in\n   $zoom"
      )
      val r = patterns match {
        case Nil => false

        case p :: Nil =>
          findA(zoom, p, leftToRight, adjacent, level)

        case (g: WildcardPattern) :: ps =>
          Debug.debug(s"L$level wildcard ${g.pattern}")
          val zoom1 = zoom.copyFrameAndResetContour
          zoom1.resizeFrame(g.minWidth, leftToRight)
          findB(zoom1, ps.reverse, !leftToRight, false, level + 1) &&
          zoom1.flipFrame(zoom, leftToRight) &&
          findA(zoom1, g, !leftToRight, true, level + 1) &&
          zoom.merge(zoom1)

        case p :: ps =>
          if (findA(zoom, p, leftToRight, adjacent, level)) {
            findB(zoom, ps, leftToRight, true, level)
          } else false
      }
      Debug.debug(level, r, s"findB ${patterns.size} L$level ${if (r) "found" else "not found"} in\n   $zoom")
      r
    }
  }

}
