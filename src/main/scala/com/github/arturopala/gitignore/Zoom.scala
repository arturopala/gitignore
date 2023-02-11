/*
 * Copyright 2021 Artur Opala
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

import scala.annotation.tailrec

/** A Mutable wrapper of the character sequence.
  * Helps track different lookup operations.
  */
final class Zoom(input: CharSequence) {

  private var from: Int = 0
  private var to: Int = input.length()
  private var min: Int = Int.MaxValue
  private var max: Int = Int.MinValue

  def start(): Int = min
  def end(): Int = max
  def frame: (Int, Int) = (from, to)
  def left: Int = from
  def right: Int = to
  def isEmpty: Boolean = from >= to
  def nonEmpty: Boolean = from < to
  def frameWidth: Int = Math.max(0, to - from)

  def contour: (Int, Int) = (min, max)
  def hasContour: Boolean = max >= 0 && min <= input.length() && min <= max
  def contourLength: Int = if (hasContour) max - min else 0

  def minElseBottom: Int = if (min != Int.MaxValue) min else 0
  def maxElseTop: Int = if (max != Int.MinValue) max else input.length
  def minElseTop: Int = if (min != Int.MaxValue) min else input.length
  def maxElseBottom: Int = if (max != Int.MinValue) max else 0

  def setFrame(from: Int, to: Int): Zoom = {
    this.from = Math.max(Math.min(from, input.length() - 1), 0)
    this.to = Math.min(input.length(), Math.max(from, to))
    this
  }

  def resetContour: Zoom = {
    min = Int.MaxValue
    max = Int.MinValue
    this
  }

  /** Similar to [[String.indexOf]] */
  def indexOf(string: String): Int = {
    @tailrec
    def lookup(
      stringPos: Int,
      inputPos: Int,
      mark: Int,
      first: Boolean
    ): Int =
      if (stringPos == string.length()) mark
      else if (inputPos == input.length()) -1
      else {
        if (input.charAt(inputPos) == string.charAt(stringPos))
          lookup(stringPos + 1, inputPos + 1, if (first) inputPos else mark, false)
        else if (first)
          lookup(0, inputPos + 1, -1, true)
        else
          lookup(0, mark + 1, -1, true)
      }

    if (string.isEmpty) 0
    else lookup(0, from, -1, true)
  }

  /** Similar to [[String.lastIndexOf]] */
  def lastIndexOf(string: String): Int = {
    @tailrec
    def lookup(
      stringPos: Int,
      inputPos: Int,
      mark: Int,
      first: Boolean
    ): Int =
      if (stringPos < 0) mark - string.length() + 1
      else if (inputPos < 0) -1
      else {
        if (input.charAt(inputPos) == string.charAt(stringPos))
          lookup(stringPos - 1, inputPos - 1, if (first) inputPos else mark, false)
        else if (first)
          lookup(string.length() - 1, inputPos - 1, -1, true)
        else
          lookup(string.length() - 1, mark - 1, -1, true)
      }

    if (string.isEmpty) 0
    else lookup(string.length() - 1, to - 1, -1, true)
  }

  /** Lookup for the given string from the left side. */
  def lookupRightFor(string: String, maxDistance: Int = Int.MaxValue): Boolean =
    nonEmpty && {
      Debug.debug(s"lookupRightFor${if (maxDistance == Int.MaxValue) "" else "(adjacent)"}: $string\n   $this")
      val i = indexOf(string)
      (i - from <= maxDistance && i >= from && (i + string.length) <= to) && {
        from = i + string.length
        min = Math.min(min, i)
        max = Math.max(max, i + string.length)
        Debug.debug(s"=> $this")
        true
      }
    }

  /** Lookup for the given string from the right side. */
  def lookupLeftFor(string: String, maxDistance: Int = Int.MaxValue): Boolean =
    nonEmpty && {
      Debug.debug(s"lookupLeftFor${if (maxDistance == Int.MaxValue) "" else "(adjacent)"}: $string\n   $this")
      val i = lastIndexOf(string)
      ((to - i - string.length) <= maxDistance && i >= from && (i + string.length) <= to) && {
        to = i
        min = Math.min(min, i)
        max = Math.max(max, i + string.length)
        Debug.debug(s"=> $this")
        true
      }
    }

  /** Lookup for the given string from both sides. */
  def lookupFor(string: String): Boolean =
    nonEmpty && {
      Debug.debug(s"lookupFor: $string\n   $this")
      val i1 = indexOf(string)
      val i2 = lastIndexOf(string)
      min =
        if (i1 >= 0) i1
        else Int.MaxValue
      max =
        if (i2 >= 0 && i2 + string.length <= to) i2 + string.length
        else Int.MinValue
      Debug.debug(hasContour, s"=> $this")
      hasContour
    }

  /** Lookup from the left side while check succeeds. */
  def lookupRightWhile(check: Char => Boolean, maxSteps: Int = Int.MaxValue): Boolean =
    nonEmpty && {
      Debug.debug(s"lookupRightWhile:\n   $this")
      var c = maxSteps
      val mark = from
      while (c > 0 && from < to && check(input.charAt(from))) {
        from = from + 1
        c = c - 1
      }

      from > mark && {
        min = Math.min(min, mark)
        max = Math.max(max, from)
        Debug.debug(s"=> $this")
        true
      }
    }

  /** Lookup from the right side while check succeeds. */
  def lookupLeftWhile(check: Char => Boolean, maxSteps: Int = Int.MaxValue): Boolean =
    nonEmpty && {
      Debug.debug(s"lookupLeftWhile:\n   $this")
      var c = maxSteps
      val mark = to
      while (c > 0 && to > from && check(input.charAt(to - 1))) {
        to = to - 1
        c = c - 1
      }

      to < mark && {
        min = Math.min(min, to)
        max = Math.max(max, mark)
        Debug.debug(s"=> $this")
        true
      }
    }

  def lookupWhile(check: Char => Boolean, maxSteps: Int = 0): Boolean = {
    Debug.debug(s"lookupWhile:\n   $this")
    var i1 = from
    var stop1 = i1 >= to || check(input.charAt(i1))
    while (!stop1)
      stop1 = { i1 = i1 + 1; i1 >= to } || check(input.charAt(i1))
    var i2 = to
    var stop2 = i2 <= from || check(input.charAt(i2 - 1))
    while (!stop2)
      stop2 = { i2 = i2 - 1; i2 <= from } || check(input.charAt(i2 - 1))

    min = Math.min(min, i1)
    max = Math.max(max, i2)
    val r = hasContour && contourLength >= maxSteps
    Debug.debug(r, s"=> $this")
    r
  }

  /** Lookup from the left side until check succeeds. */
  def lookupRightUntil(check: Char => Boolean, minSteps: Int = 0, maxSteps: Int = Int.MaxValue): Boolean =
    (isEmpty && {
      Debug.debug(s"lookupRightUntil (empty):\n   $this")
      min = Math.min(from, min)
      max = Math.max(to, max)
      Debug.debug(s"=> $this")
      true
    }) || {
      Debug.debug(s"lookupRightUntil:\n   $this")
      var c = maxSteps
      val mark = from
      while (c > 0 && from < to && !check(input.charAt(from))) {
        from = from + 1
        c = c - 1
      }

      (from - mark >= minSteps) && {
        min = Math.min(min, mark)
        max = Math.max(max, from)
        Debug.debug(s"=> $this")
        true
      }
    }

  /** Lookup from right until check succeeds. */
  def lookupLeftUntil(check: Char => Boolean, minSteps: Int = 0, maxSteps: Int = Int.MaxValue): Boolean =
    (isEmpty && {
      Debug.debug(s"lookupLeftUntil (empty):\n   $this")
      min = Math.min(from, min)
      max = Math.max(to, max)
      Debug.debug(s"=> $this")
      true
    }) || {
      Debug.debug(s"lookupLeftUntil:\n   $this")
      var c = maxSteps
      val mark = to
      while (c > 0 && to > from && !check(input.charAt(to - 1))) {
        to = to - 1
        c = c - 1
      }

      (mark - to >= minSteps) && {
        min = Math.min(min, to)
        max = Math.max(max, mark)
        Debug.debug(s"=> $this")
        true
      }
    }

  def lookupWhileNot(check: Char => Boolean, minSteps: Int = Int.MaxValue, maxSteps: Int = 0): Boolean = {
    Debug.debug(s"lookupUntil:\n   $this")
    var i1 = from
    var stop1 = i1 >= to || !check(input.charAt(i1))
    while (!stop1)
      stop1 = { i1 = i1 + 1; i1 >= to } || !check(input.charAt(i1))
    var i2 = to
    var stop2 = i2 <= from || !check(input.charAt(i2 - 1))
    while (!stop2)
      stop2 = { i2 = i2 - 1; i2 <= from } || !check(input.charAt(i2 - 1))

    min = Math.min(min, i1)
    max = Math.max(max, i2)
    val r = hasContour && {
      val l = contourLength
      l >= maxSteps && l <= minSteps
    }
    Debug.debug(r, s"=> $this")
    r
  }

  /** Expand contour from the left side. */
  def takeAllFromLeft(): Boolean = {
    Debug.debug(s"takeAllFromLeft:\n   $this")
    val mark = from
    from = to
    min = Math.min(min, mark)
    max = Math.max(max, from)
    Debug.debug(s"=> $this")
    true
  }

  /** Expand contour from the right side. */
  def takeAllFromRight(): Boolean = {
    Debug.debug(s"takeAllFromRight:\n   $this")
    val mark = to
    to = from
    min = Math.min(min, to)
    max = Math.max(max, mark)
    Debug.debug(s"=> $this")
    true
  }

  def takeAll(): Boolean = {
    Debug.debug(s"takeAll:\n   $this")
    min = Math.min(min, from)
    max = Math.max(max, to)
    Debug.debug(hasContour, s"=> $this")
    hasContour
  }

  /** Change frame boundary on the left or the right side. */
  def resizeFrame(distance: Int, leftSide: Boolean): Unit =
    if (leftSide) {
      from = Math.min(from + distance, to)
    } else {
      to = Math.max(to - distance, from)
    }

  def noOverlapBetweenContours(other: Zoom): Boolean =
    this.maxElseBottom <= other.minElseTop || other.maxElseBottom <= this.minElseTop

  /** Move this frame on the left or the right side of the other Zoom's contour. */
  def flipFrame(other: Zoom, leftSide: Boolean): Boolean = {
    val r = noOverlapBetweenContours(other)
    Debug.debug(r, s"flipFrame: ${if (leftSide) "left" else "right"}\n   $this\n&  $other")
    r && {
      if (leftSide) {
        from = other.from
        to = Math.max(from, Math.min(this.to, this.min))
      } else {
        to = other.to
        from = Math.min(to, Math.max(this.from, this.max))
      }
      Debug.debug(s"=> $this")
      true
    }
  }

  def noGapBetweenContours(other: Zoom): Boolean =
    this.minElseBottom <= other.maxElseTop && this.maxElseTop >= other.minElseBottom

  /** Merge contours and frames. */
  def merge(other: Zoom): Boolean = {
    val r = noGapBetweenContours(other)
    Debug.debug(r, s"merge:\n   $this\n&  $other")
    from = Math.max(this.from, other.from)
    to = Math.min(this.to, other.to)
    min = Math.min(this.min, other.min)
    max = Math.max(this.max, other.max)
    Debug.debug(r, s"=> $this")
    r
  }

  /** Intersect contours. */
  def intersectContour(other: Zoom): Boolean = {
    if (hasContour && other.hasContour) {
      min = Math.max(this.min, other.min)
      max = Math.min(this.max, other.max)
    } else {
      resetContour
    }
    hasContour
  }

  /** Union contours. */
  def unionContour(other: Zoom): Boolean = {
    if (hasContour && other.hasContour) {
      min = Math.min(this.min, other.min)
      max = Math.max(this.max, other.max)
    } else {
      resetContour
    }
    hasContour
  }

  def closeUpFrameAndResetContour: Boolean =
    (hasContour && {
      from = min
      to = max
      true
    }) & {
      resetContour
      true
    }

  def squeezeRightOrLeft(offset: Int, minWidth: Int, maxTo: Int): Boolean = {
    Debug.debug(s"squeezeRightOrLeft $offset $minWidth $maxTo\n   $this")
    to = to - 1
    if ((isEmpty || frameWidth < minWidth) && to < maxTo) {
      from = from + 1
      to = maxTo
    }
    min = Int.MaxValue
    max = Int.MinValue
    val r = nonEmpty && frameWidth >= minWidth
    Debug.debug(r, s"=> $this")
    r
  }

  /** Copy frame and reset contour. */
  def copyFrameAndResetContour: Zoom = {
    val z = Zoom(input)
    z.from = this.from
    z.to = this.to
    Debug.debug(s"copyFrameAndResetContour:\n   $z")
    z
  }

  /** Exact copy of this Zoom. */
  def copy: Zoom = {
    val z = Zoom(input)
    z.from = this.from
    z.to = this.to
    z.min = this.min
    z.max = this.max
    z
  }

  final def prettyPrint: String = {
    import scala.io.AnsiColor._
    val sb = new StringBuilder()
    var i = 0
    while (i < input.length()) {
      if (i >= from && i < to) sb.append(BLUE) else sb.append(CYAN)
      if (i >= min && i < max) sb.append(WHITE_B + BOLD)
      sb.append(input.charAt(i))
      sb.append(RESET)
      i = i + 1
    }
    sb.toString()
  }

  override def toString(): String =
    s"$prettyPrint from=$from to=$to min=${if (min != Int.MaxValue) min.toString else "_"} max=${if (max != Int.MinValue) max.toString
    else "_"}"

}

object Zoom {

  def apply(value: CharSequence): Zoom =
    new Zoom(value)

  def apply(value: CharSequence, from: Int): Zoom = {
    val z = Zoom(value)
    z.from = from
    z
  }

  def apply(value: CharSequence, from: Int, to: Int): Zoom = {
    val z = Zoom(value)
    z.from = from
    z.to = to
    z
  }

  def apply(value: CharSequence, from: Int, to: Int, min: Int, max: Int): Zoom = {
    val z = Zoom(value)
    z.from = from
    z.to = to
    z.min = min
    z.max = max
    z
  }
}
