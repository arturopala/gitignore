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

/** A Mutable wrapper of the string
  * helping to track lookup operations.
  */
final case class Zoom(value: String) {

  private var from: Int = 0
  private var to: Int = value.length()
  private var min: Int = Int.MaxValue
  private var max: Int = Int.MinValue

  def start: Int = min
  def end: Int = max
  def focus: (Int, Int) = (from, to)
  def left: Int = from
  def right: Int = to
  def contour: (Int, Int) = (min, max)
  def hasContour: Boolean = max >= 0 && min <= value.length()
  def isEmpty: Boolean = from >= to
  def nonEmpty: Boolean = from < to

  def minElseBottom: Int = if (min != Int.MaxValue) min else 0
  def maxElseTop: Int = if (max != Int.MinValue) max else value.length
  def minElseTop: Int = if (min != Int.MaxValue) min else value.length
  def maxElseBottom: Int = if (max != Int.MinValue) max else 0

  /** Create new Zoom with contour reset. */
  def copyAndResetContour: Zoom = {
    val z = Zoom(value)
    z.from = this.from
    z.to = this.to
    z
  }

  /** Change focus boundary on the left/right side. */
  def shiftFocus(distance: Int, leftSide: Boolean): Unit =
    if (leftSide) {
      from = Math.min(from + distance, to)
    } else {
      to = Math.max(to - distance, from)
    }

  /** Lookup given string from left side. */
  def lookupRightFor(string: String, maxDistance: Int = Int.MaxValue): Boolean =
    nonEmpty && {
      Debug.debug(s"lookupRightFor${if (maxDistance == Int.MaxValue) "" else "(adjacent)"}: $string\n   $this")
      val i = value.indexOf(string, from)
      (i - from <= maxDistance && i >= from && (i + string.length) <= to) && {
        from = i + string.length
        min = Math.min(min, i)
        max = Math.max(max, i + string.length)
        Debug.debug(s"=> $this")
        true
      }
    }

  /** Lookup given string from right side. */
  def lookupLeftFor(string: String, maxDistance: Int = Int.MaxValue): Boolean =
    nonEmpty && {
      Debug.debug(s"lookupLeftFor${if (maxDistance == Int.MaxValue) "" else "(adjacent)"}: $string\n   $this")
      val i = value.lastIndexOf(string, to - 1)
      ((to - i - string.length) <= maxDistance && i >= from && (i + string.length) <= to) && {
        to = i
        min = Math.min(min, i)
        max = Math.max(max, i + string.length)
        Debug.debug(s"=> $this")
        true
      }
    }

  /** Lookup from left while check succeeds. */
  def lookupRightWhile(check: Char => Boolean, maxSteps: Int = Int.MaxValue): Boolean =
    nonEmpty && {
      Debug.debug(s"lookupRightWhile:\n   $this")
      var c = maxSteps
      val mark = from
      while (c > 0 && from < to && check(value(from))) {
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

  /** Lookup from right while check succeeds. */
  def lookupLeftWhile(check: Char => Boolean, maxSteps: Int = Int.MaxValue): Boolean =
    nonEmpty && {
      Debug.debug(s"lookupLeftWhile:\n   $this")
      var c = maxSteps
      val mark = to
      while (c > 0 && to > from && check(value(to - 1))) {
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

  /** Lookup form left until check succeeds. */
  def lookupRightUntil(check: Char => Boolean, maxSteps: Int = Int.MaxValue): Boolean =
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
      while (c > 0 && from < to && !check(value(from))) {
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

  /** Lookup from right until check succeeds. */
  def lookupLeftUntil(check: Char => Boolean, maxSteps: Int = Int.MaxValue): Boolean =
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
      while (c > 0 && to > from && !check(value(to - 1))) {
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

  def takeAllRight(): Boolean = {
    Debug.debug(s"takeAllRight:\n   $this")
    val mark = from
    from = to
    min = Math.min(min, mark)
    max = Math.max(max, from)
    Debug.debug(s"=> $this")
    true
  }

  def takeAllLeft(): Boolean = {
    Debug.debug(s"takeAllLeft:\n   $this")
    val mark = to
    to = from
    min = Math.min(min, to)
    max = Math.max(max, mark)
    Debug.debug(s"=> $this")
    true
  }

  def noOverlapBetweenContours(other: Zoom): Boolean = {
    Debug.debug(s"noOverlapBetweenContours:\n   $this\n   $other"); true
  } && (this.maxElseBottom <= other.minElseTop || other.maxElseBottom <= this.minElseTop)

  /** Move this focus on the left/right side of the other Zoom's contour. */
  def frame(other: Zoom, leftSide: Boolean): Boolean =
    noOverlapBetweenContours(other) && {
      Debug.debug(s"flip: ${if (leftSide) "left" else "right"}\n   $this\n > $other")
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

  def noGapBetweenContours(other: Zoom): Boolean = {
    Debug.debug(s"noGapBetweenContours:\n   $this\n   $other"); true
  } && (this.minElseBottom <= other.maxElseTop && this.maxElseTop >= other.minElseBottom)

  /** Unify contours and reduce focus. */
  def merge(other: Zoom): Boolean =
    noGapBetweenContours(other) && {
      Debug.debug(s"merge:\n   $this\n   $other")
      from = Math.max(this.from, other.from)
      to = Math.min(this.to, other.to)
      min = Math.min(this.min, other.min)
      max = Math.max(this.max, other.max)
      Debug.debug(s"=> $this")
      true
    }

  /** Exact copy of this Zoom. */
  def fullCopy: Zoom = {
    val z = Zoom(value)
    z.from = this.from
    z.to = this.to
    z.min = this.min
    z.max = this.max
    z
  }

  override def toString(): String =
    s"from=$from to=$to min=${if (min != Int.MaxValue) min.toString else "_"} max=${if (max != Int.MinValue) max.toString
    else "_"} value=${value.substring(from, to)}"

}

object Zoom {
  private[gitignore] def apply(value: String, from: Int, to: Int, min: Int, max: Int): Zoom = {
    val z = Zoom(value)
    z.from = from
    z.to = to
    z.min = min
    z.max = max
    z
  }
}

object Debug {
  val isDebug: Boolean = false

  def debug(msg: => String): Unit =
    if (isDebug) println(msg) else ()
}
