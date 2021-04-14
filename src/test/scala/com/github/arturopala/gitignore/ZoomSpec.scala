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

class ZoomSpec extends AnyWordSpecCompat {

  "Zoom" should {
    "wrap the string and report initial focus details" in {
      Zoom("").frame === (0, 0)
      Zoom("a").frame === (0, 1)
      Zoom("abc").frame === (0, 3)
    }

    "lookup string from the left side, and adjust focus and contour" in {
      val z = Zoom("abcdef")
      z.frame === (0, 6)
      z.lookupRightFor("bcd") === true
      z.frame === (4, 6)
      z.hasContour === true
      z.contour === (1, 4)
      z.isEmpty === false
      z.lookupRightFor("def") === false
      z.lookupRightFor("e") === true
      z.frame === (5, 6)
      z.hasContour === true
      z.contour === (1, 5)
      z.isEmpty === false
    }

    "try lookup string from the left side and leave zoom unchanged" in {
      val z = Zoom("abcdef")
      z.frame === (0, 6)
      z.lookupRightFor("bce") === false
      z.frame === (0, 6)
      z.hasContour === false
    }

    "try lookup string from the left side and leave zoom unchanged if max distance reached" in {
      val z = Zoom("abcdef")
      z.frame === (0, 6)
      z.lookupRightFor("cde", 1) === false
      z.frame === (0, 6)
      z.hasContour === false
      z.lookupRightFor("cde", 2) === true
      z.frame === (5, 6)
      z.hasContour === true
      z.contour === (2, 5)
    }

    "lookup string from the right side, and adjust focus and contour" in {
      val z = Zoom("abcdef")
      z.frame === (0, 6)
      z.lookupLeftFor("def") === true
      z.frame === (0, 3)
      z.hasContour === true
      z.contour === (3, 6)
      z.isEmpty === false
      z.lookupLeftFor("abcd") === false
      z.lookupLeftFor("abc") === true
      z.frame === (0, 0)
      z.hasContour === true
      z.contour === (0, 6)
      z.isEmpty === true
    }

    "try lookup string from the right side and leave zoom unchanged" in {
      val z = Zoom("abcdef")
      z.frame === (0, 6)
      z.lookupLeftFor("cef") === false
      z.frame === (0, 6)
      z.hasContour === false
    }

    "try lookup string from the right side and leave zoom unchanged if max distance reached" in {
      val z = Zoom("abcdef")
      z.frame === (0, 6)
      z.lookupLeftFor("abc", 2) === false
      z.frame === (0, 6)
      z.hasContour === false
      z.lookupLeftFor("abc", 3) === true
      z.frame === (0, 0)
      z.hasContour === true
      z.contour === (0, 3)
    }

    "lookup while condition met from the left side with max steps" in {
      val z = Zoom("aaaddf")
      z.frame === (0, 6)
      z.lookupRightWhile(_ == 'a', 2)
      z.frame === (2, 6)
      z.hasContour === true
      z.contour === (0, 2)
      z.isEmpty === false
    }

    "lookup while condition met from the left side" in {
      val z = Zoom("aaaddf")
      z.frame === (0, 6)
      z.lookupRightWhile(_ == 'a')
      z.frame === (3, 6)
      z.hasContour === true
      z.contour === (0, 3)
      z.isEmpty === false
      z.lookupRightWhile(_ == 'd')
      z.frame === (5, 6)
      z.hasContour === true
      z.contour === (0, 5)
      z.isEmpty === false
      z.lookupRightWhile(_ == 'd')
      z.frame === (5, 6)
      z.hasContour === true
      z.contour === (0, 5)
      z.isEmpty === false
      z.lookupRightWhile(_ == 'f')
      z.frame === (6, 6)
      z.hasContour === true
      z.contour === (0, 6)
      z.isEmpty === true
      z.lookupRightWhile(_ == 'f')
      z.frame === (6, 6)
      z.hasContour === true
      z.contour === (0, 6)
      z.isEmpty === true
    }

    "do nothing if while condition not met from the left side" in {
      val z = Zoom("aaaddf")
      z.frame === (0, 6)
      z.lookupRightWhile(_ == 'b')
      z.frame === (0, 6)
      z.hasContour === false
      z.isEmpty === false
    }

    "lookup while condition met from the right side with max steps" in {
      val z = Zoom("bbbdff")
      z.frame === (0, 6)
      z.lookupLeftWhile(_ == 'f', 1)
      z.frame === (0, 5)
      z.hasContour === true
      z.contour === (5, 6)
      z.isEmpty === false
    }

    "lookup while condition met from the right side" in {
      val z = Zoom("bbbdff")
      z.frame === (0, 6)
      z.lookupLeftWhile(_ == 'f')
      z.frame === (0, 4)
      z.hasContour === true
      z.contour === (4, 6)
      z.isEmpty === false
      z.lookupLeftWhile(_ == 'd')
      z.frame === (0, 3)
      z.hasContour === true
      z.contour === (3, 6)
      z.isEmpty === false
      z.lookupLeftWhile(_ == 'b')
      z.frame === (0, 0)
      z.hasContour === true
      z.contour === (0, 6)
      z.isEmpty === true
    }

    "do nothing if while condition not met from the right side" in {
      val z = Zoom("aaaddf")
      z.frame === (0, 6)
      z.lookupLeftWhile(_ == 'd')
      z.frame === (0, 6)
      z.hasContour === false
      z.isEmpty === false
    }

    "lookup until condition met from the left side with max steps" in {
      val z = Zoom("aaaddf")
      z.frame === (0, 6)
      z.lookupRightUntil(_ != 'a', 0, 1)
      z.frame === (1, 6)
      z.hasContour === true
      z.contour === (0, 1)
      z.isEmpty === false
    }

    "lookup until condition met from the left side" in {
      val z = Zoom("aaaddf")
      z.frame === (0, 6)
      z.lookupRightUntil(_ != 'a')
      z.frame === (3, 6)
      z.hasContour === true
      z.contour === (0, 3)
      z.isEmpty === false
      z.lookupRightUntil(_ == 'f')
      z.frame === (5, 6)
      z.hasContour === true
      z.contour === (0, 5)
      z.isEmpty === false
      z.lookupRightUntil(_ != 'f')
      z.frame === (6, 6)
      z.hasContour === true
      z.contour === (0, 6)
      z.isEmpty === true
    }

    "lookup until condition met from the left side for an empty zoom" in {
      val z = Zoom("")
      z.frame === (0, 0)
      z.lookupRightUntil(_ != 'a')
      z.frame === (0, 0)
      z.hasContour === true
      z.contour === (0, 0)
      z.isEmpty === true
    }

    "lookup until condition met from the left side for a zoom with only disallowed character" in {
      val z = Zoom("a")
      z.frame === (0, 1)
      z.lookupRightUntil(_ != 'a')
      z.frame === (1, 1)
      z.hasContour === true
      z.contour === (0, 1)
      z.isEmpty === true
    }

    "lookup until condition met from the left side for a zoom starting with disallowed character" in {
      val z = Zoom("aaaddf")
      z.frame === (0, 6)
      z.lookupRightUntil(_ == 'a', minSteps = 0)
      z.frame === (0, 6)
      z.hasContour === true
      z.contour === (0, 0)
      z.isEmpty === false
    }

    "do nothing if until condition not met from the left side" in {
      val z = Zoom("aaaddf")
      z.frame === (0, 6)
      z.lookupRightUntil(_ == 'a', minSteps = 1)
      z.frame === (0, 6)
      z.hasContour === false
      z.isEmpty === false
    }

    "lookup until condition met from the right side with max steps" in {
      val z = Zoom("aaaddf")
      z.frame === (0, 6)
      z.lookupLeftUntil(_ == 'a', 0, 2)
      z.frame === (0, 4)
      z.hasContour === true
      z.contour === (4, 6)
      z.isEmpty === false
    }

    "lookup until condition met from the right side" in {
      val z = Zoom("aaaddf")
      z.frame === (0, 6)
      z.lookupLeftUntil(_ == 'a')
      z.frame === (0, 3)
      z.hasContour === true
      z.contour === (3, 6)
      z.isEmpty === false
      z.lookupLeftUntil(_ != 'a')
      z.frame === (0, 0)
      z.hasContour === true
      z.contour === (0, 6)
      z.isEmpty === true
    }

    "lookup until condition met from the right side for an empty zoom" in {
      val z = Zoom("")
      z.frame === (0, 0)
      z.lookupLeftUntil(_ == 'a')
      z.frame === (0, 0)
      z.hasContour === true
      z.contour === (0, 0)
      z.isEmpty === true
    }

    "lookup until condition met from the right side for a zoom with only disallowed character" in {
      val z = Zoom("a")
      z.frame === (0, 1)
      z.lookupLeftUntil(_ == 'a')
      z.frame === (0, 1)
      z.hasContour === true
      z.contour === (1, 1)
      z.isEmpty === false
    }

    "lookup until condition met from the right side for a zoom ending with disallowed character" in {
      val z = Zoom("aaaddf")
      z.frame === (0, 6)
      z.lookupLeftUntil(_ == 'f', minSteps = 0)
      z.frame === (0, 6)
      z.hasContour === true
      z.contour === (6, 6)
      z.isEmpty === false
    }

    "do nothing if until condition not met from the right side" in {
      val z = Zoom("aaaddf")
      z.frame === (0, 6)
      z.lookupLeftUntil(_ == 'f', minSteps = 1)
      z.frame === (0, 6)
      z.hasContour === false
      z.isEmpty === false
    }

    "take all from the left side" in {
      val z = Zoom("aaaddf")
      z.frame === (0, 6)
      z.takeAllFromLeft()
      z.frame === (6, 6)
      z.hasContour === true
      z.contour === (0, 6)
      z.isEmpty === true
    }

    "take all from the right side" in {
      val z = Zoom("aaaddf")
      z.frame === (0, 6)
      z.takeAllFromRight()
      z.frame === (0, 0)
      z.hasContour === true
      z.contour === (0, 6)
      z.isEmpty === true
    }

    "report no overlap and gap between contours" in {
      Zoom("aaa").noOverlapBetweenContours(Zoom("aaa")) === true
      Zoom("aaa").noGapBetweenContours(Zoom("aaa")) === true
      val z1 = Zoom("aaa")
      z1.lookupRightFor("a")
      val z2 = z1.copyFrameAndResetContour
      z2.lookupLeftFor("a")
      z1.noOverlapBetweenContours(z2) === true
      z2.noOverlapBetweenContours(z1) === true
      z1.noOverlapBetweenContours(z2) === true
      z1.noGapBetweenContours(z2) === false
      z2.noGapBetweenContours(z1) === false
      z1.noGapBetweenContours(z2) === false
      z2.lookupRightFor("a")
      z1.noOverlapBetweenContours(z2) === true
      z2.noOverlapBetweenContours(z1) === true
      z1.noGapBetweenContours(z2) === true
      z2.noGapBetweenContours(z1) === true
      z2.isEmpty === true
    }

    "report overlap and no gap between contours" in {
      val z1 = Zoom("aaa")
      z1.lookupRightFor("aa")
      val z2 = Zoom("aaa")
      z2.lookupLeftFor("aa")
      z1.noOverlapBetweenContours(z2) === false
      z2.noOverlapBetweenContours(z1) === false
      z1.noOverlapBetweenContours(z2) === false
      z1.noGapBetweenContours(z2) === true
      z2.noGapBetweenContours(z1) === true
      z1.noGapBetweenContours(z2) === true
    }

    "frame left of other zoom" in {
      val z = Zoom("aaa", 1, 3, 0, 1)
      z.flipFrame(Zoom("aaa", 1, 2, 2, 3), true) === true
      z.frame === (1, 1)
      z.hasContour
      z.contour === (0, 1)
    }

    "frame right of other zoom" in {
      val z = Zoom("aaa", 1, 2, 2, 3)
      z.flipFrame(Zoom("aaa", 1, 2, 0, 1), false) === true
      z.frame === (2, 2)
      z.hasContour
      z.contour === (2, 3)
    }

    "merge with other zoom" in {
      Zoom("aaa").merge(Zoom("aaa")) === true
      val z1 = Zoom("aab")
      z1.lookupRightFor("a")
      z1.frame === (1, 3)
      z1.hasContour
      z1.contour === (0, 1)
      val z2 = z1.copyFrameAndResetContour
      z2.lookupLeftFor("a")
      z2.frame === (1, 1)
      z2.hasContour
      z2.contour === (1, 2)
      z1.merge(z2)
      z1.frame === (1, 1)
      z1.hasContour
      z1.contour === (0, 2)
    }

    "merge with other zoom - 2" in {
      val z1 = Zoom("ab")
      z1.lookupLeftFor("a")
      z1.frame === (0, 0)
      z1.hasContour
      z1.contour === (0, 1)
      z1.lookupLeftUntil(_ == '/')
      z1.frame === (0, 0)
      z1.hasContour
      z1.contour === (0, 1)
      z1.lookupRightUntil(_ == '/')
      z1.frame === (0, 0)
      z1.hasContour
      z1.contour === (0, 1)
    }

  }

}
