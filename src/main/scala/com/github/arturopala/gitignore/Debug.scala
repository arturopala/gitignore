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

object Debug {

  import scala.io.AnsiColor._

  var isDebug: Boolean = false
  var level: Int = 0

  def indent: String = YELLOW + ("|  " * level) + RESET

  @inline def debug(isSuccess: => Boolean, msg: => String): Unit =
    if (isDebug) {
      debug(msg, if (isSuccess) GREEN else RED)
      print(RESET)
    } else ()

  @inline def debug(msg: => String, color: String): Unit =
    if (isDebug)
      println(indent + color + msg.replaceAllLiterally("\n", RESET + "\n" + indent + color))
    else ()

  @inline def debug(msg: => String): Unit =
    if (isDebug)
      println(indent + msg.replaceAllLiterally("\n", "\n" + indent))
    else ()

  @inline def debug(l: Int, msg: => String): Unit = {
    level = l
    debug(true, msg)
  }

  @inline def debug(l: Int, isSuccess: => Boolean, msg: => String): Unit = {
    level = l
    debug(isSuccess, msg)
  }
}
