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

class GitIgnoreSpec extends AnyWordSpecCompat {

  Debug.isDebug = false

  val gitIgnore1 = GitIgnore(Seq(".git", "build.sbt", "target", ".scalafmt.conf"))
  val gitIgnore2 = GitIgnore(Seq(".git/", "build.sbt/", "target/", ".scalafmt.conf/"))
  val gitIgnore3 = GitIgnore(Seq("/.git", "/build.sbt", "/target", "/.scalafmt.conf"))
  val gitIgnore4 = GitIgnore(Seq("project/target", "target/streams"))
  val gitIgnore5 = GitIgnore(Seq("project/target/", "target/streams/"))
  val gitIgnore6 = GitIgnore(Seq("/project/target/", "/target/streams"))
  val gitIgnore7 = GitIgnore(Seq("project/plugins.sbt", "resources"))

  "GitIgnore" should {
    "process top directory: target" in {
      val path = List("target")
      gitIgnore1.isIgnored(path, true) shouldBe true
      gitIgnore2.isIgnored(path, true) shouldBe true
      gitIgnore3.isIgnored(path, true) shouldBe true
      gitIgnore4.isIgnored(path, true) shouldBe false
      gitIgnore5.isIgnored(path, true) shouldBe false
      gitIgnore6.isIgnored(path, true) shouldBe false

      GitIgnore(Seq.empty).isIgnored(path, true) shouldBe false
      GitIgnore("*").isIgnored(path, true) shouldBe true
      GitIgnore("*?").isIgnored(path, true) shouldBe true
      GitIgnore("?*").isIgnored(path, true) shouldBe true
      GitIgnore("?*?").isIgnored(path, true) shouldBe true
      GitIgnore("targ*").isIgnored(path, true) shouldBe true
      GitIgnore("*arget").isIgnored(path, true) shouldBe true
      GitIgnore("target*").isIgnored(path, true) shouldBe true
      GitIgnore("*target").isIgnored(path, true) shouldBe true
      GitIgnore("tar*get").isIgnored(path, true) shouldBe true
      GitIgnore("targ*/").isIgnored(path, true) shouldBe true
      GitIgnore("*arget/").isIgnored(path, true) shouldBe true
      GitIgnore("target*/").isIgnored(path, true) shouldBe true
      GitIgnore("*target/").isIgnored(path, true) shouldBe true
      GitIgnore("tar*get/").isIgnored(path, true) shouldBe true
      GitIgnore("/targ*").isIgnored(path, true) shouldBe true
      GitIgnore("/*arget").isIgnored(path, true) shouldBe true
      GitIgnore("/target*").isIgnored(path, true) shouldBe true
      GitIgnore("/*target").isIgnored(path, true) shouldBe true
      GitIgnore("/tar*get").isIgnored(path, true) shouldBe true
      GitIgnore("/targ*/").isIgnored(path, true) shouldBe true
      GitIgnore("/*arget/").isIgnored(path, true) shouldBe true
      GitIgnore("/target*/").isIgnored(path, true) shouldBe true
      GitIgnore("/*target/").isIgnored(path, true) shouldBe true
      GitIgnore("/tar*get/").isIgnored(path, true) shouldBe true

      GitIgnore("*rg.t").isIgnored(path, true) shouldBe false
      GitIgnore("*trget").isIgnored(path, true) shouldBe false
      GitIgnore("targets*").isIgnored(path, true) shouldBe false
      GitIgnore("ar*get").isIgnored(path, true) shouldBe false
      GitIgnore("*rg.t/").isIgnored(path, true) shouldBe false
      GitIgnore("*trget/").isIgnored(path, true) shouldBe false
      GitIgnore("targets*/").isIgnored(path, true) shouldBe false
      GitIgnore("ar*get/").isIgnored(path, true) shouldBe false
      GitIgnore("/*rg.t").isIgnored(path, true) shouldBe false
      GitIgnore("/*trget").isIgnored(path, true) shouldBe false
      GitIgnore("/targets*").isIgnored(path, true) shouldBe false
      GitIgnore("/ar*get").isIgnored(path, true) shouldBe false
      GitIgnore("/*rg.t/").isIgnored(path, true) shouldBe false
      GitIgnore("/*trget/").isIgnored(path, true) shouldBe false
      GitIgnore("/targets*/").isIgnored(path, true) shouldBe false
      GitIgnore("/ar*get/").isIgnored(path, true) shouldBe false

      GitIgnore("t?rget").isIgnored(path, true) shouldBe true
      GitIgnore("?arget").isIgnored(path, true) shouldBe true
      GitIgnore("targe?").isIgnored(path, true) shouldBe true
      GitIgnore("?arge?").isIgnored(path, true) shouldBe true
      GitIgnore("t??get").isIgnored(path, true) shouldBe true
      GitIgnore("t??ge?").isIgnored(path, true) shouldBe true
      GitIgnore("???ge?").isIgnored(path, true) shouldBe true
      GitIgnore("???g??").isIgnored(path, true) shouldBe true
      GitIgnore("/t?rget").isIgnored(path, true) shouldBe true
      GitIgnore("/?arget").isIgnored(path, true) shouldBe true
      GitIgnore("/targe?").isIgnored(path, true) shouldBe true
      GitIgnore("/?arge?").isIgnored(path, true) shouldBe true
      GitIgnore("/t??get").isIgnored(path, true) shouldBe true
      GitIgnore("/t??ge?").isIgnored(path, true) shouldBe true
      GitIgnore("/???ge?").isIgnored(path, true) shouldBe true
      GitIgnore("/???g??").isIgnored(path, true) shouldBe true
      GitIgnore("/t?rget/").isIgnored(path, true) shouldBe true
      GitIgnore("/?arget/").isIgnored(path, true) shouldBe true
      GitIgnore("/targe?/").isIgnored(path, true) shouldBe true
      GitIgnore("/?arge?/").isIgnored(path, true) shouldBe true
      GitIgnore("/t??get/").isIgnored(path, true) shouldBe true
      GitIgnore("/t??ge?/").isIgnored(path, true) shouldBe true
      GitIgnore("/???ge?/").isIgnored(path, true) shouldBe true
      GitIgnore("/???g??/").isIgnored(path, true) shouldBe true
      GitIgnore("t?rget/").isIgnored(path, true) shouldBe true
      GitIgnore("?arget/").isIgnored(path, true) shouldBe true
      GitIgnore("targe?/").isIgnored(path, true) shouldBe true
      GitIgnore("?arge?/").isIgnored(path, true) shouldBe true
      GitIgnore("t??get/").isIgnored(path, true) shouldBe true
      GitIgnore("t??ge?/").isIgnored(path, true) shouldBe true
      GitIgnore("???ge?/").isIgnored(path, true) shouldBe true
      GitIgnore("???g??/").isIgnored(path, true) shouldBe true

      GitIgnore("t?get").isIgnored(path, true) shouldBe false
      GitIgnore("?arge").isIgnored(path, true) shouldBe false
      GitIgnore("tage?").isIgnored(path, true) shouldBe false
      GitIgnore("?arge").isIgnored(path, true) shouldBe false
      GitIgnore("t?get").isIgnored(path, true) shouldBe false
      GitIgnore("t??ge").isIgnored(path, true) shouldBe false
      GitIgnore("??ge?").isIgnored(path, true) shouldBe false
      GitIgnore("??g?").isIgnored(path, true) shouldBe false
      GitIgnore("/t?get").isIgnored(path, true) shouldBe false
      GitIgnore("/?arge").isIgnored(path, true) shouldBe false
      GitIgnore("/tage?").isIgnored(path, true) shouldBe false
      GitIgnore("/?arge").isIgnored(path, true) shouldBe false
      GitIgnore("/t?get").isIgnored(path, true) shouldBe false
      GitIgnore("/t??ge").isIgnored(path, true) shouldBe false
      GitIgnore("/??ge?").isIgnored(path, true) shouldBe false
      GitIgnore("/??g?").isIgnored(path, true) shouldBe false
      GitIgnore("/t?get/").isIgnored(path, true) shouldBe false
      GitIgnore("/?arge/").isIgnored(path, true) shouldBe false
      GitIgnore("/tage?/").isIgnored(path, true) shouldBe false
      GitIgnore("/?arge/").isIgnored(path, true) shouldBe false
      GitIgnore("/t?get/").isIgnored(path, true) shouldBe false
      GitIgnore("/t??ge/").isIgnored(path, true) shouldBe false
      GitIgnore("/??ge?/").isIgnored(path, true) shouldBe false
      GitIgnore("/??g?/").isIgnored(path, true) shouldBe false

      GitIgnore("[t]arget").isIgnored(path, true) shouldBe true
      GitIgnore("t[a-z]rget").isIgnored(path, true) shouldBe true
      GitIgnore("t[!b-z]rget").isIgnored(path, true) shouldBe true

      GitIgnore("t*t").isIgnored(path, true) shouldBe true
      GitIgnore("t\\*t").isIgnored(path, true) shouldBe false
      GitIgnore("ta??et").isIgnored(path, true) shouldBe true
      GitIgnore("ta\\??et").isIgnored(path, true) shouldBe false

      GitIgnore("**/target").isIgnored(path, true) shouldBe true
      GitIgnore("**/target/").isIgnored(path, true) shouldBe true
      GitIgnore("**/*").isIgnored(path, true) shouldBe true
      GitIgnore("**/*/").isIgnored(path, true) shouldBe true

      GitIgnore("tar*").isIgnored(path, true) shouldBe true
      GitIgnore("!target").isIgnored(path, true) shouldBe false
      GitIgnore(Seq("tar*", "!target")).isIgnored(path, true) shouldBe false
      GitIgnore(Seq("!target", "tar*")).isIgnored(path, true) shouldBe true
      GitIgnore(Seq("tar*", "!tar*")).isIgnored(path, true) shouldBe false
      GitIgnore(Seq("tar*", "!*get")).isIgnored(path, true) shouldBe false
      GitIgnore(Seq("!*get", "tar*")).isIgnored(path, true) shouldBe true
      GitIgnore(Seq("*", "!*")).isIgnored(path, true) shouldBe false

      GitIgnore(Seq("tar*", "!?arget", "targe?")).isIgnored(path, true) shouldBe true
    }

    "process top hidden directory: .git" in {
      val path = List(".git")
      //Paths.get(".git").iterator().asScala.map(_.toString).toIterable
      gitIgnore1.isIgnored(path, true) shouldBe true
      gitIgnore2.isIgnored(path, true) shouldBe true
      gitIgnore3.isIgnored(path, true) shouldBe true
      gitIgnore4.isIgnored(path, true) shouldBe false
      gitIgnore5.isIgnored(path, true) shouldBe false
      gitIgnore6.isIgnored(path, true) shouldBe false

      GitIgnore("*").isIgnored(path, true) shouldBe true
      GitIgnore("/*").isIgnored(path, true) shouldBe true
      GitIgnore("*/").isIgnored(path, true) shouldBe true
      GitIgnore("/*/").isIgnored(path, true) shouldBe true
      GitIgnore("????").isIgnored(path, true) shouldBe true
      GitIgnore("/????").isIgnored(path, true) shouldBe true
      GitIgnore("????/").isIgnored(path, true) shouldBe true
      GitIgnore("/????/").isIgnored(path, true) shouldBe true

      GitIgnore(".g*").isIgnored(path, true) shouldBe true
      GitIgnore(".*").isIgnored(path, true) shouldBe true
      GitIgnore("*g*t").isIgnored(path, true) shouldBe true
      GitIgnore("/*g*t").isIgnored(path, true) shouldBe true
      GitIgnore("*").isIgnored(path, true) shouldBe true
      GitIgnore("?git").isIgnored(path, true) shouldBe true
      GitIgnore("?gi?").isIgnored(path, true) shouldBe true
      GitIgnore("?g??").isIgnored(path, true) shouldBe true
      GitIgnore("?g*").isIgnored(path, true) shouldBe true
      GitIgnore("/?g??").isIgnored(path, true) shouldBe true
      GitIgnore("?g*/").isIgnored(path, true) shouldBe true

      GitIgnore("*git?").isIgnored(path, true) shouldBe false
      GitIgnore(".????").isIgnored(path, true) shouldBe false
      GitIgnore("??git").isIgnored(path, true) shouldBe false
      GitIgnore(".?gi?").isIgnored(path, true) shouldBe false

      GitIgnore("**/.git").isIgnored(path, true) shouldBe true
      GitIgnore("**/.git/").isIgnored(path, true) shouldBe true
      GitIgnore("/**/.git/").isIgnored(path, true) shouldBe false
      GitIgnore("/**/.git").isIgnored(path, true) shouldBe false
      GitIgnore("**/*git").isIgnored(path, true) shouldBe true
      GitIgnore("**/?git").isIgnored(path, true) shouldBe true
      GitIgnore("**/*").isIgnored(path, true) shouldBe true
    }

    "process top file: build.sbt" in {
      val path = List("build.sbt")
      gitIgnore1.isIgnored(path, false) shouldBe true
      gitIgnore2.isIgnored(path, false) shouldBe false
      gitIgnore3.isIgnored(path, false) shouldBe true
      gitIgnore4.isIgnored(path, false) shouldBe false
      gitIgnore5.isIgnored(path, false) shouldBe false
      gitIgnore6.isIgnored(path, false) shouldBe false

      GitIgnore("*").isIgnored(path, false) shouldBe true
      GitIgnore("/*").isIgnored(path, false) shouldBe true
      GitIgnore("*/").isIgnored(path, false) shouldBe false
      GitIgnore("/*/").isIgnored(path, false) shouldBe false
      GitIgnore("*.???").isIgnored(path, false) shouldBe true
      GitIgnore("/*.???").isIgnored(path, false) shouldBe true
      GitIgnore("*.???/").isIgnored(path, false) shouldBe false
      GitIgnore("/*.???/").isIgnored(path, false) shouldBe false
      GitIgnore("*.sbt").isIgnored(path, false) shouldBe true
      GitIgnore("/*.sbt").isIgnored(path, false) shouldBe true
      GitIgnore("*.sbt/").isIgnored(path, false) shouldBe false
      GitIgnore("/*.sbt/").isIgnored(path, false) shouldBe false
      GitIgnore("?????.sbt").isIgnored(path, false) shouldBe true
      GitIgnore("/?????.sbt").isIgnored(path, false) shouldBe true
      GitIgnore("?????.sbt/").isIgnored(path, false) shouldBe false
      GitIgnore("/?????.sbt/").isIgnored(path, false) shouldBe false

      GitIgnore("*.????").isIgnored(path, false) shouldBe false
      GitIgnore("/*.????").isIgnored(path, false) shouldBe false
      GitIgnore("*.????/").isIgnored(path, false) shouldBe false
      GitIgnore("/*.????/").isIgnored(path, false) shouldBe false

      GitIgnore("*.sbt").isIgnored(path, false) shouldBe true
      GitIgnore("build.*").isIgnored(path, false) shouldBe true
      GitIgnore("/*.sbt").isIgnored(path, false) shouldBe true
      GitIgnore("/build.*").isIgnored(path, false) shouldBe true

      GitIgnore("/*.sbt/").isIgnored(path, false) shouldBe false
      GitIgnore("/build.*/").isIgnored(path, false) shouldBe false
      GitIgnore("*.sbt/").isIgnored(path, false) shouldBe false
      GitIgnore("build.*/").isIgnored(path, false) shouldBe false

      GitIgnore("build*sbt").isIgnored(path, false) shouldBe true
      GitIgnore("build?sbt").isIgnored(path, false) shouldBe true
      GitIgnore("buil*bt").isIgnored(path, false) shouldBe true
      GitIgnore("buil???bt").isIgnored(path, false) shouldBe true
      GitIgnore("/build*sbt").isIgnored(path, false) shouldBe true
      GitIgnore("/build?sbt").isIgnored(path, false) shouldBe true
      GitIgnore("/buil*bt").isIgnored(path, false) shouldBe true
      GitIgnore("/buil???bt").isIgnored(path, false) shouldBe true
      GitIgnore("/build*sbt/").isIgnored(path, false) shouldBe false
      GitIgnore("/build?sbt/").isIgnored(path, false) shouldBe false
      GitIgnore("/buil*bt/").isIgnored(path, false) shouldBe false
      GitIgnore("/buil???bt/").isIgnored(path, false) shouldBe false
      GitIgnore("build*sbt/").isIgnored(path, false) shouldBe false
      GitIgnore("build?sbt/").isIgnored(path, false) shouldBe false
      GitIgnore("buil*bt/").isIgnored(path, false) shouldBe false
      GitIgnore("buil???bt/").isIgnored(path, false) shouldBe false

      GitIgnore("*.sbt").isIgnored(path, false) shouldBe true
      GitIgnore("!build.sbt").isIgnored(path, false) shouldBe false
      GitIgnore(Seq("*.sbt", "!build.sbt")).isIgnored(path, false) shouldBe false
    }

    "process top hidden file: .scalafmt.conf" in {
      val path = List(".scalafmt.conf")
      gitIgnore1.isIgnored(path, false) shouldBe true
      gitIgnore2.isIgnored(path, false) shouldBe false
      gitIgnore3.isIgnored(path, false) shouldBe true
      gitIgnore4.isIgnored(path, false) shouldBe false
      gitIgnore5.isIgnored(path, false) shouldBe false
      gitIgnore6.isIgnored(path, false) shouldBe false
    }

    "process nested directory: project/target" in {
      val path = List("project", "target")
      gitIgnore1.isIgnored(path, true) shouldBe true
      gitIgnore2.isIgnored(path, true) shouldBe true
      gitIgnore3.isIgnored(path, true) shouldBe false
      gitIgnore4.isIgnored(path, true) shouldBe true
      gitIgnore5.isIgnored(path, true) shouldBe true
      gitIgnore6.isIgnored(path, true) shouldBe true

      GitIgnore("*").isIgnored(path, true) shouldBe true
      GitIgnore("*?").isIgnored(path, true) shouldBe true
      GitIgnore("?*").isIgnored(path, true) shouldBe true
      GitIgnore("?*?").isIgnored(path, true) shouldBe true
      GitIgnore("targ*").isIgnored(path, true) shouldBe true
      GitIgnore("*arget").isIgnored(path, true) shouldBe true
      GitIgnore("target*").isIgnored(path, true) shouldBe true
      GitIgnore("*target").isIgnored(path, true) shouldBe true
      GitIgnore("tar*get").isIgnored(path, true) shouldBe true
      GitIgnore("targ*/").isIgnored(path, true) shouldBe true
      GitIgnore("*arget/").isIgnored(path, true) shouldBe true
      GitIgnore("target*/").isIgnored(path, true) shouldBe true
      GitIgnore("*target/").isIgnored(path, true) shouldBe true
      GitIgnore("tar*get/").isIgnored(path, true) shouldBe true
      GitIgnore("/targ*").isIgnored(path, true) shouldBe false
      GitIgnore("/*arget").isIgnored(path, true) shouldBe false
      GitIgnore("/target*").isIgnored(path, true) shouldBe false
      GitIgnore("/*target").isIgnored(path, true) shouldBe false
      GitIgnore("/tar*get").isIgnored(path, true) shouldBe false
      GitIgnore("/targ*/").isIgnored(path, true) shouldBe false
      GitIgnore("/*arget/").isIgnored(path, true) shouldBe false
      GitIgnore("/target*/").isIgnored(path, true) shouldBe false
      GitIgnore("/*target/").isIgnored(path, true) shouldBe false
      GitIgnore("/tar*get/").isIgnored(path, true) shouldBe false

      GitIgnore("*rg.t").isIgnored(path, true) shouldBe false
      GitIgnore("*trget").isIgnored(path, true) shouldBe false
      GitIgnore("targets*").isIgnored(path, true) shouldBe false
      GitIgnore("ar*get").isIgnored(path, true) shouldBe false
      GitIgnore("*rg.t/").isIgnored(path, true) shouldBe false
      GitIgnore("*trget/").isIgnored(path, true) shouldBe false
      GitIgnore("targets*/").isIgnored(path, true) shouldBe false
      GitIgnore("ar*get/").isIgnored(path, true) shouldBe false
      GitIgnore("/*rg.t").isIgnored(path, true) shouldBe false
      GitIgnore("/*trget").isIgnored(path, true) shouldBe false
      GitIgnore("/targets*").isIgnored(path, true) shouldBe false
      GitIgnore("/ar*get").isIgnored(path, true) shouldBe false
      GitIgnore("/*rg.t/").isIgnored(path, true) shouldBe false
      GitIgnore("/*trget/").isIgnored(path, true) shouldBe false
      GitIgnore("/targets*/").isIgnored(path, true) shouldBe false
      GitIgnore("/ar*get/").isIgnored(path, true) shouldBe false

      GitIgnore("project?target").isIgnored(path, true) shouldBe false
      GitIgnore("project*target").isIgnored(path, true) shouldBe false
      GitIgnore("projec*arget").isIgnored(path, true) shouldBe false
      GitIgnore("?roject/targe*").isIgnored(path, true) shouldBe true

      GitIgnore("t?rget").isIgnored(path, true) shouldBe true
      GitIgnore("?arget").isIgnored(path, true) shouldBe true
      GitIgnore("targe?").isIgnored(path, true) shouldBe true
      GitIgnore("?arge?").isIgnored(path, true) shouldBe true
      GitIgnore("t??get").isIgnored(path, true) shouldBe true
      GitIgnore("t??ge?").isIgnored(path, true) shouldBe true
      GitIgnore("???ge?").isIgnored(path, true) shouldBe true
      GitIgnore("???g??").isIgnored(path, true) shouldBe true
      GitIgnore("/t?rget").isIgnored(path, true) shouldBe false
      GitIgnore("/?arget").isIgnored(path, true) shouldBe false
      GitIgnore("/targe?").isIgnored(path, true) shouldBe false
      GitIgnore("/?arge?").isIgnored(path, true) shouldBe false
      GitIgnore("/t??get").isIgnored(path, true) shouldBe false
      GitIgnore("/t??ge?").isIgnored(path, true) shouldBe false
      GitIgnore("/???ge?").isIgnored(path, true) shouldBe false
      GitIgnore("/???g??").isIgnored(path, true) shouldBe false
      GitIgnore("/t?rget/").isIgnored(path, true) shouldBe false
      GitIgnore("/?arget/").isIgnored(path, true) shouldBe false
      GitIgnore("/targe?/").isIgnored(path, true) shouldBe false
      GitIgnore("/?arge?/").isIgnored(path, true) shouldBe false
      GitIgnore("/t??get/").isIgnored(path, true) shouldBe false
      GitIgnore("/t??ge?/").isIgnored(path, true) shouldBe false
      GitIgnore("/???ge?/").isIgnored(path, true) shouldBe false
      GitIgnore("/???g??/").isIgnored(path, true) shouldBe false
      GitIgnore("t?rget/").isIgnored(path, true) shouldBe true
      GitIgnore("?arget/").isIgnored(path, true) shouldBe true
      GitIgnore("targe?/").isIgnored(path, true) shouldBe true
      GitIgnore("?arge?/").isIgnored(path, true) shouldBe true
      GitIgnore("t??get/").isIgnored(path, true) shouldBe true
      GitIgnore("t??ge?/").isIgnored(path, true) shouldBe true
      GitIgnore("???ge?/").isIgnored(path, true) shouldBe true
      GitIgnore("???g??/").isIgnored(path, true) shouldBe true
    }

    "process nested file: project/plugins.sbt" in {
      val path = List("project", "plugins.sbt")
      gitIgnore1.isIgnored(path, false) shouldBe false
      gitIgnore2.isIgnored(path, false) shouldBe false
      gitIgnore3.isIgnored(path, false) shouldBe false
      gitIgnore4.isIgnored(path, false) shouldBe false
      gitIgnore5.isIgnored(path, false) shouldBe false
      gitIgnore6.isIgnored(path, false) shouldBe false
      gitIgnore7.isIgnored(path, false) shouldBe true
    }

    "process nested file: src/main/resources/application.conf" in {
      val path = List("src", "main", "resources", "application.conf")
      gitIgnore1.isIgnored(path, false) shouldBe false
      gitIgnore2.isIgnored(path, false) shouldBe false
      gitIgnore3.isIgnored(path, false) shouldBe false
      gitIgnore4.isIgnored(path, false) shouldBe false
      gitIgnore5.isIgnored(path, false) shouldBe false
      gitIgnore6.isIgnored(path, false) shouldBe false
      gitIgnore7.isIgnored(path, false) shouldBe true

      GitIgnore("*").isIgnored(path, false) shouldBe true
      GitIgnore("/*").isIgnored(path, false) shouldBe true
      GitIgnore("*/").isIgnored(path, false) shouldBe true
      GitIgnore("/*/").isIgnored(path, false) shouldBe true
      GitIgnore("*.????").isIgnored(path, false) shouldBe true
      GitIgnore("/*.????").isIgnored(path, false) shouldBe false
      GitIgnore("*.????/").isIgnored(path, false) shouldBe false
      GitIgnore("/*.????/").isIgnored(path, false) shouldBe false
      GitIgnore("*.conf").isIgnored(path, false) shouldBe true
      GitIgnore("/*.conf").isIgnored(path, false) shouldBe false
      GitIgnore("*.conf/").isIgnored(path, false) shouldBe false
      GitIgnore("/*.conf/").isIgnored(path, false) shouldBe false
      GitIgnore("applic?????.conf").isIgnored(path, false) shouldBe true
      GitIgnore("/applic?????.conf").isIgnored(path, false) shouldBe false
      GitIgnore("applic?????.conf/").isIgnored(path, false) shouldBe false
      GitIgnore("/applic?????.conf/").isIgnored(path, false) shouldBe false

      GitIgnore("*.???").isIgnored(path, false) shouldBe false
      GitIgnore("/*.???").isIgnored(path, false) shouldBe false
      GitIgnore("*.???/").isIgnored(path, false) shouldBe false
      GitIgnore("/*.???/").isIgnored(path, false) shouldBe false

      GitIgnore("application*conf").isIgnored(path, false) shouldBe true
      GitIgnore("application?conf").isIgnored(path, false) shouldBe true
      GitIgnore("applicati*nf").isIgnored(path, false) shouldBe true
      GitIgnore("applicati?????nf").isIgnored(path, false) shouldBe true
      GitIgnore("/application*conf").isIgnored(path, false) shouldBe false
      GitIgnore("/application?conf").isIgnored(path, false) shouldBe false
      GitIgnore("/applicati*nf").isIgnored(path, false) shouldBe false
      GitIgnore("/applicati?????nf").isIgnored(path, false) shouldBe false
      GitIgnore("/application*conf/").isIgnored(path, false) shouldBe false
      GitIgnore("/application?conf/").isIgnored(path, false) shouldBe false
      GitIgnore("/applicati*nf/").isIgnored(path, false) shouldBe false
      GitIgnore("/applicati?????nf/").isIgnored(path, false) shouldBe false
      GitIgnore("application*conf/").isIgnored(path, false) shouldBe false
      GitIgnore("application?conf/").isIgnored(path, false) shouldBe false
      GitIgnore("applicati*nf/").isIgnored(path, false) shouldBe false
      GitIgnore("applicati?????nf/").isIgnored(path, false) shouldBe false

      GitIgnore("**/main").isIgnored(path, false) shouldBe true
      GitIgnore("**/main/resources/").isIgnored(path, false) shouldBe true
      GitIgnore("**/resources").isIgnored(path, false) shouldBe true
      GitIgnore("**/application.conf").isIgnored(path, false) shouldBe true
      GitIgnore("**/*.conf").isIgnored(path, false) shouldBe true
      GitIgnore("**/application.*").isIgnored(path, false) shouldBe true
      GitIgnore("**/*.conf").isIgnored(path, false) shouldBe true

      GitIgnore("**/test").isIgnored(path, false) shouldBe false
      GitIgnore("**/test/resources/").isIgnored(path, false) shouldBe false
      GitIgnore("**/scala").isIgnored(path, false) shouldBe false
      GitIgnore("**/resources.conf").isIgnored(path, false) shouldBe false
      GitIgnore("**/resources.*").isIgnored(path, false) shouldBe false
      GitIgnore("**/*.yaml").isIgnored(path, false) shouldBe false

      GitIgnore("src/**").isIgnored(path, false) shouldBe true
      GitIgnore("src/main/**").isIgnored(path, false) shouldBe true
      GitIgnore("src/main/resources/**").isIgnored(path, false) shouldBe true
      GitIgnore("src/test/**").isIgnored(path, false) shouldBe false
      GitIgnore("src/main/scala/**").isIgnored(path, false) shouldBe false

      GitIgnore("src/**/application.conf").isIgnored(path, false) shouldBe true
      GitIgnore("src/*/application.conf").isIgnored(path, false) shouldBe false
      GitIgnore("src/*/*/application.conf").isIgnored(path, false) shouldBe true
      GitIgnore("src/*/*/*.conf").isIgnored(path, false) shouldBe true
      GitIgnore("*/*/*/*.conf").isIgnored(path, false) shouldBe true
      GitIgnore("src/**/resources").isIgnored(path, false) shouldBe true
      GitIgnore("src/**/resources/application.conf").isIgnored(path, false) shouldBe true
      GitIgnore("main/**/*.conf").isIgnored(path, false) shouldBe false
      GitIgnore("src/main/**/*.conf").isIgnored(path, false) shouldBe true
      GitIgnore("test/**/*.conf").isIgnored(path, false) shouldBe false
      GitIgnore("src/test/**/*.conf").isIgnored(path, false) shouldBe false

      GitIgnore(Seq("*.conf", "!application.conf")).isIgnored(path, false) shouldBe false
      GitIgnore(Seq("*.conf", "!/src/**/application.conf")).isIgnored(path, false) shouldBe false
      GitIgnore(Seq("*.conf", "!**/application.conf")).isIgnored(path, false) shouldBe false
      GitIgnore(Seq("!application.conf", "*.conf")).isIgnored(path, false) shouldBe true
      GitIgnore(Seq("!/src/**/application.conf", "*.conf")).isIgnored(path, false) shouldBe true
      GitIgnore(Seq("!**/application.conf", "*.conf")).isIgnored(path, false) shouldBe true

      GitIgnore(Seq("!**/application.conf", "*.conf")).isIgnored(path, false) shouldBe true
      GitIgnore(Seq("src/main", "!*.conf")).isIgnored(path, false) shouldBe true
      GitIgnore(Seq("src/", "!*.conf")).isIgnored(path, false) shouldBe true
      GitIgnore(Seq("/src", "!*.conf")).isIgnored(path, false) shouldBe true
      GitIgnore(Seq("/src/", "!*.conf")).isIgnored(path, false) shouldBe true
      GitIgnore(Seq("src", "!src/main/resources")).isIgnored(path, false) shouldBe true
      GitIgnore(Seq("/src", "!src/main/resources")).isIgnored(path, false) shouldBe true
      GitIgnore(Seq("src/", "!src/main/resources")).isIgnored(path, false) shouldBe true
      GitIgnore(Seq("/src/", "!src/main/resources")).isIgnored(path, false) shouldBe true
      GitIgnore(Seq("src", "!src/main/*")).isIgnored(path, false) shouldBe true
      GitIgnore(Seq("/src", "!src/main/*")).isIgnored(path, false) shouldBe true
      GitIgnore(Seq("src/", "!src/main/*")).isIgnored(path, false) shouldBe true
      GitIgnore(Seq("/src/", "!src/main/*")).isIgnored(path, false) shouldBe true
      GitIgnore(Seq("src", "!*")).isIgnored(path, false) shouldBe false
      GitIgnore(Seq("/src", "!*")).isIgnored(path, false) shouldBe false
      GitIgnore(Seq("src/", "!*")).isIgnored(path, false) shouldBe false
      GitIgnore(Seq("/src/", "!*")).isIgnored(path, false) shouldBe false
      GitIgnore(Seq("src", "!**/")).isIgnored(path, false) shouldBe true
      GitIgnore(Seq("/src", "!**/")).isIgnored(path, false) shouldBe true
      GitIgnore(Seq("src/", "!**/")).isIgnored(path, false) shouldBe true
      GitIgnore(Seq("/src/", "!**/")).isIgnored(path, false) shouldBe true
      GitIgnore(Seq("src", "!**/*")).isIgnored(path, false) shouldBe false
      GitIgnore(Seq("src", "!/**/*")).isIgnored(path, false) shouldBe true
      GitIgnore(Seq("/src", "!**/*")).isIgnored(path, false) shouldBe false
      GitIgnore(Seq("/src", "!/**/*")).isIgnored(path, false) shouldBe true
      GitIgnore(Seq("src/", "!**/*")).isIgnored(path, false) shouldBe false
      GitIgnore(Seq("src/", "!/**/*")).isIgnored(path, false) shouldBe true
      GitIgnore(Seq("/src/", "!**/*")).isIgnored(path, false) shouldBe false
      GitIgnore(Seq("/src/", "!/**/*")).isIgnored(path, false) shouldBe true
      GitIgnore(Seq("/src/", "!/src/")).isIgnored(path, false) shouldBe false
      GitIgnore(Seq("/src/", "!src/")).isIgnored(path, false) shouldBe false
      GitIgnore(Seq("/src/", "!/src")).isIgnored(path, false) shouldBe false
      GitIgnore(Seq("/src/", "!src")).isIgnored(path, false) shouldBe false
      GitIgnore(Seq("/src/", "!src", "/src/main")).isIgnored(path, false) shouldBe true
      GitIgnore(Seq("/src/", "!src", "**/main")).isIgnored(path, false) shouldBe true
      GitIgnore(Seq("/src/", "!src", "**/main", "!main")).isIgnored(path, false) shouldBe false
      GitIgnore(Seq("**/", "!main")).isIgnored(path, false) shouldBe false
      GitIgnore(Seq("**/*", "!main")).isIgnored(path, false) shouldBe true
      GitIgnore(Seq("/**/*", "!main")).isIgnored(path, false) shouldBe false
      GitIgnore(Seq("/src/", "!src", "**/*", "!main")).isIgnored(path, false) shouldBe true
      GitIgnore(Seq("/src/", "!src", "/**/*", "!main")).isIgnored(path, false) shouldBe false
      GitIgnore(Seq("*/", "!main")).isIgnored(path, false) shouldBe true
      GitIgnore(Seq("*/*", "!main")).isIgnored(path, false) shouldBe false
      GitIgnore(Seq("/src/", "!src", "*/*", "!main")).isIgnored(path, false) shouldBe false
    }

    "process nested file: foo/bar/baz/[*?].txt" in {
      val path = List("foo", "bar", "baz", "[*?].txt")
      gitIgnore1.isIgnored(path, false) shouldBe false
      gitIgnore2.isIgnored(path, false) shouldBe false
      gitIgnore3.isIgnored(path, false) shouldBe false
      gitIgnore4.isIgnored(path, false) shouldBe false
      gitIgnore5.isIgnored(path, false) shouldBe false
      gitIgnore6.isIgnored(path, false) shouldBe false
      gitIgnore7.isIgnored(path, false) shouldBe false

      GitIgnore("*.txt").isIgnored(path, false) shouldBe true
      GitIgnore("\\[\\*\\?].txt").isIgnored(path, false) shouldBe true
      GitIgnore("\\[??].txt").isIgnored(path, false) shouldBe true
      GitIgnore("\\[\\??].txt").isIgnored(path, false) shouldBe false
      GitIgnore("\\[*\\?].txt").isIgnored(path, false) shouldBe true
      GitIgnore("\\[*].txt").isIgnored(path, false) shouldBe true
      GitIgnore("[*].txt").isIgnored(path, false) shouldBe false

      GitIgnore("**/\\[\\*\\?].txt").isIgnored(path, false) shouldBe true
      GitIgnore("foo/**/\\[\\*\\?].txt").isIgnored(path, false) shouldBe true
      GitIgnore("foo/bar/**/\\[\\*\\?].txt").isIgnored(path, false) shouldBe true
      GitIgnore("foo/**/baz/\\[\\*\\?].txt").isIgnored(path, false) shouldBe true
      GitIgnore("foo/**/baz/**").isIgnored(path, false) shouldBe true
      GitIgnore("foo/*/baz/*").isIgnored(path, false) shouldBe true
      GitIgnore("*/*/baz/*").isIgnored(path, false) shouldBe true
      GitIgnore("*/*/*/*").isIgnored(path, false) shouldBe true
      GitIgnore("foo/*/*").isIgnored(path, false) shouldBe true
      GitIgnore("**/foo/**").isIgnored(path, false) shouldBe true
      GitIgnore("**/bar/**").isIgnored(path, false) shouldBe true
      GitIgnore("**/baz/**").isIgnored(path, false) shouldBe true

      GitIgnore("**/abc/**").isIgnored(path, false) shouldBe false
      GitIgnore("**/abc").isIgnored(path, false) shouldBe false
      GitIgnore("abc/**").isIgnored(path, false) shouldBe false

      GitIgnore
        .parse("""
          |#foo   
          |\[\*\?].txt   
          |""".stripMargin)
        .isIgnored(path, false) shouldBe true

      GitIgnore
        .parse("""
          |#foo   
          |\[\*\?].txt\   
          |""".stripMargin)
        .isIgnored(path, false) shouldBe false

      GitIgnore
        .parse("""
          |#foo   
          |
          |#bar
          |
          |#baz
          |    
          |""".stripMargin)
        .isIgnored(path, false) shouldBe false

      GitIgnore
        .parse("""
          |#foo   
          |
          |#bar
          |\ 
          |#baz
          |    
          |""".stripMargin)
        .isIgnored(path, false) shouldBe false

      GitIgnore
        .parse("""
          |#foo   
          |
          |#bar
          |\[\*\?].* 
          |#baz
          |    
          |""".stripMargin)
        .isIgnored(path, false) shouldBe true
    }
  }

}
