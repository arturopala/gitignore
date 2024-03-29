[![Build and test](https://github.com/arturopala/gitignore/actions/workflows/build.yml/badge.svg)](https://github.com/arturopala/gitignore/actions/workflows/build.yml)
[![Maven Central](https://img.shields.io/maven-central/v/com.github.arturopala/gitignore_2.13.svg?label=Maven%20Central)](https://search.maven.org/search?q=g:%22com.github.arturopala%22%20AND%20a:%22gitignore_2.13%22)
[![Scala.js](https://www.scala-js.org/assets/badges/scalajs-1.12.0.svg)](https://www.scala-js.org)
![Code size](https://img.shields.io/github/languages/code-size/arturopala/gitignore)
![GitHub](https://img.shields.io/github/license/arturopala/gitignore)
![Lift](https://lift.sonatype.com/api/badge/github.com/arturopala/gitignore)


GitIgnore
===

This is a nano-library for Scala

    "com.github.arturopala" %% "gitignore" % "0.5.0"

    //> using "com.github.arturopala::gitignore:0.5.0"

Cross-compiles to Scala versions `2.13.11`, `2.12.18`, `3.3.0`, 
and ScalaJS version `1.13.1`, and ScalaNative version `0.4.14`.

Motivation
---

The [.gitignore](https://git-scm.com/docs/gitignore) has became de-facto standard filter format for project's files and folders. 

This nano-library provides Scala implementation of the `.gitignore` compliant filter, ready to embed in different tools without having to run `git` command.

Scaladoc
---

<https://arturopala.github.io/gitignore/latest/api/com/github/arturopala/gitignore/GitIgnore.html>

Try online in Scastie!
---

<https://scastie.scala-lang.org/arturopala/D8GzZ1LwTIuNr4wBOzcMUg/11>

Usage
---

Option **1** - parse existing `.gitignore` file content

```scala
import com.github.arturopala.gitignore._

val gitignore = GitIgnore
    .parse(""" 
        |#*.json
        |*.txt
        |*.pdf
        |!ok.*
        |bar.json 
        |target  
        |""".stripMargin)
// gitignore: GitIgnore = GitIgnore(
//   gitPatterns = List("*.txt", "*.pdf", "!ok.*", "bar.json", "target")
// )
 
gitignore.isIgnored("foo.txt")
// res0: Boolean = true
gitignore.isIgnored("bar.txt")
// res1: Boolean = true
gitignore.isIgnored("ok.txt")
// res2: Boolean = false
gitignore.isIgnored("foo.pdf")
// res3: Boolean = true
gitignore.isIgnored("bar.pdf")
// res4: Boolean = true
gitignore.isIgnored("ok.pdf")
// res5: Boolean = false
gitignore.isIgnored("foo.json")
// res6: Boolean = false
gitignore.isIgnored("bar.json")
// res7: Boolean = true
gitignore.isIgnored("ok.json")
// res8: Boolean = false
gitignore.isIgnored("target/")
// res9: Boolean = true
gitignore.isIgnored("target.json")
// res10: Boolean = false

gitignore.isAllowed("foo.txt")
// res11: Boolean = false
gitignore.isAllowed("bar.txt")
// res12: Boolean = false
gitignore.isAllowed("ok.txt")
// res13: Boolean = true
gitignore.isAllowed("foo.pdf")
// res14: Boolean = false
gitignore.isAllowed("bar.pdf")
// res15: Boolean = false
gitignore.isAllowed("ok.pdf")
// res16: Boolean = true
gitignore.isAllowed("foo.json")
// res17: Boolean = true
gitignore.isAllowed("bar.json")
// res18: Boolean = false
gitignore.isAllowed("ok.json")
// res19: Boolean = true
gitignore.isAllowed("target/")
// res20: Boolean = false
gitignore.isAllowed("target.json")
// res21: Boolean = true
```

Option **2** - pass list of rules to the constructor

```scala
import com.github.arturopala.gitignore._

val gitignore = GitIgnore(Seq(
    "*.txt",
    "*.pdf",
    "!ok.*",
    "bar.json ",
    "target"))
// gitignore: GitIgnore = GitIgnore(
//   gitPatterns = List("*.txt", "*.pdf", "!ok.*", "bar.json ", "target")
// )
 
gitignore.isIgnored("foo.txt")
// res23: Boolean = true
gitignore.isIgnored("bar.txt")
// res24: Boolean = true
gitignore.isIgnored("ok.txt")
// res25: Boolean = false
gitignore.isIgnored("foo.pdf")
// res26: Boolean = true
gitignore.isIgnored("bar.pdf")
// res27: Boolean = true
gitignore.isIgnored("ok.pdf")
// res28: Boolean = false
gitignore.isIgnored("foo.json")
// res29: Boolean = false
gitignore.isIgnored("bar.json")
// res30: Boolean = false
gitignore.isIgnored("ok.json")
// res31: Boolean = false
gitignore.isIgnored("target/")
// res32: Boolean = true
gitignore.isIgnored("target.json")
// res33: Boolean = false

gitignore.isAllowed("foo.txt")
// res34: Boolean = false
gitignore.isAllowed("bar.txt")
// res35: Boolean = false
gitignore.isAllowed("ok.txt")
// res36: Boolean = true
gitignore.isAllowed("foo.pdf")
// res37: Boolean = false
gitignore.isAllowed("bar.pdf")
// res38: Boolean = false
gitignore.isAllowed("ok.pdf")
// res39: Boolean = true
gitignore.isAllowed("foo.json")
// res40: Boolean = true
gitignore.isAllowed("bar.json")
// res41: Boolean = true
gitignore.isAllowed("ok.json")
// res42: Boolean = true
gitignore.isAllowed("target/")
// res43: Boolean = false
gitignore.isAllowed("target.json")
// res44: Boolean = true
```

Development
---

Compile

    sbt compile

Compile for all Scala versions

    sbt +compile

Test

    sbt rootJVM/test
    sbt rootJS/test
    sbt rootNative/test

Test with all Scala versions

    sbt +test
    sbt +rootJVM/test


Generate README and docs

    sbt docs/mdoc

Apply scalafixes

    sbt rootJMV/scalafixAll    

Github Actions
---

 - **Build**: runs on every push or pull request
 - **Release**: manual release of artefacts to Sonatype and Maven Central, for a setup follow <https://github.com/olafurpg/sbt-ci-release/blob/main/readme.md>
 - **Site**: manual update of README and push of API docs to Github Pages
