[![Build and test](https://github.com/arturopala/gitignore/actions/workflows/build.yml/badge.svg)](https://github.com/arturopala/gitignore/actions/workflows/build.yml)
[![Maven Central](https://img.shields.io/maven-central/v/com.github.arturopala/gitignore_2.13.svg?label=Maven%20Central)](https://search.maven.org/search?q=g:%22com.github.arturopala%22%20AND%20a:%22gitignore_2.13%22)
[![Scala.js](https://www.scala-js.org/assets/badges/scalajs-1.12.0.svg)](https://www.scala-js.org)
![Code size](https://img.shields.io/github/languages/code-size/arturopala/gitignore)
![GitHub](https://img.shields.io/github/license/arturopala/gitignore)
![Lift](https://lift.sonatype.com/api/badge/github.com/arturopala/gitignore)


GitIgnore
===

This is a nano-library for Scala

    "com.github.arturopala" %% "gitignore" % "@VERSION@"

    //> using "com.github.arturopala::gitignore:@VERSION@"

Cross-compiles to Scala versions @SUPPORTED_SCALA_VERSIONS@, 
and ScalaJS version `@SCALA_JS_VERSION@`, and ScalaNative version `@SCALA_NATIVE_VERSION@`.

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

```scala mdoc
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
 
gitignore.isIgnored("foo.txt")
gitignore.isIgnored("bar.txt")
gitignore.isIgnored("ok.txt")
gitignore.isIgnored("foo.pdf")
gitignore.isIgnored("bar.pdf")
gitignore.isIgnored("ok.pdf")
gitignore.isIgnored("foo.json")
gitignore.isIgnored("bar.json")
gitignore.isIgnored("ok.json")
gitignore.isIgnored("target/")
gitignore.isIgnored("target.json")

gitignore.isAllowed("foo.txt")
gitignore.isAllowed("bar.txt")
gitignore.isAllowed("ok.txt")
gitignore.isAllowed("foo.pdf")
gitignore.isAllowed("bar.pdf")
gitignore.isAllowed("ok.pdf")
gitignore.isAllowed("foo.json")
gitignore.isAllowed("bar.json")
gitignore.isAllowed("ok.json")
gitignore.isAllowed("target/")
gitignore.isAllowed("target.json")
```

Option **2** - pass list of rules to the constructor

```scala mdoc:reset
import com.github.arturopala.gitignore._

val gitignore = GitIgnore(Seq(
    "*.txt",
    "*.pdf",
    "!ok.*",
    "bar.json ",
    "target"))
 
gitignore.isIgnored("foo.txt")
gitignore.isIgnored("bar.txt")
gitignore.isIgnored("ok.txt")
gitignore.isIgnored("foo.pdf")
gitignore.isIgnored("bar.pdf")
gitignore.isIgnored("ok.pdf")
gitignore.isIgnored("foo.json")
gitignore.isIgnored("bar.json")
gitignore.isIgnored("ok.json")
gitignore.isIgnored("target/")
gitignore.isIgnored("target.json")

gitignore.isAllowed("foo.txt")
gitignore.isAllowed("bar.txt")
gitignore.isAllowed("ok.txt")
gitignore.isAllowed("foo.pdf")
gitignore.isAllowed("bar.pdf")
gitignore.isAllowed("ok.pdf")
gitignore.isAllowed("foo.json")
gitignore.isAllowed("bar.json")
gitignore.isAllowed("ok.json")
gitignore.isAllowed("target/")
gitignore.isAllowed("target.json")
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
