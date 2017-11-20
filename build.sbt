name         := "mco2"
version      := "2.0.0"

inThisBuild(Seq(
  scalaVersion := "2.12.3"
))

resolvers += "JitPack" at "https://jitpack.io"

libraryDependencies ++= Seq(
  "org.scalaz" %% "scalaz-core" % "7.2.15",
  "com.github.julien-truffaut" %% "monocle-core" % "1.4.0",
  "com.github.julien-truffaut" %% "monocle-macro" % "1.4.0",
  "io.monix" %% "monix" % "2.3.0",
  "io.monix" %% "monix-scalaz-72" % "2.3.0",
  "com.github.oleg-py" % "macro-forwarders" % "0.1.1",
  "com.github.pathikrit" %% "better-files" % "3.1.0",
  "net.openhft" % "zero-allocation-hashing" % "0.8",
  "org.scalafx" %% "scalafx" % "8.0.144-R12",
  "com.lihaoyi" %% "pprint" % "0.5.3",
  "com.github.alexarchambault" %% "scalacheck-shapeless_1.13" % "1.1.7" % Test,
  "org.scalatest" %% "scalatest" % "3.0.1" % Test,
  "org.scalacheck" %% "scalacheck" % "1.13.4" % Test
)

fork := true

addCompilerPlugin("org.spire-math" %% "kind-projector" % "0.9.4")
addCompilerPlugin("org.scalameta" % "paradise" % "3.0.0-M10" cross CrossVersion.patch)

sourceDirectory   in Compile := baseDirectory.value / "src"
sourceDirectory   in Test    := baseDirectory.value / "test"
resourceDirectory in Compile := baseDirectory.value / "resources"
resourceDirectory in Test    := baseDirectory.value / "fixtures"

scalaSource in Compile := (sourceDirectory in Compile).value
scalaSource in Test    := (sourceDirectory in Test).value

// WORKAROUND https://github.com/scalameta/paradise/issues/10
scalacOptions in (Compile, console) ~= (_ filterNot (_ contains "paradise"))
scalacOptions in Compile ~= (_ filterNot (_ contains "unused"))
//scalacOptions in Compile += "-Xlog-implicits"
