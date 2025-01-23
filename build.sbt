def scalacOptionsVersion(scalaVersion: String): Seq[String] = {
  Seq() ++ {
    CrossVersion.partialVersion(scalaVersion) match {
      case Some((2, scalaMajor: Long)) if scalaMajor < 12 => Seq()
      case _ => Seq("-Xsource:2.11")
    }
  }
}

def javacOptionsVersion(scalaVersion: String): Seq[String] = {
  Seq() ++ {
    CrossVersion.partialVersion(scalaVersion) match {
      case Some((2, scalaMajor: Long)) if scalaMajor < 12 =>
        Seq("-source", "1.8", "-target", "1.8")
      case _ =>
        Seq("-source", "1.8", "-target", "1.8")
    }
  }
}

name := "chisel-fft"
version := "0.1.0"
scalaVersion := "2.12.13"

resolvers ++= Seq(
  Resolver.sonatypeRepo("snapshots"),
  Resolver.sonatypeRepo("releases")
)

libraryDependencies ++= Seq(
  "edu.berkeley.cs" %% "chisel3" % "3.5.6",
  "edu.berkeley.cs" %% "chiseltest" % "0.5.6",
  "org.scala-lang" % "scala-reflect" % scalaVersion.value,
  "com.github.scopt" %% "scopt" % "3.7.1",
  "edu.berkeley.cs" %% "chisel-iotesters" % "2.5.6",
  "org.fusesource.jansi" % "jansi" % "1.11"
)

scalacOptions ++= Seq(
  "-deprecation",
  "-encoding", "UTF-8",
  "-feature",
  "-unchecked",
  "-language:reflectiveCalls"
)

addCompilerPlugin("edu.berkeley.cs" % "chisel3-plugin" % "3.5.6" cross CrossVersion.full)
