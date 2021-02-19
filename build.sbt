name := "sparkRocks"
version := "1.0"
scalaVersion := "2.12.13"

scalacOptions ++= Seq("-feature")

resolvers += "Typesafe Repo" at "https://repo.typesafe.com/typesafe/releases/"
resolvers += "Sonatype Snapshots" at "https://oss.sonatype.org/content/repositories/snapshots"
resolvers += "Sonatype Releases" at "https://oss.sonatype.org/content/repositories/releases"
resolvers += Resolver.sonatypeRepo("public")

libraryDependencies += "com.typesafe.play" %% "play-json" % "2.9.2"
libraryDependencies += "org.scalanlp" %% "breeze" % "1.1"
libraryDependencies += "org.scalanlp" %% "breeze-natives" % "1.1"
libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.0" % "test"
libraryDependencies += "com.github.scopt" %% "scopt" % "3.5.0"
libraryDependencies += "org.apache.commons" % "commons-lang3" % "3.4"
libraryDependencies += "org.apache.commons" % "commons-math3" % "3.6.1"

// Spark pulls in dependencies that screw up sbt-assembly
libraryDependencies += "org.apache.spark" %% "spark-core" % "3.0.1" % "provided"

assemblyMergeStrategy in assembly := {
 case PathList("META-INF", xs @ _*) => MergeStrategy.discard
 case x => MergeStrategy.first
}
