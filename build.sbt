name := "sparkRocks"
version := "1.0"
scalaVersion := "2.10.5"

scalacOptions ++= Seq("-feature")

resolvers += "Typesafe Repo" at "http://repo.typesafe.com/typesafe/releases/"
resolvers += "Sonatype Snapshots" at "https://oss.sonatype.org/content/repositories/snapshots"
resolvers += "Sonatype Releases" at "https://oss.sonatype.org/content/repositories/releases"

libraryDependencies += "com.typesafe.play" %% "play-json" % "2.3.8"
libraryDependencies += "org.scalanlp" %% "breeze" % "0.11.2"
libraryDependencies += "org.scalanlp" %% "breeze-natives" % "0.11.2"
libraryDependencies += "org.scalatest" % "scalatest_2.10" % "2.2.1" % "test"

// Spark pulls in dependencies that screw up sbt-assembly
libraryDependencies += "org.apache.spark" %% "spark-core" % "1.3.1" % "provided"
