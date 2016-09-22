name := "sparkRocks"
version := "1.0"
scalaVersion := "2.11.8"

scalacOptions ++= Seq("-feature")

resolvers += "Typesafe Repo" at "http://repo.typesafe.com/typesafe/releases/"
resolvers += "Sonatype Snapshots" at "https://oss.sonatype.org/content/repositories/snapshots"
resolvers += "Sonatype Releases" at "https://oss.sonatype.org/content/repositories/releases"
resolvers += Resolver.sonatypeRepo("public")

libraryDependencies += "com.typesafe.play" %% "play-json" % "2.3.8"
libraryDependencies += "org.scalanlp" %% "breeze" % "0.11.2"
libraryDependencies += "org.scalanlp" %% "breeze-natives" % "0.11.2"
libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.0" % "test"
libraryDependencies += "com.github.scopt" %% "scopt" % "3.5.0"
libraryDependencies += "org.apache.commons" % "commons-lang3" % "3.4"
libraryDependencies += "org.apache.commons" % "commons-math3" % "3.6.1"

// Spark pulls in dependencies that screw up sbt-assembly
libraryDependencies += "org.apache.spark" %% "spark-core" % "2.0.0" % "provided"
