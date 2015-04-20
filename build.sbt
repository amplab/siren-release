name := "SiRen"

version := "0.0"

scalaVersion := "2.9.3"

libraryDependencies += "it.unimi.dsi" % "fastutil" % "6.3"

libraryDependencies += "org.scalatest" % "scalatest_2.10" % "1.9.2" % "test"

libraryDependencies += "org.apache.spark" % "spark-core_2.9.3" % "0.8.1-incubating"

libraryDependencies ++= Seq(
"org.eclipse.jetty.orbit" % "javax.servlet" % "3.0.0.v201112011016" artifacts (Artifact("javax.servlet", "jar", "jar")),
"org.eclipse.jetty.orbit" % "javax.transaction" % "1.1.1.v201105210645" artifacts (Artifact("javax.transaction", "jar", "jar")),
"org.eclipse.jetty.orbit" % "javax.mail.glassfish" % "1.4.1.v201005082020" artifacts (Artifact("javax.mail.glassfish", "jar", "jar")),
"org.eclipse.jetty.orbit" % "javax.activation" % "1.1.0.v201105071233" artifacts (Artifact("javax.activation", "jar", "jar")))

resolvers ++= Seq(
  "Typesafe Repository" at "http://repo.typesafe.com/typesafe/releases/",
  "Spray Repository" at "http://repo.spray.cc/",
  "Hadoop-BAM Repository" at "http://hadoop-bam.sourceforge.net/maven/",
  "Akka Repository" at "http://repo.akka.io/releases/")
