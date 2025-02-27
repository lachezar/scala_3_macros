scalaVersion := "3.6.3"

ThisBuild / scalacOptions ++= Seq(
  "-Xprint:postInlining",
  "-Xmax-inlines:10000"
)
