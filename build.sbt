
name := "fp-practice"
scalaVersion := "2.12.3"

fork in run := true

scalacOptions ++= Seq(
  "-deprecation",
  "-encoding", "UTF-8",
  "-feature",
  "-language:existentials",
  "-language:higherKinds",
  "-language:implicitConversions",
  "-language:postfixOps",
  "-unchecked",
  "-Xlint",
  "-Yno-adapted-args",
  "-Ywarn-dead-code",
  "-Ywarn-numeric-widen",
  "-Ywarn-value-discard",
  "-Xfuture")

libraryDependencies ++= Seq(
 "ch.qos.logback"        %   "logback-classic"        % "1.2.1",
 "com.github.pureconfig" %%  "pureconfig"             % "0.7.1",
  "org.scalatest"        %% "scalatest"               % "3.0.1"  % "test"
)

