import sbt._, Keys._

lazy val root = (project in file(".")).settings(
  scalaVersion:="2.12.4",
  scalacOptions ++= Seq("-feature", "-deprecation", "-language:postfixOps", "-Ypartial-unification"),
  resolvers+="SOSSS" at "https://oss.sonatype.org/content/repositories/snapshots",
  testOptions in Test += Tests.Argument(TestFrameworks.ScalaCheck, "-verbosity", "1"),
  logBuffered := false,
  parallelExecution in Test := false,
  libraryDependencies:=Seq(
    "com.typesafe"      %  "config"             % "1.3.1",
    "org.typelevel"     %% "cats-core"          % "1.0.1",
    "org.typelevel"     %% "cats-effect"        % "0.5",
    "io.circe"          %% "circe-core"         % "0.9.0-M2",
    "io.circe"          %% "circe-generic"      % "0.9.0-M2",
    "io.circe"          %% "circe-literal"      % "0.9.0-M2",
    "org.http4s"        %% "http4s-circe"       % "0.18.0-M6",
    "org.http4s"        %% "http4s-dsl"         % "0.18.0-M6",
    "org.http4s"        %% "http4s-server"      % "0.18.0-M6",
    "org.http4s"        %% "http4s-blaze-server"% "0.18.0-M6",
    "org.tpolecat"      %% "atto-core"          % "0.6.1-M7",

    "org.scalatest"     %% "scalatest"          % "3.0.1"   % "test",
    "org.scalacheck"    %% "scalacheck"         % "1.13.5"  % "test" ))
