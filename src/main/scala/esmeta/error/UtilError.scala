package esmeta.error

sealed abstract class UtilError(msg: String)
  extends ESMetaError(msg, "UtilError")

case class InvalidGitVersion(msg: String)
  extends UtilError(s"Invalid git version: $msg")

case class GitTagMismatch(hash: String, tagName: String)
  extends UtilError(s"Git tag mismatch: $hash != $tagName")

case object NoGraalError extends UtilError("no Graal engine")

case object TranspileFailureError extends UtilError("Trnaspile failure")

case class NoCommandError(command: String)
  extends UtilError(s"Command not found: $command")
