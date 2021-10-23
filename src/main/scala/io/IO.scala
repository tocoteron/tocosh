import java.io._

trait IO:
  def stdin: InputStream
  def stdout: OutputStream
  def stderr: OutputStream
  def stdinReader = BufferedReader(InputStreamReader(this.stdin))
  def stdoutWriter = BufferedWriter(OutputStreamWriter(this.stdout))
  def stderrWriter = BufferedWriter(OutputStreamWriter(this.stderr))

case class ShellIO(
  stdin: InputStream,
  stdout: OutputStream,
  stderr: OutputStream,
) extends IO

case class PipedShellIO(
  stdin: PipedInputStream,
  stdout: PipedOutputStream,
  stderr: PipedOutputStream,
) extends IO
