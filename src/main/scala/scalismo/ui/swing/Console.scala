package scalismo.ui.swing

import java.io.PrintStream
import java.util.concurrent.{ Executors, ThreadFactory }
import javax.swing.{ JSplitPane, SwingConstants }

import de.sciss.scalainterpreter._
import scalismo.ui.EdtPublisher
import scalismo.ui.swing.util.MultiOutputStream

import scala.concurrent.ExecutionContext
import scala.swing.event.Event
import scala.swing.{ BorderPanel, Component, Frame }

class ConsolePanel(imports: List[String] = List())(implicit frame: ScalismoFrame) extends BorderPanel {
  val icfg = Interpreter.Config()
  icfg.imports = icfg.imports ++ (imports :+ "scalismo.ui._")
  icfg.bindings ++= Seq(NamedParam("frame", frame)).toIndexedSeq

  val codeCfg = CodePane.Config()
  codeCfg.style = Style.BlueForest
  val split = MSplitPane(InterpreterPane.Config().build, icfg.build, codeCfg.build)
  split.component.setResizeWeight(0.5)
  layout(Component.wrap(split.component)) = BorderPanel.Position.Center
}

class ConsoleFrame(parent: Console)(implicit frame: ScalismoFrame) extends Frame {
  title = "Scalismo Console"
  contents = new ConsolePanel()

  override def closeOperation() = {
    // this will take care of sending the appropriate message
    parent.visible = false
  }
}

object Console {

  case class VisibilityChanged(source: Console) extends Event

}

class Console(implicit iframe: ScalismoFrame) extends EdtPublisher {
  lazy val frame = new ConsoleFrame(this)
  private var needToDispose = false

  private var _visible = false

  def visible = _visible

  def visible_=(v: Boolean) = {
    if (_visible != v) {
      _visible = v
      needToDispose = true
      frame.visible = v
      publishEdt(Console.VisibilityChanged(this))
    }
  }

  def dispose() = {
    if (needToDispose) {
      frame.dispose()
    }
  }
}

object MSplitPane {
  def apply(paneConfig: InterpreterPane.Config = InterpreterPane.Config().build,
    interpreterConfig: Interpreter.Config = Interpreter.Config().build,
    codePaneConfig: CodePane.Config = CodePane.Config().build): MSplitPane = {
    val fac = new ThreadFactory {
      def newThread(r: Runnable): Thread = {
        new Thread(r) {
          setDaemon(true)
        }
      }
    }
    val exec = ExecutionContext fromExecutorService Executors.newSingleThreadExecutor(fac)

    // FIXME: There must be a better way to do this.
    val sysout = System.out
    val syserr = System.err

    val lp = LogPane().makeDefault(error = true)

    val mout = new MultiOutputStream(System.out, sysout)
    val merr = new MultiOutputStream(System.err, syserr)
    System.setOut(new PrintStream(mout))
    System.setErr(new PrintStream(merr))

    val intCfg = Interpreter.ConfigBuilder(interpreterConfig)
    intCfg.out = Some(lp.writer)
    val ip = InterpreterPane(paneConfig, intCfg.build, codePaneConfig)(exec)
    val sp = new JSplitPane(SwingConstants.HORIZONTAL)
    sp.setTopComponent(ip.component.self)
    sp.setBottomComponent(lp.component.self)
    new Impl(sp, ip)
  }

  private final class Impl(val component: JSplitPane, val interpreter: InterpreterPane)
      extends MSplitPane {
    override def toString = "SplitPane@" + hashCode.toHexString
  }

}

sealed trait MSplitPane {
  def component: JSplitPane

  def interpreter: InterpreterPane
}
