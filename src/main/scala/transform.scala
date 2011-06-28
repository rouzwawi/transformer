import java.text.SimpleDateFormat


import org.rouzwawi.transformer._
import org.rouzwawi.transformer.MapParser._
import org.rouzwawi.transformer.expand._
import org.rouzwawi.transformer.selectors._

object tapp extends Application {

	object Long {
		def unapply(s : String) : Option[Long] = try {
			Some(s.toLong)
		} catch {
			case _ : java.lang.NumberFormatException => None
		}
	}

	val sdf = new SimpleDateFormat("yyyy-MM-dd HH:00")
	
	val hour: Emit.Selector = _ map {
		case Long(ms) => sdf.format(ms)
		case _ => error("not a number")
	}

	def splitcat(d:String): List[String] => List[String] = { case x:List[String] => cat(d)(split(x)) }
	
	// parameter definitions
	val $a = param("a")
	val $b = param("b")

	override def main(args: Array[String]): Unit = {
		if (args.length < 1) {
			println("run with:")
			println("  <input>")
			return
		}

		// transform rule definition
		val loggy = transform.rule
		expand ~ $a + "," + $b{splitcat("/")} in loggy

		val app = new App(loggy)
		for( line <- scala.io.Source.fromFile(args(1)).getLines ) {
		    if (!line.isEmpty) app process line
		}
	}
}