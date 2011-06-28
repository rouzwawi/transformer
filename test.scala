:silent

import org.rouzwawi.transformer._
import org.rouzwawi.transformer.MapParser._
import org.rouzwawi.transformer.expand._
import org.rouzwawi.transformer.selectors._

// TODO: remember to remove or validate in some other way, empty keys in data

object Long {
	def unapply(s : String) : Option[Long] = try {
		Some(s.toLong)
	} catch {
		case _ : java.lang.NumberFormatException => None
	}
}

// selector functions
def splitcat(d:String): List[String] => List[String] = { case x:List[String] => cat(d)(split(x)) }


// parameter definitions
val $a = param("a")
val $b = param("b")

// transform rule definition
val loggy = transform rule
expand ~ $a + "," + $b{splitcat("/")} in loggy

val app = new App(loggy)
for( line <- io.Source.stdin.getLines ) {
    if (!line.isEmpty) app process line
}
