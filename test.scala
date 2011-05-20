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
val f_name: Emit.Selector = _ map {
	case "1" => "midroll"
	case "2" => "overlay"
	case "3" => "preroll"
	case _   => "unknown"
}

val e_name: Emit.Selector = _ map {
	case "0" => "impression"
	case _   => "unknown"
}

val week: Emit.Selector = _ map {
	case Long(ms) => ms / (7*24*60*60*1000L) toString  // kind of like this
	case _ => error("not a number")
}

// parameter definitions
val $site = param("sid")
val $ad = param("a")
val $event = param("e")
val $format = param("t")
val $tags = param("tags")
val $rs = param("rs")
val $rsa = param("rsa")
val $sh = param("shares")
val $ts = param("ts")

// transform rule definition
val loggy = transform rule
expand ~ $site + "," + $ad + "," + $rsa{cat("/")} in loggy

println("site, ad, shares")

val app = new App(loggy)
for( line <- io.Source.stdin.getLines ) {
    app process line
}
