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
	
	
	// selector functions
	val f_name: Emit.Selector = _ map {
		case "1" => "midroll"
		case "2" => "overlay"
		case "3" => "preroll"
		case "4" => "postroll"
		case "5" => "companion_banner"
		case "6" => "inskin"
		case "7" => "splash"
		case _   => "unknown"
	}

	val e_name: Emit.Selector = _ map {
		case  "0" => "impression"
		case "20" => "click_through"
		case "90" => "companion_impression"
		case "92" => "companion_click_through"
		case "13" => "clip_start"
		case "14" => "ad_start"
		case "15" => "ad_first_quartile"
		case "16" => "ad_midpoint"
		case "17" => "ad_third_quartile"
		case "19" => "ad_complete"
		case "21" => "slot_start"
		case "22" => "slot_end"
		case _   => "unknown"
	}

	val hour: Emit.Selector = _ map {
		case Long(ms) => sdf.format(ms)
		case _ => error("not a number")
	}

	def splitcat(d:String): List[String] => List[String] = { case x:List[String] => cat(d)(split(x)) }
	
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

	override def main(args: Array[String]): Unit = {
		// transform rule definition
		val loggy = transform.rule
		expand ~ $event{e_name} + "," + $ts{hour} + "," + $ad + "," + $format{f_name} + "," + $rsa{splitcat("/")} + "," + $tags{splitcat("/")}  in loggy

		println("event,time,ad,format,shares,tags")

		val app = new App(loggy)
		for( line <- io.Source.stdin.getLines ) {
		    app process line
		}
	}
}