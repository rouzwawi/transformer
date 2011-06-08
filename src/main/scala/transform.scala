import java.util.TimeZone
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
		case "18" => "ad_complete"
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
		if (args.length < 4) {
			println("run with:")
			println("  <timezone> <relations> <shares> <input>")
			return
		}

		if (TimeZone.getAvailableIDs exists { _ == args(0) }) {
			sdf.setTimeZone(TimeZone.getTimeZone(args(0)))
		} else {
			println("Timezone " + args(0) + " does not exits")
			println("Available timezones:")
			TimeZone.getAvailableIDs map (_ + " ") map print
			return
		}

		// relations
		val A = 0
		val G = 1
		val C = 2
		val AN = 3
		val GN = 4
		val CN = 5
		val ag = new scala.collection.mutable.HashMap[String, String]
		val ac = new scala.collection.mutable.HashMap[String, String]
		val an = new scala.collection.mutable.HashMap[String, String]
		val gn = new scala.collection.mutable.HashMap[String, String]
		val cn = new scala.collection.mutable.HashMap[String, String]
		for( line <- scala.io.Source.fromFile(args(1)).getLines ) {
			val rel = line split "\t"
			if (rel.length != 6) {
				println("error in relations line")
				println("    " + line)
				return;
			}
			ag(rel(A)) = rel(G)
			ac(rel(A)) = rel(C)
			an(rel(A)) = rel(AN).replaceAll("/", " ")
			gn(rel(A)) = rel(GN).replaceAll("/", " ")
			cn(rel(A)) = rel(CN).replaceAll("/", " ")
		}

		// shares
		val S = 0
		val SG = 1
		val SP = 2
		val SN = 3
		val sg = new scala.collection.mutable.HashMap[String, String]
		val sp = new scala.collection.mutable.HashMap[String, String]
		val sn = new scala.collection.mutable.HashMap[String, String]
		for( line <- scala.io.Source.fromFile(args(2)).getLines ) {
			val shr = line split "\t"
			if (shr.length != 4) {
				println("error in shares line")
				println("    " + line)
				return;
			}
			sg("SHARE:" + shr(S)) = shr(SG)
			sp("SHARE:" + shr(S)) = "SHARE:" + shr(SP)
			sn("SHARE:" + shr(S)) = shr(SN).replaceAll("/", " ")
		}

		// ad selector
		val ad_path: Emit.Selector = {
			case ad :: xs  => List(
				cn.getOrElse(ad, "no campaign") + "/" + 
				gn.getOrElse(ad, "no goal") + "/" + 
				an.getOrElse(ad, "no ad")	
			)
			case Nil => List("unknown")
		}

		def sh(s: Option[String]): List[String] = s match {
			case Some(id) => sn get id match {
				case Some(name) => name :: sh(sp get id)
				case None => Nil
			}
			case None => Nil
		}

		// share selector
		def share_path(sharegroup: String): Emit.Selector = _ find { sg.get(_) == Some(sharegroup) } match {
			case Some(share) => List( sh(Option(share)).reverse.reduceLeft( _ + "/" + _ ) )
			case None => List("unknown")
		}

		// transform rule definition
		val loggy = transform.rule
		expand ~ 
		$event{e_name}						 + "," + 
		$ts{hour}							 + "," + 
		$ad{ad_path}						 + "," + 
		$format{f_name}						 + "," + 
		$rs{share_path("Sites")}			 + "," + 
		$rs{share_path("Content partners")}	 + "," + 
		$tags{splitcat("/")}				 in loggy

		//println("event,time,ad,format,shares,tags")

		val app = new App(loggy)
		for( line <- scala.io.Source.fromFile(args(3)).getLines ) {
		    app process line
		}
	}
}