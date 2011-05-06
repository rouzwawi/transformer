import org.rouzwawi.transformer._
import org.rouzwawi.transformer.expand._
import org.rouzwawi.transformer.selectors._

// TODO: remember to remove or validate in some other way, empty keys in data


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

// data
val d = Map(
	"sid"  -> List("eeffedf1-783e-4995-b28e-ac39665ce604"), 
	"ts"   -> List("1303933032"),
	"tags" -> List("newtv","sport","royal wedding"),
	"shrs" -> List("edb432dd","778dbda7"),
	"a"    -> List("uberad"),
	"e"    -> List("0"),
	"t"    -> List("3")
)

// parameter definitions
val $site = param("sid")
val $ad = param("a")
val $event = param("e")
val $format = param("t")
val $tags = param("tags")
val $shares = param("shrs")
val $ts = param("ts")


// transform rule definition
val loggy = transform rule
expand ~ "site["     + $site +           "]"    in loggy
expand ~ "tag["      + $tags +           "]"    in loggy
expand ~ "category[" + $shares +         "]"    in loggy
expand ~ "ad["       + $ad +             "]"    in loggy
expand ~ "event["    + $event{e_name} +  "]"    in loggy
expand ~ "format["   + $format{f_name} + "]"    in loggy


// combining transformation rule
val combos = transform rule
expand ~ "tag-cat[" + $shares + "/" + $tags + "]" in combos




















// example emits
var examples = transform rule

// expand site[$sid]-tags[$tags[*]]
expand ~ "site[" + $site + "]-tags[" + $tags{*} + "]" in examples

// expand tags[$tags]
expand ~ "tags[" + $tags + "]" in examples

// expand foo[$missing]
val $missing = param("missing")
expand ~ "foo[" + $missing + "]" in examples // bug, emits "foo[", should not emit anything


// scope $sid/v$$week($ts)
//val scope = Emit( Variable("sid", one) :: Literal("/v") :: Function(week, Variable("ts", one)) :: Nil )
val scope = transform scope
expand ~ $site + "/v" + $ts to scope
