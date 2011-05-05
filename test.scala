import org.rouzwawi.transformer._

// TODO: remember to remove or validate in some other way, empty keys in data


// selector functions
val one: List[String] => List[String] = {
	case s :: ss => List(s)
	case Nil => Nil
}
val all: List[String] => List[String] = {
	case l: List[String] => l
}

// data
val d = Map(
	"sid"  -> List("site1"), 
	"ts"   -> List("1303933032"),
	"tags" -> List("tag1","tag2","tag3"),
	"a"    -> List("ad1"),
	"e"    -> List("0"),
	"t"    -> List("3")
)

case class Expr(val terms: List[Term]) {
 def +(v: String) = Expr(Literal(v) :: terms)
 def ->(func: List[String] => List[String]) = terms match {
  case Literal(v) :: ts => Expr(Variable(v, func) :: ts)
  case _ => throw new Exception()
 }
 def emit(data: Emit.D) = Emit(terms.reverse) emit data
}

object expand {
 def ~(name: String) = Expr(Literal(name) :: Nil)
}
import expand._


// example emits

// expand site[$sid]-tags[$tags[*]]
val e0 = Emit( Literal("site[") :: Variable("sid", one) :: Literal("]-tags[") :: Variable("tags", all) :: Literal("]") :: Nil )

// expand tags[$tags]
val e1 = Emit( Literal("tags[") :: Variable("tags", one) :: Literal("]") :: Nil )

// expand foo[$missing]
val e2 = Emit( Literal("foo[") :: Variable("missing", one) :: Literal("]") :: Nil ) // bug, emits "foo[", should not emit anything


// example scope, using functions

// scope $sid/v$$week($ts)
//val scope = Emit( Variable("sid", one) :: Literal("/v") :: Function(week, Variable("ts", one)) :: Nil )
val scope = Emit( Variable("sid", one) :: Literal("/v") :: Variable("ts", one) :: Nil )
