package org.rouzwawi.transformer

/**
 * Emit objects
 *
 * Each emit has a list of Terms
 * Variable terms select some values from a key/name in the data according to a selector
 * Literal terms just evaluate to their literal value
 *
 * All permutations of the selected values will be emitted
 */

trait Term
case class Variable( val name: String, val selector: Emit.Selector ) extends Term
case class Literal( val value: String ) extends Term

case object Emit {
	type Data = Map[String, List[String]]
	type Selector = List[String] => List[String]

	def cat(data: Data, terms: List[Term])(value: String) = expand(data, terms) match {
		case x :: xs => (x :: xs) map { value + _ }
		case Nil     => value :: Nil
	}

	def expand(data: Data, terms: List[Term]): List[String] = terms match {
		case t :: ts => t match {
			case Variable(name, selector) => data get name match {
				case Some(values) => selector apply values flatMap cat(data, ts)
				case None => Nil
			}
			case Literal(value) => cat(data, ts)(value)
		}
		case Nil => Nil
	}
}

case class Emit( val terms: List[Term] ) {
	def emit(data: Emit.Data) = Emit.expand(data, terms)
}
