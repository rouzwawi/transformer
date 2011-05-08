package org.rouzwawi.transformer

case class param(val v: String) {
	def apply(f: Emit.Selector) = Variable(v, f)
}

class rule(var emits: List[Emit] = Nil) {
	def add(e: Emit) = {
		emits = e :: emits;
	}
	def set(e: Emit) = {
		emits = e :: Nil
	}
	def emit(d: Emit.Data) = emits flatMap { _ emit d }
}

object transform {
	def rule  = new rule
	def scope = new rule
}

object expand {
	def ~(name: String) = expr(Literal(name) :: Nil)
	def ~(term: Term)   = expr(term :: Nil)
	def ~(param: param) = expr(Variable(param.v, selectors.all) :: Nil)
	def %(name:String)  = expand ~ name
	def %(term: Term)   = expand ~ term
	def %(param: param) = expand ~ param
}

object selectors {
	val all: Emit.Selector = {
		case l: List[String] => l
	}
	val * = all
	val first: Emit.Selector = {
		case s :: ss => List(s)
		case Nil => Nil
	}
	val split: Emit.Selector = _ flatMap {
		case v => v.split(",")
	}
}

case class expr(val terms: List[Term]) {
	def apply(f: Emit.Selector) = ->(f)

	def +(v: String) = expr(Literal(v) :: terms)
	def +(t: Term)   = expr(t :: terms)
	def +(p: param)  = expr(Variable(p.v, selectors.all) :: terms)

	def ->(f: Emit.Selector) = terms match {
		case Literal(v) :: ts => expr(Variable(v, f) :: ts)
		case _ => throw new Exception()
	}

	def emit(data: Emit.Data) = Emit(terms.reverse) emit data

	def in(r: rule) = r add Emit(this.terms.reverse)
	def to(s: rule) = s set Emit(this.terms.reverse)
}
