package org.rouzwawi.transformer

import org.rouzwawi.transformer.MapParser._
import org.rouzwawi.transformer.expand._
import org.rouzwawi.transformer.selectors._

import akka.actor.{Actor, ActorRef}
import akka.actor.Actor.actorOf
import akka.dispatch._


object App extends Application {
	override def main (args: Array[String]) : Unit = {
		
		val as = 5
		var a = 0

		val actors = new Array[ActorRef](as)
		for (an <- Range(0,as)) actors(an) = actorOf[MyActor].start

		var n = 0
		for( line <- io.Source.stdin.getLines ) {
			actors(a) ! line
			a += 1
			a %= as
			n += 1
			n %= 1000
			if (n == 0) System.err.print(".")
		}
	}
}

 
object MyActor {
  val dispatcher = new ExecutorBasedEventDrivenDispatcher("pool", 5, BoundedMailbox(true, 1000))
}
 
class MyActor extends Actor {
	self.dispatcher = MyActor.dispatcher

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

	val $site = param("sid")
	val $ad = param("a")
	val $event = param("e")
	val $format = param("t")
	val $tags = param("tags")
	val $shares = param("rsa")
	val $ts = param("ts")

	val loggy = transform.rule
	expand ~ "site["     + $site           + "]" in loggy
	expand ~ "tag["      + $tags{split}    + "]" in loggy
	expand ~ "category[" + $shares         + "]" in loggy
	expand ~ "ad["       + $ad             + "]" in loggy
	expand ~ "event["    + $event{e_name}  + "]" in loggy
	expand ~ "format["   + $format{f_name} + "]" in loggy

	var n = 0

	def receive = {
		case line:String => try {
			loggy emit line map println
			n += 1
			n %= 1000
			if (n == 0) System.err.println("d")
		} catch {
			case _ => println("no no no")
		}
		case _           => println("received unknown message")
	}
}
