package org.rouzwawi.transformer

import org.rouzwawi.transformer.MapParser._
import org.rouzwawi.transformer.selectors._

import akka.actor.{Actor, ActorRef}
import akka.actor.Actor.actorOf
import akka.dispatch._


class App(val rule: rule) {
	val as = 8
	val actors = new Array[ActorRef](as)
	for (an <- Range(0,as)) actors(an) = actorOf(new MyActor(rule)).start

	def process(line: String) = {
		var a = 0
		actors(a) ! line
		a += 1
		a %= as
	}
	
	def shutdown = Actor.registry.shutdownAll
}

object MyActor {
	val dispatcher = new ExecutorBasedEventDrivenDispatcher("pool", 5, BoundedMailbox(true, 1000))
	val output = actorOf[Output].start
}

class MyActor(val rule: rule) extends Actor {
	self.dispatcher = MyActor.dispatcher
	
	val map = Map(
		"e" -> List("18"),
		"t" -> List("3"),
		"tags" -> List("news", "csp+", "sp_all"),
		"ts" -> List("1304498977042"),
		"rs" -> List("SHARE:5ad0d4e6-221d-47bb-9770-0ed889877966", "SHARE:772476a6-a240-4d95-b0a4-f1cc6032ee25"),
		"a" -> List("775faa89-2707-4d74-b909-ddabaa43b74a"),
		"sid" -> List("01200b57-68a3-469f-9ae7-ce1d75d4e577")
	)

	def receive = {
		case line:String => try {
			val x = (rule emit line)
			MyActor.output ! x
			//val y = rule emit map
			//MyActor.output ! y
		} catch {
			case _ => System.err.println("no no no")
		}
		case _     => System.err.println("received unknown message")
	}
}

class Output extends Actor {
	def receive = {
		case l: List[String] => {
			l foreach println
			println
		}
		case _ => Unit
	}
}
