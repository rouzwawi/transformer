package org.rouzwawi.transformer

import scala.util.matching.Regex


object MapParser {

	val nwl = """\n""";

	val   escapePatterns = nwl ::   """=""" ::   """,""" ::   """\[""" ::   """\]""" ::   """\|""" :: Nil
	val   escapeReplaces = ""  :: """\\=""" :: """\\,""" ::  """\\[""" ::  """\\]""" ::  """\\|""" :: Nil
	val unescapePatterns =        """\\=""" :: """\\,""" :: """\\\[""" :: """\\\]""" :: """\\\|""" :: Nil
	val unescapeReplaces =            "="   ::     ","   ::      "["   ::      "]"   ::      "|"   :: Nil

	val   escapes = (  escapePatterns map { new Regex(_) }) zip   escapeReplaces
	val unescapes = (unescapePatterns map { new Regex(_) }) zip unescapeReplaces

	val pipe  = new Regex("""(?<!\\)\|""")
	val prop  = new Regex("""(.*)(?<!\\)=(?<!\\)\[(.*)\]""")
	val comma = new Regex("""(?<!\\),""")

	implicit def parse(line: String) = {
		val entries = for (entry <- pipe.split(line)) yield {
			val prop(key, values) = entry
			unescape(key) -> (comma.split(values).toList map unescape)
		}
		entries toMap
	}

	def escape(value: String) = {
		escapes.foldLeft(value){ (s,z) => z._1.replaceAllIn(s, z._2) }
	}

	def unescape(value: String) = {
		unescapes.foldLeft(value){ (s,z) => z._1.replaceAllIn(s, z._2) }
	}

}
