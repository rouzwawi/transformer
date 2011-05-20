package org.rouzwawi.transformer
 
import org.rouzwawi.transformer.MapParser._
import org.rouzwawi.transformer.selectors._
 
 
class App(val rule: rule) {
    def process(line: String) = {
        rule emit line foreach println
    }
}
