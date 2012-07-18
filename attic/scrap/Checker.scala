import com.github.kputnam.bee.types._
import com.github.kputnam.bee.static._

object Test {

    implicit def compose(x: WordType) = new {
        def |>(y: WordType) =
            x.compose(y.rename(x.freeVariables).asWord) match {
                case Some(x) => x.asWord
                case _       =>
                  throw new RuntimeException("can't compose " + x + " with " + y)
            }
    }

    val st = SymbolTable.default

    def lookup(s: String) =
        st.searchBindings(s) match { case Some(Entry(_, t)) => t.asWord }

    def Q(t: WordType) =
      t.quote

    def main(s: Array[String]) {
        val _swap = lookup("swap")
        val _dup  = lookup("dup")
        val _dig  = lookup("dig")
        val _bury = lookup("bury")
        val _sub  = lookup("-")
        val _cmp  = lookup("==")
        val _pop  = lookup("pop")
        val _app  = lookup("apply")
        val _if   = lookup("if")
        val _id   = lookup("id")
        val _y    = lookup("y")

        val _num  = NumericType.asWord
        val _bool = BooleanType.asWord

        // n [r] 
        //   swap dup 1 - bury 0 == [pop pop] [apply] if
        val _count =         // S num b
          (  _swap           // S b num
          |> _dup            // S b num num
          |> _num            // S b num num num
          |> _sub            // S b num num
          |> _bury           // S num b num
          |> _num            // S num b num num
          |> _cmp            // S num b bool
          |> Q(_pop)// _pop) // S num b bool (T u v -> T)
          |> Q(_app)         // S num b bool (T u v -> T) (X (X -> Y) -> Y)
          |> _if)

        // swap dup 1 <= [pop pop 1] [dup 1 - dig apply *] if
        val _fact = // S num b
          (  _swap  // S b num
          |> _dup   // S b num num
          |> _num   // S b num num num
          |> _cmp   // S b num bool
          |> Q(_pop |> _pop |> _num)                         |>
             Q(_dup |> _num |> _sub |> _dig |> _app |> _sub) |>
             _if)

        println("Countdown: " + _count)
        println("Factorial: " + _fact)
    }
}

// vim: set ts=4 sw=4 et:
