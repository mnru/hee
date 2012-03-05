module Bee
  module Primitives
    autoload :Bits,   "bee/primitives/bits"
    autoload :Char,   "bee/primitives/char"
    autoload :Float,  "bee/primitives/float"
    autoload :Int,    "bee/primitives/int"
    autoload :String, "bee/primitives/string"

    # S -> empty
    def reset
      @stack.clear
      @input.clear
    end

    # S (S -> T) -> T
    def apply
      @input.unshift(*pop.terms)
    end

    # S t -> S (U -> U t)
    def quote
      push(Term::Quotation.new([pop]))
    end

    # S (X -> Y) (Y -> Z) -> S (X -> Z)
    def compose
      push(Term::Quotation.new(nip.terms + pop.terms))
    end

    # S u (S -> T) -> T u
    def dip
      @input.unshift(*pop.terms, pop)
    end

    # S a -> S string
    def show
      push(pop.inspect)
    end

    # S a -> S
    def print
      $stdout.puts pop.inspect
    end

    # S string -> S
    def dump
      a = pop
      p = Parser.new

      File.open(a.to_s, "w+") do |io|
        @dictionary.definitions.each{|d| io << p.unparse(d) }
      end
    end

    # S string -> S
    def load
      a    = pop
      t, d = Parser.new.parse(File.read(a.to_s))
      @dictionary.import(d)
    end

    # S (T -> U) -> S (T -> U)
    def inline
      a = pop
      q = Term::Quotation.new

      while t = a.terms.shift
        if t.name? and @dictionary.defined?(t.name)
          q.terms.push(*@dictionary.lookup(t.name))
        else
          q.terms.push(t)
        end
      end

      push(q)
    end

  end
end
