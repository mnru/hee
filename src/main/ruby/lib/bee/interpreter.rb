module Bee
  class Interpreter
    include Primitives::Bits,
            Primitives::Int,
            Primitives::Float,
            Primitives::Char,
            Primitives::String

    attr_reader :stack, :input, :dictionary

    def initialize(debug = true)
      @debug = debug
      @stack = Stack.new
      @input = Input.new
      @dictionary = Dictionary.new

      @stackops = %w(pop dup drop swap nip dig rot over id)
      @primops  = (public_methods - Object.new.public_methods - [:run]).map(&:to_s)
    end

    def reset
      @stack.clear
      @input.clear
    end

    def apply # S (S -> T) -> T
      @input.unshift(*@stack.pop.terms)
    end

    def quote # S t -> S (U -> U t)
      @stack.push(Term::Quotation.new([@stack.pop]))
    end

    def compose # S (X -> Y) (Y -> Z) -> S (X -> Z)
      @stack.push(Term::Quotation.new(@stack.nip.terms + @stack.pop.terms))
    end

    def dip # S u (S -> T) -> T u
      @input.unshift(*@stack.pop.terms, @stack.pop)
    end

    def print # S a -> S
      $stdout.puts pop.inspect
    end

    def dump # S string -> S
      a = @stack.pop
      p = Parser.new

      File.open(a.to_s, "w+") do |io|
        @dictionary.definitions.each{|d| io << p.unparse(d) }
      end
    end

    def load # S string -> S
      a    = @stack.pop
      t, d = Parser.new.parse(File.read(a.to_s))
      @dictionary.import(d)
    end

    def inline # S (T -> U) -> S (T -> U)
      a = @stack.pop
      q = Term::Quotation.new

      while t = a.terms.shift
        if t.name? and @dictionary.defined?(t.name)
          q.terms.push(*@dictionary.lookup(t.name))
        else
          q.terms.push(t)
        end
      end

      @stack.push(q)
    end

    def run(debug, terms, dictionary)
      @dictionary.import(dictionary)
      @input.concat(terms)

      # Store triples of stack + current + continuation
      trace = []

      until (term = @input.shift).nil?
        if debug
          trace << [@stack.inspect, term.inspect, @input.inspect]
        end

        if AlgebraicType::Variant === term
          term.box(@stack)
        elsif AlgebraicType === term
          term.unbox(@stack, @input)
        elsif !(Term === term)
          @stack.push(term)
        elsif term.literal?
          @stack.push(term.value)
        elsif term.quotation?
          @stack.push(term)
        elsif term.name?
          case term.name
          when *@stackops
            @stack.__send__(term.name)
          when *@primops
            __send__(term.name)
          else
            @input.unshift(*@dictionary.lookup(term.name))
          end
        end
      end

      @stack
    ensure
      unless trace.empty?
        s = trace.map{|_| _[0].length }.max # stack
        t = trace.map{|_| _[1].length }.max # eval term
        c = trace.map{|_| _[2].length }.max # continuation

        maxs, maxt, maxc = trace.inject([0,0,0]) do |(s,t,c), _|
          [ s > _[0].length ? s : _[0].length,
            t > _[1].length ? t : _[1].length,
            c > _[2].length ? c : _[2].length ]
        end

        trace.each do |t|
          $stdout.puts ".. " << t[0].rjust(maxs).yellow <<
                       " : " << t[1].rjust(maxt).cyan   <<
                     ((" : " << t[2])[maxs+maxt .. 80] || "")
        end
      end
    end

  private

    def nip
      @stack.nip
    end

    def pop
      @stack.pop
    end

    def push(*args)
      @stack.push(*args)
    end

    def bool(value)
      @dictionary.lookup(value ? "true" : "false").box(@stack)
    end
  end
end
