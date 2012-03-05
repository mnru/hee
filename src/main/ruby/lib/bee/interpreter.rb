module Bee
  class Interpreter
    include Primitives,
            Primitives::Bits,
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

      @stackops = %w(pop dup drop swap id)
      @primops  = (public_methods - Object.new.public_methods - [:run]).map(&:to_s)
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

        # Calculate size of each column
        maxs, maxt, maxc = trace.inject([0,0,0]) do |(s,t,c), _|
          [ s > _[0].length ? s : _[0].length,
            t > _[1].length ? t : _[1].length,
            c > _[2].length ? c : _[2].length ]
        end

        # Ensure at least 10 chars of continuation on the screen
        if maxs + maxt > 61 # 80 - 3 - 3 - 3 - 10
          maxs = 40 if maxs > 40
          maxt = 61 - maxs
          maxc = 80 - maxt - maxs
        end

        # Print a header
        $stdout.puts " stack".rjust(maxs + 3, "=").yellow.bold <<
                     " : " << "term ".ljust(maxt, "=").cyan.bold <<
                     " : " << "continuation ".ljust(maxc, "=").bold

        trace.each do |triple|
          $stdout.puts (".. " << triple[0].rjust(maxs)[-maxs..-1].yellow <<
                        " : " << triple[1].ljust(maxt)[0,   maxt].cyan   <<
                        " : " << triple[2][0, maxc])
        end
      end
    end

  private

    def nip
      @stack.swap
      @stack.pop
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
