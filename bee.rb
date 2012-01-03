require "strscan"
require "term/ansicolor"

String.__send__(:include, Term::ANSIColor)

module Bee
  module List
    def head
      # Damnit, 1.8!
      is_a?(String) ? self[0,1] : self[0]
    end

    def tail
      self[1..-1]
    end

    def cons(a)
      method(self.class.name).call(a) + self
    end
  end

  ::Array.__send__(:include, List)
  ::String.__send__(:include, List)

  class Term
    def name?
      false
    end

    def literal?
      false
    end

    def quotation?
      false
    end
  end

  class Name < Term
    attr_reader :name

    def initialize(name)
      @name = name
    end

    def name?
      true
    end

    def inspect
      @name
    end
  end

  class Literal < Term
    attr_reader :value

    def initialize(value)
      @value = value
    end

    def literal?
      true
    end

    def inspect
      @value.inspect
    end
  end

  class Quotation < Term
    attr_reader :terms

    def initialize(terms = [])
      @terms = terms
    end

    def append(term)
      if term.literal?
        @terms << term.value
      else
        @terms << term
      end
      self
    end

    def inspect
      "[" + @terms.map(&:inspect).join(" ") + "]"
    end
  end

  class Definition
    attr_reader :name, :terms

    def initialize(name = nil, terms = [])
      @name  = name
      @terms = terms
    end

    def append(token)
      if @name.nil?
        @name = token.name
      else
        if token.literal?
          @terms << token.value
        else
          @terms << token
        end
      end
    end
  end

  class Dictionary
    attr_reader :storage

    def initialize
      @storage = {}
    end

    def define(definition)
      @storage[definition.name] = definition.terms
      self
    end

    def lookup(name)
      @storage[name] or raise "undefined `#{name}'"
    end

    def import(other)
      @storage.merge!(other.storage)
      self
    end
  end

  class Parser

    # @return [Quotation, Dictionary]
    def parse(unparsed)
      dictionary = Dictionary.new
      nesting = [Quotation.new]
      scanner = StringScanner.new(unparsed)
      scanner.skip(/\s+/)

      while token = scanner.scan(/\[|\]|"[^"]*"|'[^']*'|[^\s\]]+/)
        case token
        when "["
          Quotation.new.tap do |q|
            nesting.last.append(q)
            nesting.push(q)
          end
        when "]"
          raise "unexpected ]" unless nesting.size > 1
          nesting.pop
        when ":"
          nesting.push(Definition.new)
        when ";"
          dictionary.define(nesting.pop)
        else
          nesting.last.append(term(token))
        end

        scanner.skip(/\s+/)
      end

      raise "unexpected EOF" unless nesting.size == 1
      return nesting.first, dictionary
    end

    def term(token)
      case token
      when /^-?\d+$/;       Literal.new(token.to_i)
      when /^-?\d*\.\d+$/;  Literal.new(token.to_f)
      when /^"([^"]*)"$/;   Literal.new(token[1..-2])
      when /^'([^']*)'$/;   Literal.new(token[1..-2])
      when "true";          Literal.new(true)
      when "false";         Literal.new(false)
      else                  Name.new(token)
      end
    end
  end

  class Interpreter
    attr_reader :stack, :input, :dictionary

    def initialize(debug = true)
      @debug = debug
      @stack = []
      @input = []
      @dictionary = Dictionary.new
    end

    def run(quotation, dictionary)
      @dictionary.import(dictionary)
      @input.concat(quotation.terms)

      # Store triples of stack + current + continuation
      trace = []

      until (term = @input.shift).nil?
        trace << [@stack.map(&:inspect).join(" "), term.inspect, @input.map(&:inspect).join(" ")]

        if term.is_a?(Term) and term.name?
          case term.name
          when "id" # S -> S

          when "halt" # S -> 0
            @stack = []
            @input = []

          when "print" # S t -> S
            a = @stack.pop
            $stdout.puts(a.inspect)

          when "apply" # S (S -> T) -> T
            a = @stack.pop
            @input.unshift(*a.terms)

          when "quote" # S t -> S (U -> U t)
            a = @stack.pop
            @stack.push(Quotation.new([a]))

          when "compose" # S (X -> Y) (Y -> Z) -> S (X -> Z)
            b = @stack.pop
            a = @stack.pop
            @stack.push(Quotation.new(a.terms + b.terms))

          when "pop" # S t -> S
            @stack.pop

          when "swap" # S t u -> S u t
            b = @stack.pop
            a = @stack.pop
            @stack.push(b)
            @stack.push(a)

          when "dup" # S t -> S t t
            a = @stack.pop
            @stack.push(a)
            @stack.push(a)

          when "dip" # S (S -> T) u -> T u
            b = @stack.pop
            a = @stack.pop
            @input.unshift(a)
            @input.unshift(*b.terms)

          when "dig" # S a b c -> S b c a
            c = @stack.pop
            b = @stack.pop
            a = @stack.pop
            @stack.push(b)
            @stack.push(c)
            @stack.push(a)

          when "if" # S boolean t t -> S t
            c = @stack.pop
            b = @stack.pop
            a = @stack.pop
            @stack.push(a ? b : c)

          when *%w(+ - * / % ** < <= == >= > >> << & | ^)
            b = @stack.pop
            a = @stack.pop
            @stack.push(a.__send__(term.name.to_sym, b))

          when *%w(to_s to_i to_f)
            a = @stack.pop
            @stack.push(a.__send__(term.name.to_sym))

          when "and" # S boolean boolean -> S boolean
            b = @stack.pop
            a = @stack.pop
            @stack.push(a && b)

          when "or" # S boolean boolean -> S boolean
            b = @stack.pop
            a = @stack.pop
            @stack.push(a || b)

          when "xor" # S boolean boolean -> S boolean
            b = @stack.pop
            a = @stack.pop
            @stack.push(a ^ b)

          when "not" # S boolean -> S boolean
            a = @stack.pop
            @stack.push(!a)

          when "null" # S -> S list
            @stack.push([])

          when "cons" # S list t -> S t-list
            b = @stack.pop
            a = @stack.pop
            @stack.push(a.cons(b))

          # Should this be polymorphic?
          #   boolean [true-case] [false-case] fold
          #   option  [some-case] [none-case]  fold
          #   either  [left-case] [right-case] fold
          #   list    [null-case] [cons-case]  fold
          #
          # Probably not, because the number of cases (arguments) depends on
          # the data type definition. Type classes can't safely handle per-
          # instance arity -- needs research -- and readability would suffer
          # as reading "fold" in the source could imply several meanings
          when "fold" # S t-list (S -> U) (S t-list t -> U) -> U
            c = @stack.pop
            b = @stack.pop
            a = @stack.pop
            if a.empty?
              @input.unshift(*b.terms)
            else
              @stack.push(a.tail)
              @stack.push(a.head)
              @input.unshift(*c.terms)
            end

          else
            @input.unshift(*@dictionary.lookup(term.name))
          end
        else
          # Just a literal value
          @stack.push(term)
        end
      end

      @stack
    rescue
      @input.clear
    ensure
      s = trace.map{|_| _[0].length }.max # stack
      t = trace.map{|_| _[1].length }.max # eval term
      c = trace.map{|_| _[2].length }.max # continuation

      maxs, maxt, maxc = trace.inject([0,0,0]) do |(s,t,c), _|
        [ s > _[0].length ? s : _[0].length,
          t > _[1].length ? t : _[1].length,
          c > _[2].length ? c : _[2].length ]
      end

      trace.each do |t|
        puts ".. " << t[0].rjust(maxs).yellow    <<
             " : " << t[1].rjust(maxt).cyan.bold <<
             " : " << t[2]
      end
    end

  end
end

###############################################################################

$vm = Bee::Interpreter.new
$p  = Bee::Parser.new

def bee(unparsed)
  $vm.run(*$p.parse(unparsed))
end

################################################################################
#
# $ irb -rbee
# >> bee "5 2 -"
# => [3]
#
# >> bee ": count dup dup print 0 == [pop] [1 - count] if apply ;"
# => [3]
#
# >> bee "count"
# 3
# 2
# 1
# 0
# => []
#
# >> bee ": twice dup compose apply ;"
# => []
#
# >> bee "3 4 5 [+] twice"
# => [12]
#
# >> bee ": length [0] [pop length 1 +] ;"
# => [12]
#
# >> bee "'bebe' length"
# => [12, 4]
#
# >> bee "pop pop"
# => []
#
# >> bee ": sum [0] [swap sum +] fold ;"
#
# >> bee "null 3 cons 2 cons 1 cons"
# => [[1,2,3]]
#
# >> bee "sum"
# => [6]
#
################################################################################
