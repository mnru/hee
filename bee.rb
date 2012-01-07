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

    def add(definition)
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

    # @return [[Terms], Dictionary]
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
          dictionary.add(nesting.pop)
        else
          nesting.last.append(term(token))
        end

        scanner.skip(/\s+/)
      end

      raise "unexpected EOF" unless nesting.size == 1
      return nesting.first.terms, dictionary
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

    def run(debug, terms, dictionary)
      @dictionary.import(dictionary)
      @input.concat(terms)

      # Store triples of stack + current + continuation
      trace = []

      until (term = @input.shift).nil?
        trace << [@stack.map(&:inspect).join(" "), term.inspect, @input.map(&:inspect).join(" ")] if debug

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

          when "="
            b = @stack.pop
            a = @stack.pop
            @stack.push(a == b)

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
          when "unlist" # S t-list (S -> U) (S t-list t -> U) -> U
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
                       " : " << t[2]
        end
      end
    end

  end
end

###############################################################################

$vm = Bee::Interpreter.new
$p  = Bee::Parser.new

$vm.dictionary.add(Bee::Definition.new("bottles",
  $p.parse('dup dup to_s " bottles" swap cons print 0 == [pop] [1 - bottles] if apply').first))

$vm.dictionary.add(Bee::Definition.new("twice",
  $p.parse("dup compose apply").first))

$vm.dictionary.add(Bee::Definition.new("fold",
  $p.parse("dup quote compose [fold] compose [quote dup] dip compose unlist").first))

$vm.dictionary.add(Bee::Definition.new("reverse-map",
  $p.parse("null [swap] dig compose [cons] compose fold").first))

$vm.dictionary.add(Bee::Definition.new("reverse",
  $p.parse("null [swap cons] fold").first))

$vm.dictionary.add(Bee::Definition.new("map",
  $p.parse("reverse-map reverse").first))

# Naive version (non tail call)
#$vm.dictionary.add(Bee::Definition.new("map",
#  $p.parse("swap [pop null] [dig dup dip [swap] dip map swap cons] unlist").first))

$vm.dictionary.add(Bee::Definition.new("length",
  $p.parse("0 [[pop] dip 1 +] fold").first))

# Naive version (non tail call)
#$vm.dictionary.add(Bee::Definition.new("length",
#  $p.parse("[0] [pop length 1 +] unlist").first))

$vm.dictionary.add(Bee::Definition.new("sum",
  $p.parse("0 [+] fold").first))

#$vm.dictionary.add(Bee::Definition.new("sum",
#  $p.parse("[0] [swap sum +] unlist").first))

$vm.dictionary.add(Bee::Definition.new("product",
  $p.parse("1 [*] fold").first))

$vm.dictionary.add(Bee::Definition.new("null?",
  $p.parse("[true] [pop pop false] unlist").first))

$vm.dictionary.add(Bee::Definition.new("cons?",
  $p.parse("[false] [pop pop true] unlist").first))

# This is like fix f = f (fix f)... references itself
$vm.dictionary.add(Bee::Definition.new("y'",
  $p.parse("dup quote [y'] compose quote swap compose apply").first))

$vm.dictionary.add(Bee::Definition.new("!'",
  $p.parse("swap dup 1 = [pop pop 1] [dup [-1 + swap apply] dip *] if apply").first))

$vm.dictionary.add(Bee::Definition.new("!",
  $p.parse("[!'] y'").first))

def bee(unparsed, debug = false)
  $vm.run(debug, *$p.parse(unparsed))
rescue
  $vm.input.clear
  $stderr.puts $!.to_s.red
end

################################################################################
#
# $ irb -rbee
# >> bee "5 2 -"
# => [3]
#
# >> bee "3 bottles"
# 3 bottles
# 2 bottles
# 1 bottles
# 0 bottles
# => []
#
# >> bee "3 4 5 [+] twice"
# => [12]
#
# >> bee "'bebe' length"
# => [12, 4]
#
# >> bee "pop pop"
# => []
#
# >> bee ": xs null 3 cons 2 cons 1 cons ;"
# => [[1,2,3]]
#
# >> bee "xs sum"
# => [6]
#
# >> "xs [dup *] map"
# => [6, [1,4,9]]
#
################################################################################
