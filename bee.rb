require "strscan"

module Bee

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
      @terms.inspect
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

      while token = scanner.scan(/\[|\]|[^\s\]]+/)
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
      when /^"([^"]+)"$/;   Literal.new(token[1..-2])
      when "true";          Literal.new(true)
      when "false";         Literal.new(false)
      else                  Name.new(token)
      end
    end
  end

  class Interpreter
    attr_reader :stack, :input, :dictionary

    def initialize
      @stack = []
      @input = []
      @dictionary = Dictionary.new
    end

    def run(quotation, dictionary)
      @dictionary.import(dictionary)
      @input.concat(quotation.terms)

      until (term = @input.shift).nil?
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

          when "if" # S boolean t t -> S t
            c = @stack.pop
            b = @stack.pop
            a = @stack.pop
            @stack.push(a ? b : c)

          when *%w(+ - * / % ** < <= == >= > >> << & | ^)
            b = @stack.pop
            a = @stack.pop
            @stack.push(a.__send__(term.name.to_sym, b))

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

          else
            @input.unshift(*@dictionary.lookup(term.name))
          end
        else
          @stack.push(term)
        end
      end

      @stack
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
# >> bee "3 4 +"
# => [7]
#
# >> bee ": count dup dup print 0 == [pop] [1 - count] if apply ;"
# => [7]
#
# >> bee "count"
# 7
# 6
# 5
# 4
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
################################################################################
