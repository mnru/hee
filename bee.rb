require "strscan"
require "term/ansicolor"

String.__send__(:include, Term::ANSIColor)

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

      def unparse
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

      def unparse
        @value.inspect
      end
    end

    class Quotation < Term
      attr_reader :terms

      def initialize(terms = [])
        @terms = terms
      end

      def <<(term)
        @terms.push(term)
        self
      end

      def quotation?
        true
      end

      def inspect
        "[" + @terms.map(&:inspect).join(" ") + "]"
      end

      def unparse
        "[" + @terms.map(&:unparse).join(" ") + "]"
      end
    end

    class Definition < Term
      attr_reader :name, :terms

      alias value terms

      def initialize(name = nil, terms = [])
        @name, @terms = name, terms
      end

      def <<(token)
        if @name.nil?
          @name = token.name
        else
          @terms << token
        end

        self
      end
    end

    class TypeBuilder
      def initialize(name = nil, variants = [])
        @name, @variants = name, variants
      end

      def <<(token)
        if @name.nil?
          @name = token.name
        elsif token.name == "|"
          @variants << []
        else
          @variants.last << token.name
        end
      end

      def value
        AlgebraicType.define(@name, @variants.map{|(n,*a)| [n, a.length] })
      end
    end
  end

  class AlgebraicType
    attr_reader :name, :variants

    def self.define(name, variants)
      new(name, variants.map.with_index{|(n,a),t| Variant.new(n,t,a) })
    end

    def initialize(name, variants)
      @name, @variants = name, variants
    end

    def unbox(stack, input)
      if stack.length < @variants.length + 1
        raise "stack underflow"
      end

      # S boxed [1] [2] [...] unbox-type
      boxed, *fs = stack.slice!(-(@variants.length+1), @variants.length+1)
      boxed.unbox(stack)
      input.unshift(*fs[boxed.tag].terms)
    end

    def inspect
      "un#{@name}[#{@variants.map(&:arity).join(',')}]"
    end

    class Variant
      attr_reader :name, :tag, :arity

      def initialize(name, tag, arity)
        @name, @tag, @arity = name, tag, arity
      end

      def box(stack)
        if stack.length < @arity
          raise "stack underflow"
        end

        stack.push(Boxed.new(@name, @tag, stack.slice!(-@arity, @arity)))
      end

      def inspect
        "#{@name}[#{@arity}]"
      end
    end

    class Boxed
      attr_reader :name, :tag, :fields

      def initialize(name, tag, fields)
        @name, @tag, @fields = name, tag, fields
      end

      def unbox(stack)
        stack.push(*@fields)
      end

      def inspect
        if @fields.empty?
          @name
        else
          "(#{@fields.map(&:inspect).join(' ')} #{@name})"
        end
      end
    end
  end

  class Dictionary
    attr_reader :storage

    def initialize
      @storage = {}
    end

    def add(name, value)
      @storage[name] = value
      self
    end

    def defined?(name)
      @storage.include?(name)
    end

    # @return [Term]
    def lookup(name)
      @storage[name] or raise "undefined `#{name}'"
    end

    def import(other)
      @storage.merge!(other.storage)
      self
    end

    def definitions
      @storage.map{|name, terms| Term::Definition.new(name, terms) }
    end
  end

  class Parser

    # @return [[Terms], Dictionary]
    def parse(unparsed)
      nested      = [[]]
      dictionary  = Dictionary.new

      scanner = StringScanner.new(unparsed)
      scanner.skip(/\s+/)

      while token = scanner.scan(/\[|\]|"|'|[^\s\]]+/)
        case token
        when "["
          nested << Term::Quotation.new
        when "]"
          token = nested.pop
          nested.last << token
          if nested.empty?
            raise "unexpected ]"
          end
        when "'"
          token = scanner.scan(/\\?.(?!\S)/) or raise "unexpected '"
          nested.last << term("'" << token)
        when '"'
          token = scanner.scan(/(?:\\"|[^"])*"/) or raise 'unterminated "'
          nested.last << term('"' << token)
        when ":"
          nested << Term::Definition.new
        when "::"
          nested << Term::TypeBuilder.new
        when ";"
          case nested.last
          when Term::Definition
            token = nested.pop
            dictionary.add(token.name, token.value)
          when Term::TypeBuilder
            token = nested.pop
            value = token.value

            # Constructors
            value.variants.each{|v| dictionary.add(v.name, v) }

            # Deconstructor
            dictionary.add("un#{value.name}", value)
          else
            nested.last << term(token)
          end
        else
          nested.last << term(token)
        end

        scanner.skip(/\s+/)
      end

      raise "unexpected eof" unless nested.size == 1
      return nested.last, dictionary
    end

    def term(token)
      unescape = Hash['\t' => "\t", '\n' => "\n", '\r' => "\r", '\"' => '"', "\\'" => "'"]

      case token
      when /^-?\d+$/;       Term::Literal.new(token.to_i)
      when /^-?\d*\.\d+$/;  Term::Literal.new(token.to_f)
      when /^".*"$/;        Term::Literal.new(token[1..-2].gsub(/\\[tnr"]/){|c| unescape[c] })
      when /^'.*$/;         Term::Literal.new(token[1..-1].gsub(/\\[tnr']/){|c| unescape[c] })
      else                  Term::Name.new(token)
      end
    end

    def unparse(o)
      case o
      when Term::Definition
        ": #{o.name}\n  #{o.terms.map{|p| unparse(p) }.join(' ')} ;\n\n"
      when Term::Quotation
        o.unparse
      when Term::Literal
        o.unparse
      when Term::Name
        o.unparse
      else
        raise "can't unparse #{o.class}"
      end
    end
  end

  class Stack < Array
    def dup # S t -> S t t
      push(last)
    end

    def drop # S t -> S
      pop
    end

    def swap # S t u -> S u t
      push(pop, pop)
    end

    def nip # S t u -> S u
      slice!(-2)
    end

    def dig # S t u v -> S u v t
      @stack.push(@stack.slice!(-3))
    end

    def rot # S t u v -> S u v t
      @stack.push(@stack.slice!(-3))
    end

    def over # S t u -> S t u t
      @stack.push(@stack.slice(-2))
    end

    def id # S -> S
    end

    def inspect
      map(&:inspect).join(" ")
    end
  end

  class Input < Array
  end

  module Primitives
    module Bits
      def bitsShow
        pop.to_s
      end

      def bitsAnd
        push(nip & pop)
      end

      def bitsOr
        push(nip | pop)
      end

      def bitsXor
        push(nip ^ pop)
      end

      def bitsNot
        push(~pop)
      end

      def bitsLshift
        push(nip << pop)
      end

      def bitsRshift
        push(nip >> pop)
      end

      public_instance_methods.each do |m|
        alias_method m.to_s.gsub(/[A-Z]/){|c| "-#{c.downcase}" }, m
        remove_method m
      end
    end

    module Int
      def intShow
        pop.to_s
      end

      def intNeg
        push(-pop)
      end

      def intAdd
        push(nip + pop)
      end

      def intSub
        push(nip - pop)
      end

      def intMul
        push(nip * pop)
      end

      def intDiv
        push(nip / pop)
      end

      def intMod
        push(nip % pop)
      end

      def intLt
        bool(nip < pop)
      end

      def intGt
        bool(nip > pop)
      end

      def intLte
        bool(nip <= pop)
      end

      def intGte
        bool(nip >= pop)
      end

      def intEq
        bool(nip == pop)
      end

      public_instance_methods.each do |m|
        alias_method m.to_s.gsub(/[A-Z]/){|c| "-#{c.downcase}" }, m
        remove_method m
      end
    end

    module Float
      def floatShow
        pop.to_s
      end

      def floatNeg
        push(-pop)
      end

      def floatAdd
        push(nip + pop)
      end

      def floatSub
        push(nip - pop)
      end

      def floatMul
        push(nip * pop)
      end

      def floatDiv
        push(nip / pop)
      end

      def floatMod
        push(nip % pop)
      end

      def floatLt
        bool(nip < pop)
      end

      def floatGt
        bool(nip > pop)
      end

      def floatLte
        bool(nip < pop)
      end

      def floatGte
        bool(nip > pop)
      end

      def floatEq
        bool(nip == pop)
      end

      public_instance_methods.each do |m|
        alias_method m.to_s.gsub(/[A-Z]/){|c| "-#{c.downcase}" }, m
        remove_method m
      end
    end

    module Char
      def charShow
        pop.to_s
      end

      def charLt
        bool(nip < pop)
      end

      def charGt
        bool(nip > pop)
      end

      def charLte
        bool(nip < pop)
      end

      def charGte
        bool(nip > pop)
      end

      def charEq
        bool(nip == pop)
      end

      public_instance_methods.each do |m|
        alias_method m.to_s.gsub(/[A-Z]/){|c| "-#{c.downcase}" }, m
        remove_method m
      end
    end

    module String
      def stringShow
        pop.to_s
      end

      public_instance_methods.each do |m|
        alias_method m.to_s.gsub(/[A-Z]/){|c| "-#{c.downcase}" }, m
        remove_method m
      end
    end
  end

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

###############################################################################

@vm ||= Bee::Interpreter.new
@p  ||= Bee::Parser.new

if File.exists?("runtime.bee")
  @vm.run(false, *@p.parse(File.read("runtime.bee")))
else
  $stderr.puts "cannot load runtime.bee from current directory".yellow
end

def bee(unparsed, debug = false)
  @vm.run(debug, *@p.parse(unparsed))
rescue
  @vm.input.clear
  $stderr.puts $!.to_s.red
# $stderr.puts "  " << $!.backtrace.join("\n  ")
end

def time(n, &block)
  a = Time.now
  c = nil
  n.times do |m|
    b = Time.now
    print "#{m}... "
    block.call
    c = Time.now
    puts c - b
  end
  return (c - a)/n
end

################################################################################
#
# $ irb -rbee
# >> bee "5 2 -"
# => [3]
#
# >> bee "'runtime.bee' load"
# => []
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
