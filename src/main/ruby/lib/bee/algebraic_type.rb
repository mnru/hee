module Bee

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
      input.unshift(Term::Name.new("apply"))
      input.unshift(fs[boxed.tag])
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
          "#{@fields.map(&:inspect).join(' ')} #{@name}"
        end
      end
    end
  end

end
