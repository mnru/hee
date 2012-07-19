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
          unless token.respond_to?(:name) and token.name?
            raise "token after ':' must be a name"
          end

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

end
