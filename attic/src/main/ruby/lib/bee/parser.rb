require "strscan"

module Bee
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
end
