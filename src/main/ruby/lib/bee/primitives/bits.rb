module Bee
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
  end
end
