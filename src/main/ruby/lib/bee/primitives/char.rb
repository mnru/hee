module Bee
  module Primitives
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
        bool(nip <= pop)
      end

      def charGte
        bool(nip >= pop)
      end

      def charEq
        bool(nip == pop)
      end

      def charNe
        bool(nip != pop)
      end

      public_instance_methods.each do |m|
        alias_method m.to_s.gsub(/[A-Z]/){|c| "-#{c.downcase}" }, m
        remove_method m
      end
    end
  end
end
