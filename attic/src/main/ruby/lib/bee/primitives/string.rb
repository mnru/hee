module Bee
  module Primitives
    module String
      def stringShow
        pop.to_s
      end

      def stringLt
        bool(nip < pop)
      end

      def stringGt
        bool(nip > pop)
      end

      def stringLte
        bool(nip <= pop)
      end

      def stringGte
        bool(nip >= pop)
      end

      def stringEq
        bool(nip == pop)
      end

      def stringNe
        bool(nip != pop)
      end

      public_instance_methods.each do |m|
        alias_method m.to_s.gsub(/[A-Z]/){|c| "-#{c.downcase}" }, m
        remove_method m
      end
    end
  end
end
