module Bee
  module Primitives
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
        bool(nip <= pop)
      end

      def floatGte
        bool(nip >= pop)
      end

      def floatEq
        bool(nip == pop)
      end

      def floatNe
        bool(nip != pop)
      end

      public_instance_methods.each do |m|
        alias_method m.to_s.gsub(/[A-Z]/){|c| "-#{c.downcase}" }, m
        remove_method m
      end
    end
  end
end
