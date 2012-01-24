module Bee
  module Primitives
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

      def intNe
        bool(nip != pop)
      end

      public_instance_methods.each do |m|
        alias_method m.to_s.gsub(/[A-Z]/){|c| "-#{c.downcase}" }, m
        remove_method m
      end
    end
  end
end
