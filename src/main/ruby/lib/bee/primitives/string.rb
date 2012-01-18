module Bee
  module Primitives
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
end
