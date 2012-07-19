module Bee
  class Input < Array
    def inspect
      map(&:inspect).join(" ")
    end
  end
end
