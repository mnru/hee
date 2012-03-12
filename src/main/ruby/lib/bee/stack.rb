module Bee

  class Stack < Array

    # S t -> S t t
    def dup
      push(last)
    end

    # S t -> S
    def pop
      raise "stack underflow" if empty?
      super
    end

    # S t u -> S u t
    def swap
      push(pop, pop)
    end

    # S t u v -> S u v t
    def dig
      @stack.push(@stack.slice!(-3))
    end

    # S -> S
    def id
    end

    def inspect
      if empty?
        "(empty)"
      else
        map(&:inspect).join(" ")
      end
    end

  end

end
