module Bee

  class Stack < Array
    def dup # S t -> S t t
      push(last)
    end

    def drop # S t -> S
      pop
    end

    def swap # S t u -> S u t
      push(pop, pop)
    end

    def nip # S t u -> S u
      slice!(-2)
    end

    def dig # S t u v -> S u v t
      @stack.push(@stack.slice!(-3))
    end

    def rot # S t u v -> S u v t
      @stack.push(@stack.slice!(-3))
    end

    def over # S t u -> S t u t
      @stack.push(@stack.slice(-2))
    end

    def id # S -> S
    end

    def inspect
      map(&:inspect).join(" ")
    end
  end

end
