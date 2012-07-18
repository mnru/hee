# length    :: (Num n) => [a] -> n
# take      :: (Integral n) => n -> [a] -> [a]
# drop      :: (Integral n) => n -> [a] -> [a]
# splitAt   :: (Integral n) => n -> [a] -> ([a],[a])
# index     :: (Integral n) => [a] -> n -> a
# replicate :: (Integral n) => n -> a -> [a]
#
# nub       = nubBy (==)
# delete    = deleteBy (==)
# union     = unionBy (==)
# intersect = intersectBy (==)
# group     = groupBy (==)
#
# sort    = sortBy compare
# insert  = insertBy compare
# maximum = maximumBy compare
# maximum = minimumBy compare

class List

  def prepend(head)
    Cons.new(head, self)
  end

  def foldl(init, &block)
    List.foldl(self, init, &block)
  end

  def listr(init, &block)
    List.foldr(self, init, &block)
  end

  def reverse
    List.reverse(self)
  end

  def map(&block)
    List.map(self, &block)
  end

  def intersperse(delim)
    List.intersperse(self, delim)
  end

  def join
    List.join(self)
  end

  def length
    List.length(self)
  end

  def inspect
    "(" << map(&:inspect).intersperse(" ").join << ")"
  end

  # @private
  class Null < List
    def null?
      true
    end

    def cons?
      false
    end

    def tail
      throw TypeError, "null.tail"
    end

    def head
      throw TypeError, "null.head"
    end
  end

  # @private
  class Cons < List
    attr_reader :head
    attr_reader :tail

    def initialize(head, tail)
      @head, @tail = head, tail
    end

    def null?
      false
    end

    def cons?
      true
    end
  end

  class << self
    def null
      @null ||= Null.new
    end

    def build(*args)
      args.inject(null, &:prepend).reverse
    end

    def foldl(list, init, &block)
      until list.null?
        init = block.call(init, list.head)
        list = list.tail
      end

      init
    end

    def foldr(list, init, &block)
      foldl(reverse(list), init, &block)
    end

    def reverse(list)
      foldl(list, null, &:prepend)
    end

    def map(list, &block)
      foldr(list, null){|l,e| l.prepend(yield(e)) }
    end

    def intersperse(list, delim)
      foldr(list, null){|l,e| (l.null?) ? l.prepend(e) : l.prepend(delim).prepend(e) }
    end

    def join(list)
      foldl(list, ""){|l,e| l << e.to_s }
    end

    def length(list)
      foldl(list, 0){|l,e| l + 1 }
    end
  end
end

