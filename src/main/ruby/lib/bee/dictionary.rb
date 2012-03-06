module Bee

  class Dictionary
    attr_reader :storage

    def initialize
      @storage = {}
    end

    def add(name, value)
      @storage[name] = value
      self
    end

    def defined?(name)
      @storage.include?(name)
    end

    # @return [Term]
    def lookup(name)
      @storage[name] or raise "undefined `#{name}'"
    end

    def import(other)
      @storage.merge!(other.storage)
      self
    end

    def definitions
      @storage.map{|name, terms| Term::Definition.new(name, terms) }
    end

    def names
      @storage.keys
    end
  end

end
