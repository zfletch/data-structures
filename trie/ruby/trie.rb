class Trie
  def initialize
    @nodes = []
  end

  def set(key, value)
    return self if key.empty?

    current_nodes = nodes
    node = nil

    key.chars.each do |character|
      char = ord(character)
      node = current_nodes[char] ||= Node.new(char)
      current_nodes = node.children
    end

    node.value = value

    self
  end

  def get(key)
    return self if key.empty?

    current_nodes = nodes
    node = nil

    key.chars.each do |character|
      char = ord(character)
      if node = current_nodes[ord(character)]
        current_nodes = node.children
      else
        return
      end
    end

    node.value
  end

  def inspect
    current_nodes = nodes
    "<#{nodes.compact.map(&:string).join(" ")}>"
  end

  private

  attr_reader :nodes

  def ord(character)
    character.ord - 'a'.ord
  end

  class Node
    attr_accessor :char, :children, :value

    def initialize(char)
      @char = char
      @children = []
    end

    def string
      "#{(char + 'a'.ord).chr} => #{value} (#{children.compact.map(&:string).join(") (")})"
    end
  end
end
