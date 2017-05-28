# Trie
#
# Example:
#   trie = Trie.new
#   trie.set("fruit", "apple")
#   trie.set("fruit", "pear")
#   trie.set("color", "blue")
#
#   trie.get("fruit") # => "pear"
#   trie.get("color") # => "blue"
#   trie.get("frui") # => nil

class Trie
  def initialize
    @nodes = []
  end

  def set(key, value)
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
