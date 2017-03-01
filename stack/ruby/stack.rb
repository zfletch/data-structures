class Stack
  def initialize
    @first_node = nil
  end

  def push(data)
    @first_node = Node.new(data, first_node)

    self
  end

  def pop
    return unless first_node

    data = first_node.data
    @first_node = first_node.next_node

    data
  end

  def to_s
    "/\\\n#{string}\\/"
  end

  private

  attr_reader :first_node

  def string(node = first_node)
    return "" unless node

    "#{node.data}\n" << string(node.next_node)
  end

  class Node
    attr_accessor :next_node
    attr_reader :data

    def initialize(data, next_node = nil)
      @data = data
      @next_node = next_node
    end
  end
end
