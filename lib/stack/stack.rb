class Stack
  def initialize
    @head = nil
  end

  def empty?
    head.nil?
  end

  def push(val)
    @head = Node.new(val, head)

    self
  end

  def pop
    return if empty?

    val = head.val
    @head = head.next_node

    val
  end

  def inspect
    "/#{string}/"
  end

  private

  attr_reader :head

  def string(node = head)
    return "" unless node

    node.next_node ? "#{node.val} " << string(node.next_node) : "#{node.val}"
  end

  class Node
    attr_accessor :next_node
    attr_reader :val

    def initialize(val, next_node = nil)
      @val = val
      @next_node = next_node
    end
  end
end
