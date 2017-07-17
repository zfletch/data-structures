class Queue
  def initialize
    @head = nil
    @tail = nil
  end

  def empty?
    head.nil?
  end

  def push(val)
    node = Node.new(val)

    if empty?
      @head = @tail = node
    else
      @tail = tail.next_node = node
    end

    self
  end

  def shift
    return if empty?

    val = head.val
    @head = head.next_node

    val
  end

  def inspect
    "\\#{string}\\"
  end

  private

  attr_reader :head, :tail

  def string(node = head)
    return "" unless node

    node.next_node ? "#{node.val} " << string(node.next_node) : "#{node.val}"
  end

  class Node
    attr_accessor :next_node
    attr_reader :val

    def initialize(val)
      @val = val
      @next_node = nil
    end
  end
end
