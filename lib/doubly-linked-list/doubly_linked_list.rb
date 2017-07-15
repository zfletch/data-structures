class DoublyLinkedList
  def initialize
    @head = nil
    @tail = nil
  end

  def insert_left(value)
    @head = Node.new(value, right: head)
    @tail ||= head

    head.right.left = head if head.right

    self
  end

  def insert_right(value)
    @tail = Node.new(value, left: tail)
    @head ||= tail

    tail.left.right = tail if tail.left

    self
  end

  def delete_left(value)
    node = each_left { |v| break v if v == value }

    delete_node(node)
  end

  def delete_right(value)
    node = each_right { |v| break v if v == value }

    delete_node(node)
  end

  def each_left
    node = head

    while node
      yield node.value
      node = node.right
    end
  end

  def each_right
    node = tail

    while node
      yield node.value
      node = node.left
    end
  end

  def inspect
    array = []
    each_left { |v| array << v }

    "<<#{array.join(", ")}>>"
  end

  private

  attr_reader :head, :tail

  def delete_node(node)
    return unless node

    if node == head && node == tail
      @head = @tail = nil
    elsif node == head
      @head = node.right
      head.left = nil
    elsif node == tail
      @tail = node.left
      tail.right = nil
    else
      node.left.right = node.right
      node.right.left = node.left
    end

    node.value
  end

  class Node
    attr_accessor :value, :left, :right

    def initialize(value, left: nil, right: nil)
      @value = value
      @left = left
      @right = right
    end
  end
end
