class BinaryTree
  include Enumerable

  def insert(value)
    node = Node.new(value)

    empty? ? @root = node : root.insert(node)

    self
  end

  def empty?
    root.nil?
  end

  def delete(value)
    return if empty?

    @root, val = *@root.delete_and_value(value)

    val
  end

  def exists?(number)
    return false if empty?

    root.exists?(number)
  end

  def in_order
    return unless root

    root.in_order { |value| yield value }
  end
  alias_method :each, :in_order

  def inspect
    empty? ? "()" : root.string
  end

  private

  attr_reader :root

  class Node
    attr_accessor :value, :left_child, :right_child

    def initialize(value, left_child: nil, right_child: nil)
      @value = value
      @left_child = left_child
      @right_child = right_child
    end

    def insert(node)
      if node.value < value
        left_child ? left_child.insert(node) : @left_child = node
      else
        right_child ? right_child.insert(node) : @right_child = node
      end
    end

    def exists?(number)
      if value == number
        true
      elsif number < value
        left_child ? left_child.exists?(number) : nil
      elsif number > value
        right_child ? right_child.exists?(number) : nil
      end
    end

    def in_order
      left_child.in_order { |value| yield value } if left_child
      yield value
      right_child.in_order { |value| yield value } if right_child
    end

    def string
      left_string = left_child && left_child.string
      right_string = right_child && right_child.string

      "(#{[left_string, value, right_string].compact.join(" ")})"
    end

    def delete_and_value(number)
      if number < value
        @left_child, val = *(left_child ? left_child.delete_and_value(number) : [nil, nil])

        [self, val]
      elsif number > value
        @right_child, val = *(right_child ? right_child.delete_and_value(number) : [nil, nil])

        [self, val]
      else
        tree_without_node_and_value
      end
    end

    protected

    def minimum
      left_child ? left_child.minimum : self
    end

    private

    def tree_without_node_and_value
      if !left_child && !right_child
        [nil, value]
      elsif right_child && !left_child
        [right_child, value]
      elsif left_child && !right_child
        [left_child, value]
      else
        return_value = value
        successor = right_child.minimum

        @value = successor.value
        @right_child, _ = @right_child.delete_and_value(value)

        [self, return_value]
      end
    end
  end
end
