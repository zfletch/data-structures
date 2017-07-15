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

    if root.value == value
      if root.left_child && root.right_child
        right_child = root.right_child
        @root = root.left_child

        root.insert(right_child)
      elsif root.left_child
        @root = root.left_child
      elsif root.right_child
        @root = root.right_child
      else
        @root = nil
      end

      value
    else
      root.delete(value)
    end
  end

  def exists?(number)
    return false unless root

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
      elsif number < value && left_child
        left_child.exists?(number)
      elsif number > value && right_child
        right_child.exists?(number)
      end
    end

    def in_order
      left_child.in_order { |value| yield value } if left_child
      yield value
      right_child.in_order { |value| yield value } if right_child
    end

    def delete(number)
      if number < value
        if left_child && left_child.value == number
          if left_child.left_child && left_child.right_child
            left_childs_right_child = left_child.right_child
            @left_child = left_child.left_child

            left_child.insert(left_childs_right_child)
          elsif left_child.left_child
            @left_child = left_child.left_child
          elsif left_child.right_child
            @left_child = left_child.right_child
          else
            @left_child = nil
          end

          number
        elsif left_child
          left_child.delete(number)
        end
      else
        if right_child && right_child.value == number
          if right_child.left_child && right_child.right_child
            right_childs_left_child = right_child.left_child
            @right_child = right_child.right_child

            right_child.insert(right_childs_left_child)
          elsif right_child.left_child
            @right_child = right_child.left_child
          elsif right_child.right_child
            @right_child = right_child.right_child
          else
            @right_child = nil
          end

          number
        elsif right_child
          right_child.delete(number)
        end
      end
    end

    def string
      left_string = left_child && left_child.string
      right_string = right_child && right_child.string

      "(#{[left_string, value, right_string].compact.join(" ")})"
    end
  end
end
