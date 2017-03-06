class Tree
  def initialize
    @root = nil
  end

  def insert(number)
    node = Node.new(number)

    if root
      root.insert(node)
    else
      @root = node
    end

    self
  end

  def delete(number)
    return unless root

    if root.value == number
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

      number
    else
      root.delete(number)
    end
  end

  def find(number)
    return unless root

    root.find(number)
  end

  def inorder
    return unless root

    root.inorder { |value| yield value }
  end

  def preorder
    return unless root

    root.preorder { |value| yield value }
  end

  def postorder
    return unless root

    root.postorder { |value| yield value }
  end

  def inspect
    "(#{root && root.string || ""})"
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

    def find(number)
      if value == number
        number
      elsif value < number && left_child
        left_child.find(number)
      elsif value > number && right_child
        right_child.find(number)
      end
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

    def inorder
      left_child.inorder { |value| yield value } if left_child
      yield value
      right_child.inorder { |value| yield value } if right_child
    end

    def preorder
      yield value
      left_child.preorder { |value| yield value } if left_child
      right_child.preorder { |value| yield value } if right_child
    end

    def postorder
      left_child.postorder { |value| yield value } if left_child
      right_child.postorder { |value| yield value } if right_child
      yield value
    end

    def string(count: 0)
      left_string = left_child && left_child.string(count: count + 1) || ""
      right_string = right_child && right_child.string(count: count + 1) || ""
      space = "  " * (count + 1)

      "#{value}\n#{space}(#{left_string})\n#{space}(#{right_string})"
    end
  end
end
