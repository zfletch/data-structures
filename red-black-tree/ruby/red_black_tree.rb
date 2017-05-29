# Red-black tree
#   A self-balancing binary search tree
#
# Examples:
#   rbtree = RedBlackTree.new
#   rbtree.insert(5)
#   rbtree.insert(10)
#   rbtree.insert(15)
#   rbtree.insert(20)
#   rbtree.find(5) # => 5
#   rbtree.find(20) # => 20
#   rbtree.find(11) # => nil
#
#   rbtree = RedBlackTree.new
#   nums = (0..1000).to_a.shuffle
#   nums.each { |num| rbtree.insert(num) }
#   arr = []; rbtree.inorder { |n| arr << n }
#   arr == nums.sort # => true

class RedBlackTree
  def initialize
    @root = nil
  end

  def insert(value)
    node = Node.new(value)

    if root
      root.insert(node)
    else
      @root = node
    end

    balance!(node)

    self
  end

  def find(value)
    return nil unless root

    root.find(value)
  end

  def inorder
    return unless root

    root.inorder { |node| yield node.value }
  end

  def inspect
    "(#{root && root.string || ""})"
  end

  private

  attr_reader :root

  def rotate_left!(node)
    rotate!(node, left: true)
  end

  def rotate_right!(node)
    rotate!(node, left: false)
  end

  def root?(node)
    @root == node
  end

  def balance!(node)
    if node.root?
      node.black!
    elsif node.parent.black?
    elsif node.parent.red? && node.uncle && node.uncle.red?
      node.parent.black!
      node.uncle.black!
      node.grandparent.red!

      balance!(node.grandparent)
    else
      if node.right_child_of_parent? && node.parent.left_child_of_parent?
        rotate_left!(node)
        node = node.left_child
      elsif node.left_child_of_parent? && node.parent.right_child_of_parent?
        rotate_right!(node)
        node = node.right_child
      end

      node.parent.black!
      node.grandparent.red!
      if node.left_child_of_parent?
        rotate_right!(node.parent)
      else
        rotate_left!(node.parent)
      end
    end
  end

  def rotate!(node, left:)
    previous_parent = node.parent
    previous_grandparent = node.grandparent
    previous_child = left ? node.left_child : node.right_child

    if previous_grandparent
      if previous_parent.left_child_of_parent?
        previous_grandparent.left_child = node
      else
        previous_grandparent.right_child = node
      end
    else
      @root = node
    end

    node.parent = previous_grandparent
    previous_parent.parent = node

    if left
      node.left_child = previous_parent
      previous_parent.right_child = previous_child
    else
      node.right_child = previous_parent
      previous_parent.left_child = previous_child
    end

    if previous_child
      previous_child.parent = previous_parent
    end

    self
  end

  class Node
    attr_accessor :value, :left_child, :right_child, :parent, :color

    def initialize(value, left_child: nil, right_child: nil, parent: nil, color: :red)
      @value = value
      @left_child = left_child
      @right_child = right_child
      @parent = parent
      @color = color
    end

    def insert(node)
      if node.value < value
        if left_child
          left_child.insert(node)
        else
          node.parent = self
          @left_child = node
        end
      else
        if right_child
          right_child.insert(node)
        else
          node.parent = self
          @right_child = node
        end
      end
    end

    def find(val)
      if val == value
        value
      elsif val < value && left_child
        left_child.find(val)
      elsif val > value && right_child
        right_child.find(val)
      else
        nil
      end
    end

    def string(count: 0)
      left_string = left_child && left_child.string(count: count + 1) || ""
      right_string = right_child && right_child.string(count: count + 1) || ""
      space = "  " * (count + 1)

      "#{value} : #{color}\n#{space}(#{left_string})\n#{space}(#{right_string})"
    end

    def inorder
      left_child.inorder { |node| yield node } if left_child
      yield self
      right_child.inorder { |node| yield node } if right_child
    end

    def root?
      parent.nil?
    end

    def black?
      color == :black
    end

    def black!
      @color = :black
    end

    def red?
      color == :red
    end

    def red!
      @color = :red
    end

    def grandparent
      parent.parent
    end

    def uncle
      parent.left_child_of_parent? ? grandparent.right_child : grandparent.left_child
    end

    def right_child_of_parent?
      parent.right_child == self
    end

    def left_child_of_parent?
      parent.left_child == self
    end
  end
end
