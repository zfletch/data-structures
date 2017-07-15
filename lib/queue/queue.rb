# Stack (or FIFO Queue)
#
# Example:
#   queue = Queue.new
#   queue.unshift "first"
#   queue.unshift "second"
#   queue.unshift "last"
#
#   queue.shift # => "first"
#   queue.shift # => "second"
#   queue.shift # => "last"
#   queue.shift # => nil

class Queue
  def initialize
    @first_node = nil
    @last_node = nil
  end

  def unshift(data)
    node = Node.new(data)
    last_node.next_node = node if last_node
    @first_node = node unless first_node
    @last_node = node

    self
  end

  def shift
    return unless first_node

    data = first_node.data
    if first_node == last_node
      @first_node = @last_node = nil
    else
      @first_node = first_node.next_node
    end

    data
  end

  def to_s
    "/\\\n#{string}\\/"
  end

  private

  attr_reader :first_node, :last_node

  def string(node = first_node)
    return "" unless node

    "#{node.data}\n" << string(node.next_node)
  end

  class Node
    attr_accessor :next_node
    attr_reader :data

    def initialize(data)
      @data = data
    end
  end
end
