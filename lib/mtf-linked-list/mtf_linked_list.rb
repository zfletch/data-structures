class MtfLinkedList
  def initialize
    @first_node = nil
  end

  def insert(data)
    @first_node = Node.new(data, first_node)

    self
  end

  def delete(data, node = first_node)
    return unless node

    if node.data == data && node == first_node
      @first_node = node.next_node

      data
    elsif next_node = node.next_node
      if next_node.data == data
        node.next_node = next_node.next_node

        data
      else
        delete(data, next_node)
      end
    end
  end

  def find(data, node = first_node)
    return unless node

    if node.data == data && node == first_node
      data
    elsif next_node = node.next_node
      if next_node.data == data
        node.next_node = next_node.next_node
        next_node.next_node = first_node
        @first_node = next_node

        data
      else
        find(data, next_node)
      end
    end
  end

  def to_s
    "<#{string}>"
  end

  private

  attr_reader :first_node

  def string(node = first_node)
    return "" unless node
    return "#{node.data}" unless node.next_node

    "#{node.data}, " << string(node.next_node)
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
