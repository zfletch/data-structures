class UList
  def initialize(size)
    @node_class = self.class.generate_node_class(size)
    @head = nil
  end

  def insert(value)
    @head = node_class.new unless head

    head.insert(value)

    self
  end

  def delete(value)
  end

  def inspect
    return "[]" unless head

    "[#{head.string}]"
  end

  private

  attr_reader :node_class, :head

  def self.generate_node_class(size)
    Class.new do
      attr_accessor :values, :next_node, :index

      def initialize
        @values = []
        @index = 0
        @next_node = nil
      end

      define_method(:insert) do |value| # closure so we have access to `size`
        if index < size
          values[index] = value
          @index += 1
        elsif next_node
          next_node.insert(value)
        else
          @next_node = self.class.new.insert(value)
        end

        self
      end

      def each
        (0...index).each do |iteration_index|
          yield values[iteration_index]
        end

        next_node.each { |value| yield value } if next_node
      end

      define_method(:string) do
        node_string = "<#{ (0...size).map { |i| i < index ? values[i] : '-' }.join(", ") }>"
        node_string += " #{next_node.string}" if next_node

        node_string
      end
    end
  end
end
