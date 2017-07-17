class ArrayStack
  def initialize
    @array = []
    @index = 0
  end

  def empty?
    index.zero?
  end

  def push(val)
    array[index] = val
    @index += 1

    self
  end

  def pop
    if empty?
      nil
    else
      @index -= 1
      array[index]
    end
  end

  def inspect
    "/#{array.reverse.join(" ")}/"
  end

  private

  attr_reader :array, :index
end
