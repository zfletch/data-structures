class CircularBuffer
  def initialize(size)
    @size = size += 1
    @buffer = [0] * size
    @buffer_start = 0
    @buffer_end = 0
  end

  def enqueue(num)
    return if full?

    buffer[buffer_end] = num
    @buffer_end = inc(buffer_end)

    self
  end

  def dequeue
    return if empty?

    num = buffer[buffer_start]
    @buffer_start = inc(buffer_start)

    num
  end

  def empty?
    buffer_start == buffer_end
  end

  def full?
    inc(buffer_end) == buffer_start
  end

  def inspect
    "[#{string}]"
  end

  private

  attr_reader :size, :buffer, :buffer_start, :buffer_end

  def inc(index)
    index += 1
    index = 0 if index == size

    index
  end

  def string
    buffer.map.with_index do |num, index|
      if index == buffer_start && index == buffer_end
        "<> #{ num }"
      elsif index == buffer_start
        "< #{ num }"
      elsif index == buffer_end
        "> #{ num }"
      else
        num
      end
    end.join(" ")
  end
end
