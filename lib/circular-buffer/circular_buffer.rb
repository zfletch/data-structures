class CircularBuffer
  def initialize(capacity)
    @capacity = capacity
    @buffer = [0] * capacity
    @read = 0
    @write = 0
  end

  def enqueue(num)
    buffer[write] = num
    @write = inc(write)

    self
  end

  def dequeue
    result = buffer[read]
    @read = inc(read)

    result
  end

  def inspect
    "[#{string}]"
  end

  private

  attr_reader :capacity, :buffer, :read, :write

  def inc(index)
    (index + 1) % capacity
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
