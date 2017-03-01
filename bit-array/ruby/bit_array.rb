class BitArray
  def initialize(array = 0)
    @array = array
  end

  def add(num)
    @array |= (1 << num)

    self
  end

  def remove(num)
    if member?(num)
      @array &= ~(1 << num)
      num
    end
  end

  def member?(num)
    (array & (1 << num)) != 0
  end

  def union(set)
    BitArray.new(array | set.array)
  end

  def intersection(set)
    BitArray.new(array & set.array)
  end

  def difference(set)
    BitArray.new(array & ~set.array)
  end

  def subset?(set)
    (array & ~set.array) == 0
  end

  def inspect
    "{#{ string }}"
  end

  protected

  attr_reader :array

  private

  def string
    [].tap do |acc|
      array.to_s(2).chars.reverse.each_with_index { |c, i| acc << i if c != "0"  }
    end.join(", ")
  end
end
