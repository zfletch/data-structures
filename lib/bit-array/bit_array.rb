# Bitarray
#
# Example:
#   bit_array = BitArray.new
#   bit_array.add(1)
#   bit_array.add(2)
#   bit_array.add(3)
#
#   bit_array.remove(2) # => 2
#   bit_array.remove(2) # => nil
#
#   bit_array.member?(1) # => true
#   bit_array.member?(2) # => false
#   bit_array.member?(7) # => false
#
#   other_bit_array = BitArray.new
#   other_bit_array.add(1)
#   other_bit_array.add(4)
#
#   union = bit_array.union(other_bit_array)
#   union.member?(1) # => true
#   union.member?(3) # => true
#   union.member?(4) # => true
#   union.member?(5) # => false
#
#   intersection = bit_array.intersection(other_bit_array)
#   intersection.member?(1) # => true
#   intersection.member?(3) # => false
#   intersection.member?(4) # => false
#
#   difference = bit_array.difference(other_bit_array)
#   difference.member?(1) # => false
#   difference.member?(3) # => true
#   difference.member?(4) # => false
#
#   subset = BitArray.new
#   subset.add(1)
#
#   bit_array.subset?(other_bit_array) # => false
#   bit_array.subset?(subset) # => false
#   subset.subset?(bit_array) # => true

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
