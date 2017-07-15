class Heap
  def initialize
    @array = []
    @last_index = -1
  end

  def insert(number)
    @last_index += 1
    array[last_index] = number

    bubble_up(last_index)

    self
  end

  def find_min
    return if empty?

    array[0]
  end

  def delete_min
    return if empty?

    result = find_min
    array[0] = array[last_index]
    @last_index -= 1

    bubble_down(0) unless empty? 

    result
  end

  def empty?
    last_index == -1
  end

  def inspect
    "[#{0.upto(last_index).map { |index| array[index] }.join(", ")}]"
  end

  private

  attr_reader :array, :last_index

  def bubble_up(index)
    return if index == 0

    parent_index = (index - 1) / 2

    if array[index] < array[parent_index]
      swap(index, parent_index)
      bubble_up(parent_index)
    end
  end

  def bubble_down(index)
    return if index == last_index

    left_index = index * 2 + 1
    right_index = left_index + 1

    return if left_index > last_index

    if left_index == last_index
      swap(index, left_index) if array[left_index] < array[index]
    else
      if array[left_index] < array[index] && array[left_index] < array[right_index]
        swap(index, left_index)
        bubble_down(left_index)
      elsif array[right_index] < array[index]
        swap(index, right_index)
        bubble_down(right_index)
      end
    end
  end

  def swap(index1, index2)
    array[index1], array[index2] = array[index2], array[index1]
  end
end
