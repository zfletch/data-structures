require 'circular-buffer/circular_buffer'

RSpec.describe CircularBuffer do
  subject(:buffer) { CircularBuffer.new(12) }

  describe "#enqueue and #dequeue" do
    it "can enqueue a value" do
      buffer.enqueue(1)

      expect(buffer.dequeue).to eq(1)
    end

    it "wraps around" do
      (1..13).each { |n| buffer.enqueue(n) }

      expect(buffer.dequeue).to eq(13)
      (2..12).each { |n| expect(buffer.dequeue).to eq(n) }
    end
  end
end
