require 'queue/queue'

RSpec.describe Queue do
  subject(:queue) { Queue.new }

  describe "#empty?" do
    it { should be_empty }

    context "the queue is not empty" do
      before { queue.push(5) }

      it { should_not be_empty }
    end
  end

  describe "#inspect" do
    specify { expect(queue.inspect).to eq("\\\\") }

    context "there are values in the queue" do
      before { [1, 2].each { |v| queue.push(v) } }

      specify { expect(queue.inspect).to eq("\\1 2\\") }
    end
  end

  describe "#shift" do
    before { 1.upto(10) { |v| queue.push(v) } }

    it "can shift off all of the values" do
      acc = []
      10.times { acc << queue.shift }

      expect(acc).to eq(1.upto(10).to_a)
      expect(queue.shift).to be_nil
    end
  end

  describe "#push" do
    before { queue.push(1) }

    it { should_not be_empty }
  end
end

