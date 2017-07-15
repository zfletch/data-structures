require 'stack/stack'

RSpec.describe Stack do
  subject(:stack) { Stack.new }

  describe "#empty?" do
    it { should be_empty }

    context "the stack is not empty" do
      before { stack.push(5) }

      it { should_not be_empty }
    end
  end

  describe "#inspect" do
    specify { expect(stack.inspect).to eq("//") }

    context "there are values in the stack" do
      before { [1, 2].each { |v| stack.push(v) } }

      specify { expect(stack.inspect).to eq("/2 1/") }
    end
  end

  describe "#pop" do
    before { 1.upto(10) { |v| stack.push(v) } }

    it "can pop off all of the values" do
      acc = []
      10.times { acc << stack.pop }

      expect(acc).to eq(1.upto(10).to_a.reverse)
      expect(stack.pop).to be_nil
    end
  end

  describe "#push" do
    before { stack.push(1) }

    it { should_not be_empty }
  end
end
