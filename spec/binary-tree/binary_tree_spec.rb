require 'binary-tree/binary_tree'

RSpec.describe BinaryTree do
  let(:size) { 100 }

  subject(:tree) { BinaryTree.new }

  describe "#empty?" do
    it { should be_empty }

    context "the tree is not empty" do
      before { tree.insert(5) }

      it { should_not be_empty }
    end
  end

  describe "#exists?" do
    before { (1..size).to_a.shuffle.each { |v| tree.insert(v) } }

    specify { expect(tree.exists?(size)).to be_truthy }
    specify { expect(tree.exists?(size + 1)).to be_falsey }
  end

  describe "#insert" do
    before { tree.insert(1) }

    specify { expect(tree.exists?(1)).to be_truthy }
    specify { expect(tree.exists?(2)).to be_falsey }
  end

  describe "#in_order" do
    before { (1..size).to_a.shuffle.each { |v| tree.insert(v) } }

    it "visits all of the nodes in order" do
      acc = []
      tree.in_order { |v| acc << v }

      expect(acc).to eq((1..size).to_a)
    end
  end

  describe "#delete" do
    before { (1..size).to_a.shuffle.each { |v| tree.insert(v) } }

    it "returns the value it deletes" do
      expect(tree.delete(size)).to eq(size)
      expect(tree.delete(size + 1)).to be_nil
    end

    it "deletes the values" do
      deleted = (1..size).to_a.shuffle
      array = (1..size).to_a

      deleted.each do |d|
        tree.delete(d)

        acc = []
        tree.in_order { |v| acc << v }

        array.delete(d)

        if acc != array
          binding.pry
        end

        expect(acc).to eq(array)
      end

      expect(tree).to be_empty
    end
  end

  describe "#inspect" do
    specify { expect(tree.inspect).to eq("()") }

    context "there are values in the tree" do
      before { [5, 0, 10, 8, 12].each { |v| tree.insert(v) } }

      specify { expect(tree.inspect).to eq("((0) 5 ((8) 10 (12)))") }
    end
  end

  describe "Enumerable" do
    before { (1..size).to_a.shuffle.each { |v| tree.insert(v) } }

    specify { expect(tree.take(3)).to eq([1,2,3]) }
  end
end
