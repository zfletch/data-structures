require 'binary-tree/binary_tree'

RSpec.describe BinaryTree do
  subject(:tree) { BinaryTree.new }

  describe "#delete" do
    let(:deleted) { [54, 18, 12] }

    before do
      (1..100).to_a.shuffle.each { |v| tree.insert(v) }
      deleted.each { |v| tree.delete(v) }
    end

    it "deletes the values" do
      acc = []
      tree.in_order { |v| acc << v }

      expect(acc).to eq((1..100).to_a - deleted)
    end
  end

  describe "#empty?" do
    it { should be_empty }

    context "the tree is not empty" do
      before { tree.insert(5) }

      it { should_not be_empty }
    end
  end

  describe "#exists?" do
    before { (1..100).to_a.shuffle.each { |v| tree.insert(v) } }

    specify { expect(tree.exists?(100)).to be_truthy }
    specify { expect(tree.exists?(101)).to be_falsey }
  end

  describe "#insert" do
    before { tree.insert(1) }

    specify { expect(tree.exists?(1)).to be_truthy }
    specify { expect(tree.exists?(2)).to be_falsey }
  end

  describe "#inspect" do
    specify { expect(tree.inspect).to eq("()") }

    context "there are values in the tree" do
      before { [5, 0, 10, 8, 12].each { |v| tree.insert(v) } }

      specify { expect(tree.inspect).to eq("((0) 5 ((8) 10 (12)))") }
    end
  end

  describe "#in_order" do
    before { (1..100).to_a.shuffle.each { |v| tree.insert(v) } }

    it "visits all of the nodes in order" do
      acc = []
      tree.in_order { |v| acc << v }

      expect(acc).to eq((1..100).to_a)
    end
  end

  describe "Enumerable" do
    before { (1..100).to_a.shuffle.each { |v| tree.insert(v) } }

    specify { expect(tree.take(3)).to eq([1,2,3]) }
  end
end
