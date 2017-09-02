require 'bit-array/bit_array'

RSpec.describe BitArray do
  subject(:bit_array) { BitArray.new }

  describe "#empty?" do
    it { should be_empty }

    context "the bit array is not empty" do
      before { bit_array.add(1) }

      it { should_not be_empty }
    end
  end

  describe "#add" do
    before { bit_array.add(5) }

    it { should_not be_empty }

    it "now has that number as a member" do
      expect(bit_array.member?(5)).to be_truthy
    end

    it "does not have other numbers as members" do
      expect(bit_array.member?(10)).to_not be_truthy
    end
  end

  describe "#remove" do
    before { 1.upto(10) { |n| bit_array.add(n) } }

    it "can remove numbers" do
      1.upto(10) do |n|
        expect(bit_array.member?(n)).to be_truthy
        expect(bit_array.remove(n)).to eq(n)
        expect(bit_array.member?(n)).to be_falsey
      end
    end

    it "returns nil when the number is not a member" do
      expect(bit_array.remove(55)).to be_nil
    end
  end

  describe "#member?" do
    before { bit_array.add(10) }

    it "is truthy when the number is a member" do
      expect(bit_array.member?(10)).to be_truthy
    end

    it "is falsey when the number is not a member" do
      expect(bit_array.member?(9)).to be_falsey
    end
  end

  describe "#==" do
    let(:equal) { BitArray.new }
    let(:unequal) { BitArray.new }

    before do
      bit_array.add(1)
      equal.add(1)
      unequal.add(2)
    end

    it "is truthy when two bit arrays have the same members" do
      expect(bit_array == bit_array).to be_truthy
      expect(bit_array == equal).to be_truthy
      expect(bit_array != bit_array).to be_falsey
      expect(bit_array != equal).to be_falsey
    end

    it "is falsey when two bit arrays have different members" do
      expect(bit_array == unequal).to be_falsey
      expect(bit_array != unequal).to be_truthy
    end

    it "is falsey when a bit array is compared to another object" do
      expect(bit_array == "string").to be_falsey
      expect(bit_array != "string").to be_truthy
    end
  end

  describe "#subset" do
    let(:subset) { BitArray.new }

    before do
      bit_array.add(4)
      bit_array.add(5)
      subset.add(4)
    end

    it "is truthy when the array is a subset of another bit array" do
      expect(subset.subset?(bit_array)).to be_truthy
      expect(BitArray.new.subset?(bit_array)).to be_truthy
    end

    it "is truthy when the array is a strict subset of another bit array" do
      expect(bit_array.subset?(bit_array)).to be_truthy
    end

    it "is falsey when the array is not a subset of another bit array" do
      expect(bit_array.subset?(subset)).to be_falsey
      expect(bit_array.subset?(BitArray.new)).to be_falsey
    end
  end

  describe "#union" do
    let(:union) { BitArray.new }
    let(:other) { BitArray.new }

    before do
      bit_array.add(1)
      union.add(1)
      other.add(2)
      union.add(2)
    end

    it "returns a new bit array that is a union of the two" do
      expect(bit_array).to_not be ==(union)
      expect(other).to_not be ==(union)
      expect(bit_array.union(other)).to be ==(union)
      expect(other.union(bit_array)).to be ==(union)
    end
  end

  describe "#intersection" do
    let(:intersection) { BitArray.new }
    let(:other) { BitArray.new }

    before do
      bit_array.add(1)
      other.add(2)
      bit_array.add(3)
      other.add(3)
      intersection.add(3)
    end

    it "returns a new bit array that is a intersection of the two" do
      expect(bit_array).to_not be ==(intersection)
      expect(other).to_not be ==(intersection)
      expect(bit_array.intersection(other)).to be ==(intersection)
      expect(other.intersection(bit_array)).to be ==(intersection)
    end
  end

  describe "#difference" do
    let(:difference) { BitArray.new }
    let(:other) { BitArray.new }

    before do
      bit_array.add(1)
      other.add(1)
      bit_array.add(2)
      difference.add(2)
      other.add(4)
    end

    it "returns a new bit array that is a intersection of the two" do
      expect(bit_array).to_not be ==(difference)
      expect(other).to_not be ==(difference)
      expect(bit_array.difference(other)).to be ==(difference)
      expect(other.difference(bit_array)).to_not be ==(difference)
    end
  end

  describe "#inspect" do
    it "is {} for an empty bit array" do
      expect(bit_array.inspect).to eq("{}")
    end

    context "there are numbers in the bit array" do
      before do
        bit_array.add(1)
        bit_array.add(2)
      end

      it "shows all numbers in the bit array" do
        expect(bit_array.inspect).to eq("{1, 2}")
      end
    end
  end
end
