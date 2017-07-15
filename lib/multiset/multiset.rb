class Multiset
  def initialize
    @hash = {}
  end

  def inc(key, count: 1)
    return self if count < 1

    hash[key] ||= Entry.new(key)
    hash[key].count += count

    self
  end

  def dec(key, count: 1)
    return if count < 1

    if entry = hash[key]
      entry.count -= count
      hash.delete(key) if entry.count < 0

      key
    end
  end

  def count(key)
    (hash[key] && hash[key].count) || 0
  end

  def union(multiset)
    create_set(multiset) { |a, b| [a, b].max }
  end

  def intersection(multiset)
    create_set(multiset) { |a, b| [a, b].min }
  end

  def sum(multiset)
    create_set(multiset, &:+)
  end

  def difference(multiset)
    Multiset.new.tap do |result|
      keys.each { |key| result.inc(key, count: count(key) - multiset.count(key)) }
    end
  end

  def subset?(multiset)
    keys.all? { |key| count(key) <= multiset.count(key) }
  end

  def superset?(multiset)
    multiset.subset?(self)
  end

  def ==(multiset)
    return false unless multiset.is_a?(Multiset)

    superset?(multiset) && subset?(multiset)
  end

  def inspect
    "{#{ keys.flat_map { |key| count(key).times.map { key.inspect } }.join(", ") }}"
  end

  protected

  def keys
    hash.keys
  end

  private

  attr_reader :hash

  def create_set(multiset)
    Multiset.new.tap do |result|
      keys.each { |key| result.inc(key, count: yield(count(key), multiset.count(key))) }

      multiset.keys.each do |key|
        result.inc(key, count: yield(count(key), multiset.count(key))) if result.count(key).zero?
      end
    end
  end

  class Entry
    attr_reader :key
    attr_accessor :count

    def initialize(key)
      @key = key
      @count = 0
    end
  end
end
