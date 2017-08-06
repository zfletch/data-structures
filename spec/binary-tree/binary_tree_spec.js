var BinaryTree = require("binary-tree/binary_tree.js");
var _ = require('lodash');

describe(BinaryTree, function () {
  beforeEach(function () {
    this.size = 100;
    this.tree = new BinaryTree();
  });

  describe("isEmpty", function () {
    it("is empty", function () {
      expect(this.tree.isEmpty()).toBeTruthy();
    });

    it("is not empty", function () {
      this.tree.insert(5);

      expect(this.tree.isEmpty()).toBeFalsy();
    });
  });

  describe("exists", function () {
    beforeEach(function () {
      var tree = this.tree;

      _.shuffle(_.range(0, this.size)).forEach(function (n) {
        tree.insert(n);
      });
    });

    it("contains the value", function () {
      expect(this.tree.exists(this.size - 1)).toBeTruthy();
    });

    it("does not contain the value", function () {
      expect(this.tree.exists(this.size)).toBeFalsy();
    });
  });

  describe("insert", function () {
    beforeEach(function () {
      this.tree.insert(1);
    });

    it("contains the value", function () {
      expect(this.tree.exists(1)).toBeTruthy();
    });

    it("does not contain the value", function () {
      expect(this.tree.exists(2)).toBeFalsy();
    });
  });

  describe("inOrder", function () {
    beforeEach(function () {
      var tree = this.tree;

      _.shuffle(_.range(0, this.size)).forEach(function (n) {
        tree.insert(n);
      });
    });

    it("visits all of the nodes in order", function () {
      var acc = [];
      this.tree.inOrder(function (n) { acc.push(n); });

      expect(acc).toEqual(_.range(0, this.size));
    });
  });

  describe("delete", function () {
    beforeEach(function () {
      var tree = this.tree;

      _.shuffle(_.range(0, this.size)).forEach(function (n) {
        tree.insert(n);
      });
    });

    it("returns the value it deleted", function () {
      expect(this.tree.delete(25)).toEqual(25);
      expect(this.tree.delete(110)).toBeNull();
    });

    it("deletes the values", function () {
      var tree = this.tree;
      var array = _.range(0, this.size);

      _.shuffle(_.range(0, this.size)).forEach(function (d) {
        tree.delete(d);

        var acc = [];
        tree.inOrder(function (n) { acc.push(n); });

        array = _.reject(array, function (n) { return n === d; });
        expect(acc).toEqual(array);
      });

      expect(this.tree.isEmpty()).toBeTruthy();
    });
  });
});
