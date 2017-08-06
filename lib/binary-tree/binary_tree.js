(function () {
  'use strict';

  var BinaryTree = function () {
    this.root = null;
  };

  BinaryTree.prototype.isEmpty = function () {
    return !this.root;
  };

  BinaryTree.prototype.exists = function (value) {
    return _exists(this.root, value);
  };

  var _exists = function (root, value) {
    if (!root) return false;
    if (root.value === value) return true;

    return root.value > value ? _exists(root.left, value) : _exists(root.right, value);
  };

  BinaryTree.prototype.insert = function (value) {
    var node = { value: value };

    if (this.isEmpty()) {
      this.root = node;
    } else {
      _insert(this.root, node);
    }

    return this;
  };

  var _insert = function (root, node) {
    if (root.value > node.value) {
      if (root.left) {
        _insert(root.left, node);
      } else {
        root.left = node;
      }
    } else if (root.value < node.value) {
      if (root.right) {
        _insert(root.right, node);
      } else {
        root.right = node;
      }
    }
  };

  BinaryTree.prototype.inOrder = function (fn) {
    _inOrder(this.root, fn);
  };

  var _inOrder = function (root, fn) {
    if (!root) return;

    _inOrder(root.left, fn);
    fn(root.value);
    _inOrder(root.right, fn);
  };

  BinaryTree.prototype.delete = function (value) {
    if (this.isEmpty()) return null;

    var val = null;

    this.root = _delete(this.root, value, function (v) { val = v; });

    return val;
  };

  var _delete = function (root, value, callback) {
    if (!root) {
    } else if (root.value > value) {
      root.left = _delete(root.left, value, callback);
    } else if (root.value < value) {
      root.right = _delete(root.right, value, callback);
    } else {
      callback(root.value);

      if (!root.left && !root.right) {
        root = null;
      } else if (!root.left) {
        root = root.right;
      } else if (!root.right) {
        root = root.left;
      } else {
        root.value = _minimum(root.right).value;
        root.right = _delete(root.right, root.value, function () {});
      }
    }

    return root;
  };

  var _minimum = function (root) {
    return root.left ? _minimum(root.left) : root;
  };

  module.exports = BinaryTree;
})();
