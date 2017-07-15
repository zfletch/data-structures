var BinaryTree;

(function () {
  'use strict';

  BinaryTree = function () {
    this.root = null;
  };

  BinaryTree.prototype.insert = function (value) {
    var node = { value: value };

    if (this.empty()) {
      this.root = node;
    } else {
      insert(this.root, node);
    }

    return this;
  };

  BinaryTree.prototype.empty = function () {
    return !this.root;
  };

  var insert = function (root, node) {
    if (root.value > node.value) {
      root.left ? insert(root.left, node) : root.left = node
    } else if (root.value < node.value) {
      root.right ? insert(root.right, node) : root.right = node
    }
  };
})();
