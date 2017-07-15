var LinkedList;

(function () {
  'use strict';

  LinkedList = function () {
    this.root = null;
  };

  LinkedList.prototype.insert = function (value) {
    var node = createNode(value);

    node.next = this.root;
    this.root = node;

    return this;
  };

  LinkedList.prototype.remove = function (value) {
    if (this.root === null) {
      return null;
    }

    if (this.root.value === value) {
      this.root = this.root.next;
      return value;
    }

    return removeNode(this.root, value);
  };

  LinkedList.prototype.find = function (value) {
    return this.root === null ? null : findNode(this.root, value);
  };

  LinkedList.prototype.each = function (callback) {
    eachNode(this.root, callback);
  };

  var createNode = function (value) {
    return { value: value, next: null };
  };

  var findNode = function (root, value) {
    return root === null ? null : (root.value === value ? value : findNode(root.next, value));
  };

  var removeNode = function (root, value) {
    if (root.next === null) {
      return null;
    }

    return root.next.value === value ? root.next = root.next.next : removeNode(root.next, value);
  };

  var eachNode = function (root, callback) {
    if (root === null) {
      return;
    }

    callback(root.value);
    eachNode(root.next, callback);
  };
})();
