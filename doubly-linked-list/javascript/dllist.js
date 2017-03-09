var DLList;

(function () {
  'use strict';

  DLList = function () {
    this.head = null;
    this.tail = null;
  };

  DLList.prototype.insertLeft = function (value) {
    var node = createNode(value);

    node.next = this.head;
    if (this.head) {
      this.head.previous = node;
    }
    this.head = node;
    if (this.tail === null) {
      this.tail = node;
    }

    return this;
  };

  DLList.prototype.insertRight = function (value) {
    var node = createNode(value);

    node.previous = this.tail;
    if (this.tail) {
      this.tail.next = node;
    }
    this.tail = node;
    if (this.head === null) {
      this.head = node;
    }

    return this;
  };

  DLList.prototype.eachLeft = function (callback) {
    eachNodeLeft(this.head, callback);
  };

  DLList.prototype.eachRight = function (callback) {
    eachNodeRight(this.tail, callback);
  };

  var createNode = function (value) {
    return { value: value, next: null, previous: null };
  };

  var eachNodeLeft = function (head, callback) {
    if (head === null) {
      return;
    }

    callback(head.value);
    eachNodeLeft(head.next, callback);
  };

  var eachNodeRight = function (tail, callback) {
    if (tail === null) {
      return;
    }

    callback(tail.value);
    eachNodeRight(tail.previous, callback);
  };
})();
