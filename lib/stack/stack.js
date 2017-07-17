(function () {
  'use strict';

  var Stack = function () {
    this.head = null;
  };

  Stack.prototype.isEmpty = function () {
    return this.head === null;
  };

  Stack.prototype.push = function (value) {
    this.head = { value: value, next: this.head };

    return this;
  };

  Stack.prototype.pop = function () {
    var stackValue;

    if (this.isEmpty()) return null;

    stackValue = this.head.value;
    this.head = this.head.next;

    return stackValue;
  };

  module.exports = Stack;
})();
