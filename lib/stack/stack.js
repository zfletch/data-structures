// Stack (or LIFO Queue)
//
// Example:
//   stack = new Stack();
//   stack.push("first");
//   stack.push("second");
//   stack.push("last");
//
//   stack.pop(); # => "last"
//   stack.pop(); # => "second"
//   stack.pop(); # => "first"
//   stack.pop(); # => null

var Stack;

(function () {
  'use strict';

  Stack = function () {
    this.head = null;
  };

  Stack.prototype.empty = function () {
    return this.head === null;
  };

  Stack.prototype.push = function (value) {
    this.head = { value: value, next: this.head };

    return this;
  };

  Stack.prototype.pop = function () {
    var stackValue;

    if (this.head === null) {
      return null;
    }

    stackValue = this.head.value;
    this.head = this.head.next;

    return stackValue;
  };
})();
