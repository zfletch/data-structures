(function () {
  'use strict';

  var ArrayStack = function () {
    this.index = 0;
    this.array = [];
  };

  ArrayStack.prototype.isEmpty = function () {
    return !this.index;
  };

  ArrayStack.prototype.push = function (value) {
    this.array[this.index++] = value;

    return this;
  };

  ArrayStack.prototype.pop = function () {
    if (this.isEmpty()) return null;

    return this.array[--this.index];
  };

  module.exports = ArrayStack;
})();
