var ArrayStack = require("array-stack/array_stack.js");

describe(ArrayStack, function () {
  beforeEach(function () {
    this.stack = new ArrayStack();
  });
  
  describe("isEmpty", function () {
    it("is empty", function () {
      expect(this.stack.isEmpty()).toBeTruthy();
    });

    it("is not empty", function () {
      this.stack.push(5);

      expect(this.stack.isEmpty()).toBeFalsy();
    });
  });

  describe("pop", function () {
    beforeEach(function () {
      for (var ii = 0; ii < 10; ii++) {
        this.stack.push(ii);
      }
    });

    it("can pop off all of the values", function () {
      var acc = [];
      for (var ii = 0; ii < 10; ii++) {
        acc.push(this.stack.pop());
      }

      expect(acc).toEqual([9, 8, 7, 6, 5, 4, 3, 2, 1, 0]);
      expect(this.stack.pop()).toBeNull();
    });
  });

  describe("push", function () {
    it("pushes", function () {
      this.stack.push(1);

      expect(this.stack.isEmpty()).toBeFalsy();
    });
  });
});
