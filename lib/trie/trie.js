var Trie;

(function () {
  'use strict';

  Trie = function () { };

  Trie.prototype.set = function (key, value) {
    var first, rest;

    if (!key.length) {
      this.value = value;
    } else {
      first = key[0];
      rest = key.slice(1);

      this.tries[first] = this.tries[first] || new Trie();
      this.tries[first].set(rest, value);
    }

    return this;
  };

  Trie.prototype.get = function (key) {
    var first, rest;

    if (!key.length) return this.value;

    first = key[0];
    rest = key.slice(1);

    if (this.tries[first]) return this.tries[first].get(rest);
  };
})();
