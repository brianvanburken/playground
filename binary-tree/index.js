class Tree {
  constructor() {
    this.root = null;
  }

  addNode(node) {
    if (this.root === null) {
      this.root = node;
    } else {
      this.root.addNode(node);
    }
  }

  traverse() {
    if (this.root !== null) {
      return this.root.traverse([]);
    } else {
      return [];
    }
  }

  search(value) {
    if (this.root !== null) {
      return this.root.search(value);
    } else {
      return null;
    }
  }

  // TODO: remove
  // TODO: get height
  // TODO: balance
}

class Node {

  constructor(value) {
    this.value = value;
    this.left = null;
    this.right = null;
  }

  addNode(node) {
    if (node.value < this.value || node.value === this.value) {
      if (this.left === null) {
        this.left = node;
      } else {
        this.left.addNode(node);
      }
    } else if (node.value > this.value) {
      if (this.right === null) {
        this.right = node;
      } else {
        this.right.addNode(node);
      }
    }
  }

  traverse(accumulator) {
    if (this.left !== null) {
      accumulator = this.left.traverse(accumulator);
    }

    accumulator.push(this.value);

    if (this.right !== null) {
      accumulator = this.right.traverse(accumulator);
    }

    return accumulator;
  }

  search(value) {
    if (this.value === value) {
      return this.value;
    }
    if (this.left !== null && value < this.value) {
      return this.left.search(value);
    }
    if (this.right !== null && value > this.value) {
      return this.right.search(value);
    }
    return null;
  }
}


function example() {
  const tree = new Tree();

  tree.addNode( new Node( 15 ) );
  tree.addNode( new Node( 20 ) );
  tree.addNode( new Node( 10 ) );
  tree.addNode( new Node( 16 ) );
  tree.addNode( new Node( 8 ) );
  tree.addNode( new Node( 12 ) );
  tree.addNode( new Node( 25 ) );

  return tree;
}

module.exports = { Tree, Node, example };
