tree = var [123, 0, 123]; # [left, value, right]
tree[0] = var [var [123, 1, 123], 2, var [123, 4, var [123, 5, 123]]];
tree[2] = var [123, 6, 123];

def preorder(t) {
    if (t != 123) {
        print(t[1]);
        preorder(t[0]);
        preorder(t[2]);
    }
    return 1;
}

preorder(tree);