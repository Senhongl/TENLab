tree = var [nil, 0, nil]; # [left, value, right]
tree[0] = var [var [nil, 1, nil], 2, var [nil, 4, var [nil, 5, nil]]];
tree[2] = var [nil, 6, nil];

def preorder(t) {
    if (t != nil) {
        print(t[1]);
        preorder(t[0]);
        preorder(t[2]);
    }
    return 1;
}

print("Pre-Order");
preorder(tree);