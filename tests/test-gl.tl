a = 1;
b = 1;
c = 2;
def foo(a) {
    a = 2;
    print(b);
    b = c;
    print(b);
}

foo(3);
print(a);
print(b);