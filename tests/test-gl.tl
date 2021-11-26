a = 1;
b = 1;
c = 2;
def foo(a) {
    a = 2;
    print(b);
    b = c;
    print(b);
}

foo(a);
print(a);
print(b);