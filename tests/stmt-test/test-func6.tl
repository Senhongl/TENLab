e = 100;
def foo(a, b) {
    d = a+e;
	if (e < 5) {
		return 10;
	} else {
		return 88;
	}
    return e;
}
e = 0;

f = foo(1, 2);
print(f);