a = [[1, 2, 3], [4, 5, 6]];
b = [[-1, -2, -3], [-4, -5, -6]];
c = a .* b;
print(c);
d = a .* 2;
print(d);
e = float_of(a) .* 0.2;
print(e);