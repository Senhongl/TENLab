parallel_define fast_add {
    overload __+__ (x,y) {
		map f1 {
			l = shape(x);
			z = x[0:l[0]//2:1]+y[0:l[0]//2:1];
			return z;
		}
		map f2 {
			l = shape(x);
			z = x[l[0]//2:l[0]:1]+y[l[0]//2:l[0]:1];
			return z;
		}
		reduce {
			return cat(f1,f2,0);
		} 
	}
}


a = zeros([20]);
b = ones([20]);
using fast_add;
c = a+b;
print(c);







