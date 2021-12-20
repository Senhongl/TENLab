parallel_define et {
    overload __+__ (x,y) {
		map f1 {
			z = x[0]+y[0];
			return z;
		}
		map f2 {
			z = x[1]+y[1];
			return z;
		}
		reduce {
			return f1+f2;
		} 
	}
}

using et;
print([1,1]+[2,2]);