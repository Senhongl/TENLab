parallel_define et {
    overload __+__ (x,y) {
		map f1 {
			z = x+y;
			return z;
		}
		map f2 {
			z = x+y;
			return z;
		}
		reduce {
			return f1+f2;
		} 
	}
}

using et;
print(1+2);
end et;
print(1+2);
