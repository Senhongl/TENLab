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
	overload __-__ (x,y) {
		map f1 {
			z = x+y;
			return z;
		}
		reduce {
			return f1;
		} 
	}
}

using et;
print("user defined environment:");
print(1+2);
print(1-2);
end et;
print("--------------------------");
print("default operation:");
print(1+2);
print(1-2);
