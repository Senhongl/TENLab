parallel_define Strassen {
    overload __*__ (x, y) {
        map f1 {
            l1 = shape(x);
            l2 = shape(y);
			x11 = x[0:l1[0]//2:1,0:l1[1]//2:1];
			x22 = x[l1[0]//2:l1[0]:1,l1[1]//2:l1[1]:1];
			y11 = y[0:l2[0]//2:1,0:l2[1]//2:1];
			y22 = y[l2[0]//2:l2[0]:1,l2[1]//2:l2[1]:1];
            return (x11+x22)*(y11+y22);
        }
        map f2 {
            l1 = shape(x);
            l2 = shape(y);
			x21 = x[l1[0]//2:l1[0]:1,0:l1[1]//2:1];
			x22 = x[l1[0]//2:l1[0]:1,l1[1]//2:l1[1]:1];
			y11 = y[0:l2[0]//2:1,0:l2[1]//2:1];
            return (x21+x22)*y11;
        }
        map f3 {
            l1 = shape(x);
            l2 = shape(y);
			x11 = x[0:l1[0]//2:1,0:l1[1]//2:1];
			y12 = y[0:l2[0]//2:1,l2[1]//2:l2[1]:1];
			y22 = y[l2[0]//2:l2[0]:1,l2[1]//2:l2[1]:1];
            return x11*(y12-y22);
        }
        map f4 {
            l1 = shape(x);
            l2 = shape(y);
			x22 = x[l1[0]//2:l1[0]:1,l1[1]//2:l1[1]:1];
			y11 = y[0:l2[0]//2:1,0:l2[1]//2:1];
			y21 = y[l2[0]//2:l2[0]:1,0:l2[1]//2:1];
            return x22*(y21-y11);
        }
        map f5 {
            l1 = shape(x);
            l2 = shape(y);
			x11 = x[0:l1[0]//2:1,0:l1[1]//2:1];
			y22 = y[l2[0]//2:l2[0]:1,l2[1]//2:l2[1]:1];
			x12 = x[0:l1[0]//2:1,l1[1]//2:l1[1]:1];
            return (x11+x12)*y22;
        }
        map f6 {
            l1 = shape(x);
            l2 = shape(y);
			x21 = x[l1[0]//2:l1[0]:1,0:l1[1]//2:1];
			y11 = y[0:l2[0]//2:1,0:l2[1]//2:1];
			x11 = x[0:l1[0]//2:1,0:l1[1]//2:1];
			y12 = y[0:l2[0]//2:1,l2[1]//2:l2[1]:1];
            return (x21-x11)*(y11+y12);
        }
        map f7 {
            l1 = shape(x);
            l2 = shape(y);
			x22 = x[l1[0]//2:l1[0]:1,l1[1]//2:l1[1]:1];
			y22 = y[l2[0]//2:l2[0]:1,l2[1]//2:l2[1]:1];
			y21 = y[l2[0]//2:l2[0]:1,0:l2[1]//2:1];
			x12 = x[0:l1[0]//2:1,l1[1]//2:l1[1]:1];
            return (x12-x22)*(y21+y22);
        }
        reduce {
            c11 = f1+f4-f5+f7;
            c12 = f3+f5;
            c21 = f2+f4;
            c22 = f1-f2+f3+f6;
            return cat(cat(c11,c12,0),cat(c21,c22,0),1);
        }
    }
}

using Strassen;

# create data
num_of_dimensions = 8192;
x_train = rand([20000, 8192]);
b_true = 1.;
w_true = rand([8192]) .* 3.;
y_train = x_train * w_true + b_true;

# random init
w = rand([8192]);
b = 0.;

# Mean Square Error loss function
def loss() {
    y_test = x_train * w + b;
    error = sum((y_test - y_train).^2 .* 0.5);

    return error / float_of(num_of_dimensions);
}

# Derivative of Loss w.r.t W
def dLdW() {
    y_test = x_train * w + b;
    dLdw = (y_test - y_train) * x_train / float_of(num_of_dimensions);
    
    return dLdw;
}

# Derivative of Loss w.r.t W
def dLdB() {
    y_test = x_train * w + b;
    dLdb = sum(y_test - y_train) / float_of(num_of_dimensions);

    return dLdb;
}

# training
lr = 0.0001;
for (epoch in 0:100:1) {
    print("*****************");
    print(loss());
    w = w - dLdW() .* lr;
    b = b - dLdB() .* lr;
}