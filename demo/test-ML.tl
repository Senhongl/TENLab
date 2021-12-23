# create data
number_of_records = 1000;
x_train = rand([1000]) .* 100.;
b_true = 1.;
w_true = 0.4;
y_train = x_train .* w_true + b_true;

# initialize guess of weight and bias 
w = 0.;
b = 0.;

# Mean Square Error loss function
def loss() {
    y_test = x_train .* w + b;
    error = sum((y_test - y_train).^2 .* 0.5);

    return error / float_of(number_of_records);
}

# Derivative of Loss w.r.t W
def dLdW() {
    y_test = x_train .* w + b;
    dLdw = sum((y_test - y_train) .* x_train) / float_of(number_of_records);
    
    return dLdw;
}

# Derivative of Loss w.r.t b
def dLdB() {
    y_test = x_train .* w + b;
    dLdb = sum(y_test - y_train) / float_of(number_of_records);

    return dLdb;
}

# training
lr = 0.0001;
for (epoch in 0:10:1) {
    print_error(epoch, loss());
    w = w - dLdW() .* lr;
    b = b - dLdB() .* lr;
}