A = [5,3,9,6,3,2,1,7,8,4];
    
def partition(A, p, r) {
    n = rand([1]);
    n = int_of((n[0]+1.) .* float_of(r) .* 10.);
    n = (n % (r - p + 1)) + p;
    t = A[n];
    A[r] = A[n];
    A[n] = t;
    x = A[r];
    i = p - 1;
    for (j in p:r:1) {
        if (A[j] <= x) {
            i = i + 1;
            t = A[i];
            A[i] = A[j];
            A[j] = t;
        }
    }
    t = A[i+1];
    A[i+1] = A[r];
    A[r] = t;
    return i+1;
}

def quicksort(A, p, r) {
    if (p < r) {
        q = partition(A, p, r);
        quicksort(A, p, q-1);
        quicksort(A, q+1, r);
    }
    return 1;
}

quicksort(A, 0, 9);
print(A);