# [vertex_name, visited, adjacency list]
uw = var["undershorts", 0, nil];
pt = var["pants", 0, nil];
se = var["shoes", 0, nil];
sk = var["socks", 0, nil];
wt = var["watch", 0, nil];
bl = var["belt", 0, nil];
ts = var["shirt", 0, nil];
ti = var["tie", 0, nil];
jc = var["jacket", 0, nil];

vertices = var[wt, sk, ts, uw, pt, se, bl, ti, jc];
sorted_v = var[nil, nil, nil, nil, nil, nil, nil, nil, nil];

uw[2] = var[pt, se];
pt[2] = var[bl, se];
sk[2] = var[se];
bl[2] = var[jc];
ts[2] = var[bl, ti];
ti[2] = var[jc];

def dfs_visit(U, time, L) {
    U[1] = 1;
    if (U[2] != nil) {
        for (v in U[2]) {
            if (v[1] == 0) {
                time = dfs_visit(v, time, L);
            }
        }
    }
    time = time - 1;
    L[time] = U;
    return time;
}

def dfs(V, L) {
    time = shape(L);
    for (u in V) {
        if (u[1] == 0) {
            time = dfs_visit(u, time, L);
        }
    }
    return time;
}

dfs(vertices, sorted_v);

for (v in sorted_v) {
    print(v[0]);
}