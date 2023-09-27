int: N = 4;

var set of 1..(2 * N): u;
var set of (N + 1)..(3 * N): v;
var set of (2 * N + 1)..(4 * N): w;

% should translate to SetIntersection
var set of int: uv = u intersect v;

% should translate to SetIntersection and Subset
var set of (2 * N + 2)..(3 * N): vw = v intersect w;

solve minimize card(uv) + card(vw);

output [
    "u = ", show(u), "\n",
    "v = ", show(v), "\n",
    "w = ", show(w), "\n",
    "uv = ", show(uv), "\n",
    "vw = ", show(vw)];
