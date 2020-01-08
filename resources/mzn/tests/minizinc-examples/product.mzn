% RUNS ON mzn_mer_lp
% RUNS ON mzn_mer_fdlp
% RUNS ON zinc_mip_hl

%% Product example from the OPL book!
%% product.mzn
%% Peter Stuckey September 30 2006

%enum Products  = {kluski, capellini, fettucine};
int: NumProducts = 3;
set of int: Products = 1..NumProducts;
int: kluski = 0;
int: capellini = 1;
int: fettucine = 2;

%enum Resources = {flour, eggs};
int: NumResources = 2;
set of int: Resources = 1..NumResources;
int: flour = 0;
int: eggs = 1;

%type ProductData =
%	record(
%		float: demand, float: insideCost, float: outsideCost,
%		array[Resources] of float: consumption
%	);

%array[Products] of ProductData:
%	prod = [kluski:    (100, 0.6, 0.8, [flour: 0.5, eggs: 0.2]),
%		capellini: (200, 0.8, 0.9, [flour: 0.4, eggs: 0.4]),
%		fettucine: (300, 0.3, 0.4, [flour: 0.3, eggs: 0.6])];
array[Products] of float: demand = [100.0,200.0,300.0];
array[Products] of float: insideCost = [0.6,0.8,0.3];
array[Products] of float: outsideCost = [0.8,0.9,0.4];
array[Products, Resources] of float: consumption =
	[| 0.5, 0.2 |
	   0.4, 0.4 |
	   0.3, 0.6 |];

array[Resources] of float: capacity = [20.0, 40.0];

array[Products] of var float: inside;
array[Products] of var float: outside;

constraint
	forall (p in Products) (
		inside[p]  >= 0.0
	/\	outside[p] >= 0.0
	);

constraint
	forall(r in Resources) (
		sum (p in Products) ( consumption[p, r] * inside[p] )
		<=
                capacity[r]
	);

constraint
	forall(p in Products) (
		inside[p] + outside[p] >= demand[p]
	);

var float: cost =
	sum (p in Products) (
		insideCost[p]*inside[p] + outsideCost[p]*outside[p]
	);

solve minimize cost;

output [
	"production planning\n",
	"             \tkluski\t\tfettucine\tcapellini\n",
	"make inside: \t",
	show(inside[1]), "\t\t",
	show(inside[2]), "\t\t",
	show(inside[3]), "\n",
	"make outside: \t",
	show(outside[1]), "\t\t",
	show(outside[2]), "\t\t",
	show(outside[3]), "\n",
	"cost: ", show(cost), "\n"
];
