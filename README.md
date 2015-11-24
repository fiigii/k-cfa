# <i>k</i>-CFA
Context-sensitive control flow analysis (uniform k-CFA) for my fJS
language.

#Principle
<i>k</i>-CFA (ployvariant) is much more complicated than 0-CFA
(monovariant) because there is no any information about how many
varanits a function has, statically. Consequentially, the
constraint-based <i>k</i>-CFA cannot generate all of the constraints
then to solve them. I implement it as a demand-driven constraint
generator/solver, which statically generates parts of constraints and
solves them to get more constraints for closures.

# Demo
Program:
```
function main (argument) {
  var f = function (x)x;
  f(f)(function(y)y)
}
```
Result:
```
> cabal run
Preprocessing executable 'k-cfa' for k-cfa-0.1.0.0...
Running k-cfa...
Pragram:
var f = function (x) {x @<1> } @<2> ;
f @<3> (f @<4> ) @<5> (function (y) {y @<6> } @<7> ) @<8>  @<9>

Control Flow:
1 at [5] : [function (x) {x @<1> } @<2>  bind []]
1 at [8] : [function (y) {y @<6> } @<7>  bind []]
2 at [] : [function (x) {x @<1> } @<2>  bind []]
3 at [] : [function (x) {x @<1> } @<2>  bind []]
4 at [] : [function (x) {x @<1> } @<2>  bind []]
5 at [] : [function (x) {x @<1> } @<2>  bind []]
7 at [] : [function (y) {y @<6> } @<7>  bind []]
8 at [] : [function (y) {y @<6> } @<7>  bind []]
9 at [] : [function (y) {y @<6> } @<7>  bind []]
"f" at [] : [function (x) {x @<1> } @<2>  bind []]
"x" at [5] : [function (x) {x @<1> } @<2>  bind []]
"x" at [8] : [function (y) {y @<6> } @<7>  bind []]
```

