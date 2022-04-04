option solprint = on;
option limrow = 100;
option limcol = 100;
option lp = minos;

*sets and parameters
sets
   i /1*6/; 

alias(i,j);
alias(i,ii);
alias(i,iii);

sets
   line(i,j) /1.2,1.3,2.3,1.4,3.4,3.5,4.5,4.6,5.6/;
   
parameters
   lpar(i,j,*) line parameters
   ptdf(i,j,i) nptdf matrix
   q(i) net injection
   sw_node;
  
variables
   f(i,j) line flow
   theta(i) phase angle for node
   outflow;

equations
   kirch1 kirchhoffs current law for every node
   kirch2 kirchhoffs voltage law for every line
   swingnode fix phase angle for first node
   obj;

kirch1(i).. q(i) =E=
   sum(j$line(i,j), f(i,j)) - sum(j$line(j,i), f(j,i));

kirch2(i,j)$line(i,j)..
   f(i,j) =E= theta(i) - theta(j);

swingnode(i)$(i.val eq sw_node).. theta(i) =E= 0;

obj.. outflow =e= sum(i$(i.val eq sw_node),q(i));

model comp_ptdf
    /kirch1, kirch2, swingnode, obj/;

sw_node = 1;
loop(ii$(ii.val ne sw_node),
    q(ii) = 1;
    q(iii)$(iii.val eq sw_node) = -1;
    q(iii)$(iii.val ne sw_node and iii.val ne ii.val) = 0;
    solve comp_ptdf using lp minimizing outflow;
    ptdf(i,j,ii) = f.l(i,j);
    );

option ptdf:6:0:1;
display i, line, ptdf;