sets
    i /1*6/
    k /1/
    l /1/
    z /1*2/
    nodal_set /4*6/
    zmem /1.1, 2.1, 3.1, 4.2, 5.2, 6.2/
    line /1.2, 1.3, 1.4, 2.3, 3.5, 4.5, 4.6, 5.6/
    zconn /1.2/
    con_reg /3.1, 4.1, 5.1/
    gen_reg /1.1, 2.1, 6.1/;

parameters
a(i,l) /
3.1 100, 
4.1 100, 
5.1 100 /

b(i,l) /
3.1 0.05, 
4.1 0.1, 
5.1 0.05 /

con_ub(i,l) /
3.1 10000, 
4.1 10000, 
5.1 10000 /

e(i,k) /
1.1 0
2.1 0
6.1 0 /

c(i,k) /
1.1 0.05, 
2.1 0.05, 
6.1 0.025 /

gen_ub(i,k) /
1.1 10000, 
2.1 10000, 
6.1 10000 /

con_gamma(i,l) /
1.1 8, 
2.1 8, 
3.1 8, 
4.1 8, 
5.1 8, 
6.1 8 /

gen_gamma(i,k) /
1.1 2, 
2.1 2, 
3.1 2, 
4.1 2, 
5.1 2, 
6.1 2 /

lcap(i,i) /
1.2 60, 
1.3 500, 
1.4 300, 
2.3 600, 
3.5 50, 
4.5 100, 
4.6 600, 
5.6 300 /

zcap(z,z,*) /
1.2.'fwd' 0
1.2.'bwd' 0/

ptdf(i,i,i) /
1.2.2 -0.633333333333333, 
1.2.3 -0.266666666666667, 
1.2.4 -0.1, 
1.2.5 -0.166666666666667, 
1.2.6 -0.133333333333333, 
1.3.2 -0.266666666666667, 
1.3.3 -0.533333333333333, 
1.3.4 -0.2, 
1.3.5 -0.333333333333333, 
1.3.6 -0.266666666666667, 
1.4.2 -0.1, 
1.4.3 -0.2, 
1.4.4 -0.7, 
1.4.5 -0.5, 
1.4.6 -0.6, 
2.3.2 0.366666666666667, 
2.3.3 -0.266666666666667, 
2.3.4 -0.0999999999999999, 
2.3.5 -0.166666666666667, 
2.3.6 -0.133333333333333, 
3.5.2 0.1, 
3.5.3 0.2, 
3.5.4 -0.3, 
3.5.5 -0.5, 
3.5.6 -0.4, 
4.5.2 -0.0666666666666667, 
4.5.3 -0.133333333333333, 
4.5.4 0.2, 
4.5.5 -0.333333333333333, 
4.5.6 -0.0666666666666667, 
4.6.2 -0.0333333333333333, 
4.6.3 -0.0666666666666667, 
4.6.4 0.1, 
4.6.5 -0.166666666666667, 
4.6.6 -0.533333333333333, 
5.6.2 0.0333333333333333, 
5.6.3 0.0666666666666667, 
5.6.4 -0.1, 
5.6.5 0.166666666666667, 
5.6.6 -0.466666666666667 /;