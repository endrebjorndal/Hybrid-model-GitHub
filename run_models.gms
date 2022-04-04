option solprint = off;
option limrow = 100;
option limcol = 100;
option nlp = minos;
option optCR = 1E-02;

*declare models and assign values to model parameters
$include model_formulations.gms

*assigning some values to avoid error messages because some models have not been run yet
net_inj_seq.m(i) = 0;

* unconstrained model
cur_case(case) = no;
cur_case('unc') = yes;
*regulation market quantities fixed to zero
qug.fx(i,k) = 0;
qdg.fx(i,k) = 0;
quc.fx(i,l) = 0;
qdc.fx(i,l) = 0;
*calculate market solution
solve unconstrained using nlp maximizing surplus;
status('unc')= unconstrained.modelstat;
$include store_results.gms

* day-ahead market with nodal pricing
cur_case(case) = no;
cur_case('nodal') = yes;
*regulation market quantities fixed to zero
qug.fx(i,k) = 0;
qdg.fx(i,k) = 0;
quc.fx(i,l) = 0;
qdc.fx(i,l) = 0;
*calculate market solution
solve redispatch using nlp maximizing surplus;
status('nodal')= redispatch.modelstat;
$include store_results.gms


*setting zonal interconnection capacities
*zcap(x,z,'fwd')$zconn(x,z) = sum((i,j)$(line(i,j) and zmem(i,x) and zmem(j,z)), lcap(i,j))
*               + sum((j,i)$(line(j,i) and zmem(i,x) and zmem(j,z)), lcap(i,j));
zcap(x,z,'fwd')$zconn(x,z) = 300;
zcap(x,z,'bwd')$zconn(x,z) = zcap(x,z,'fwd');

zcap_hybrid(i,z,'fwd') = sum(j$(line(i,j) and zmem(j,z)), lcap(i,j))
+ sum(j$(line(j,i) and zmem(j,z)), lcap(j,i));
zcap_hybrid(i,z,'bwd') = zcap_hybrid(i,z,'fwd');

zcap_hybrid_3(i,j,'fwd') = lcap(i,j);
zcap_hybrid_3(i,j,'bwd') = zcap_hybrid_3(i,j,'fwd');

* day-ahead market with zonal pricing a la NordPool
cur_case(case) = no;
cur_case('zonal') = yes;
*regulation market quantities fixed to zero
qug.fx(i,k) = 0;
qdg.fx(i,k) = 0;
quc.fx(i,l) = 0;
qdc.fx(i,l) = 0;
*calculate market solution
solve zonal using nlp maximizing surplus;
status('zonal')= zonal.modelstat;
$include store_results.gms

*redispatch market
cur_case(case) = no;
cur_case('redispatch') = yes;
*fix DA market quantities
qg.fx(i,k) = qg.l(i,k);
qc.fx(i,l) = qc.l(i,l);
*unfix regulation market quantities for
*for participating generators and consumers
qug.lo(i,k) = 0;
qdg.lo(i,k) = 0;
quc.lo(i,l) = 0;
qdc.lo(i,l) = 0;
qug.up(i,k) = inf;
qdg.up(i,k) = inf;
quc.up(i,l) = inf;
qdc.up(i,l) = inf;
*calculate market solution
solve redispatch using nlp maximizing surplus;
status('redispatch')= redispatch.modelstat;
$include store_results.gms


* First Version of Simultaneous Hybrid Model - Day Ahead Market
cur_case(case) = no;
cur_case('zonal_sim1') = yes;
*unfix DA market quantities
qg.lo(i,k) = 0;
qc.lo(i,l) = 0;
qg.up(i,k) = inf;
qc.up(i,l) = inf;
*regulation market quantities fixed to zero
qug.fx(i,k) = 0;
qdg.fx(i,k) = 0;
quc.fx(i,l) = 0;
qdc.fx(i,l) = 0;
*calculate market solution
solve zonal_sim1 using nlp maximizing surplus;
status('zonal_sim1')= zonal_sim1.modelstat;
$include store_results.gms

*redispatch market
cur_case(case) = no;
cur_case('redispatch_sim1') = yes;
*fix DA market quantities
qg.fx(i,k) = qg.l(i,k);
qc.fx(i,l) = qc.l(i,l);
*unfix regulation market quantities for
*for participating generators and consumers
qug.lo(i,k) = 0;
qdg.lo(i,k) = 0;
quc.lo(i,l) = 0;
qdc.lo(i,l) = 0;
qug.up(i,k) = inf;
qdg.up(i,k) = inf;
quc.up(i,l) = inf;
qdc.up(i,l) = inf;
*calculate market solution
solve redispatch using nlp maximizing surplus;
status('redispatch_sim1')= redispatch.modelstat;
$include store_results.gms

* Second Version of Simultaneous Hybrid Model - Day Ahead Market
cur_case(case) = no;
cur_case('zonal_sim2') = yes;
*unfix DA market quantities
qg.lo(i,k) = 0;
qc.lo(i,l) = 0;
qg.up(i,k) = inf;
qc.up(i,l) = inf;
*regulation market quantities fixed to zero
qug.fx(i,k) = 0;
qdg.fx(i,k) = 0;
quc.fx(i,l) = 0;
qdc.fx(i,l) = 0;
*calculate market solution
display zcap_hybrid;
solve zonal_sim2 using nlp maximizing surplus;
status('zonal_sim2')= zonal_sim2.modelstat;
$include store_results.gms

*redispatch market
cur_case(case) = no;
cur_case('redispatch_sim2') = yes;
*fix DA market quantities
qg.fx(i,k) = qg.l(i,k);
qc.fx(i,l) = qc.l(i,l);
*unfix regulation market quantities for
*for participating generators and consumers
qug.lo(i,k) = 0;
qdg.lo(i,k) = 0;
quc.lo(i,l) = 0;
qdc.lo(i,l) = 0;
qug.up(i,k) = inf;
qdg.up(i,k) = inf;
quc.up(i,l) = inf;
qdc.up(i,l) = inf;
*calculate market solution
solve redispatch using nlp maximizing surplus;
status('redispatch_sim2')= redispatch.modelstat;
$include store_results.gms

* Third Version of Simultaneous Hybrid Model - Day Ahead Market
cur_case(case) = no;
cur_case('zonal_sim3') = yes;
*unfix DA market quantities
qg.lo(i,k) = 0;
qc.lo(i,l) = 0;
qg.up(i,k) = inf;
qc.up(i,l) = inf;
*regulation market quantities fixed to zero
qug.fx(i,k) = 0;
qdg.fx(i,k) = 0;
quc.fx(i,l) = 0;
qdc.fx(i,l) = 0;
*calculate market solution
solve zonal_sim3 using nlp maximizing surplus;
status('zonal_sim3')= zonal_sim3.modelstat;
$include store_results.gms

*redispatch market
cur_case(case) = no;
cur_case('redispatch_sim3') = yes;
*fix DA market quantities
qg.fx(i,k) = qg.l(i,k);
qc.fx(i,l) = qc.l(i,l);
*unfix regulation market quantities for
*for participating generators and consumers
qug.lo(i,k) = 0;
qdg.lo(i,k) = 0;
quc.lo(i,l) = 0;
qdc.lo(i,l) = 0;
qug.up(i,k) = inf;
qdg.up(i,k) = inf;
quc.up(i,l) = inf;
qdc.up(i,l) = inf;
*calculate market solution
solve redispatch using nlp maximizing surplus;
status('redispatch_sim3')= redispatch.modelstat;
$include store_results.gms

*Sequential Hybrid Models

*Sequential Hybrid Model Variant 1 - Day Ahead
cur_case(case) = no;
cur_case('zonal_seq1') = yes;
*simultaneous model 1
qug.fx(i,k)$nodal_set(i) = 0;
qdg.fx(i,k)$nodal_set(i) = 0;
quc.fx(i,l)$nodal_set(i) = 0;
qdc.fx(i,l)$nodal_set(i) = 0;
*set nodal generation and consumption capacities
qg.lo(i,k)$nodal_set(i) = 0;
qc.lo(i,l)$nodal_set(i) = 0;
qg.up(i,k)$nodal_set(i) = inf;
qc.up(i,l)$nodal_set(i) = inf;
*calculate market solution
solve zonal_seq1 using nlp maximizing surplus;
status('zonal_seq1')= zonal_seq1.modelstat;
$include store_results.gms

*Sequential Hybrid Model Variant 1 - Redispatch
cur_case(case) = no;
cur_case('redispatch_seq1') = yes;
*fix DA market quantities
qg.fx(i,k) = qg.l(i,k);
qc.fx(i,l) = qc.l(i,l);
*unfix regulation market quantities for
*for participating generators and consumers
qug.lo(i,k) = 0;
qdg.lo(i,k) = 0;
quc.lo(i,l) = 0;
qdc.lo(i,l) = 0;
qug.up(i,k) = inf;
qdg.up(i,k) = inf;
quc.up(i,l) = inf;
qdc.up(i,l) = inf;
*calculate market solution
solve redispatch using nlp maximizing surplus;
status('redispatch_seq1')= redispatch.modelstat;
$include store_results.gms

*Sequential Hybrid Model Variant 2 - Day Ahead
cur_case(case) = no;
cur_case('zonal_seq2') = yes;
*sequential model 2
qug.fx(i,k)$nodal_set(i) = 0;
qdg.fx(i,k)$nodal_set(i) = 0;
quc.fx(i,l)$nodal_set(i) = 0;
qdc.fx(i,l)$nodal_set(i) = 0;
*set nodal generation and consumption capacities
qg.lo(i,k)$nodal_set(i) = 0;
qc.lo(i,l)$nodal_set(i) = 0;
qg.up(i,k)$nodal_set(i) = inf;
qc.up(i,l)$nodal_set(i) = inf;
*calculate market solution
solve zonal_seq2 using nlp maximizing surplus;
status('zonal_seq2')= zonal_seq2.modelstat;
$include store_results.gms

*Sequential Hybrid Model Variant 2 - Redispatch
cur_case(case) = no;
cur_case('redispatch_seq2') = yes;
*fix DA market quantities
qg.fx(i,k) = qg.l(i,k);
qc.fx(i,l) = qc.l(i,l);
*unfix regulation market quantities for
*for participating generators and consumers
qug.lo(i,k) = 0;
qdg.lo(i,k) = 0;
quc.lo(i,l) = 0;
qdc.lo(i,l) = 0;
qug.up(i,k) = inf;
qdg.up(i,k) = inf;
quc.up(i,l) = inf;
qdc.up(i,l) = inf;
*calculate market solution
solve redispatch using nlp maximizing surplus;
status('redispatch_seq2')= redispatch.modelstat;
$include store_results.gms

*day ahead market
cur_case(case) = no;
cur_case('zonal_seq3') = yes;
*simultaneous model 1
qug.fx(i,k)$nodal_set(i) = 0;
qdg.fx(i,k)$nodal_set(i) = 0;
quc.fx(i,l)$nodal_set(i) = 0;
qdc.fx(i,l)$nodal_set(i) = 0;
*set nodal generation and consumption capacities
qg.lo(i,k)$nodal_set(i) = 0;
qc.lo(i,l)$nodal_set(i) = 0;
qg.up(i,k)$nodal_set(i) = inf;
qc.up(i,l)$nodal_set(i) = inf;
*calculate market solution
solve zonal_seq3 using nlp maximizing surplus;
status('zonal_seq3')= zonal_seq3.modelstat;
$include store_results.gms

*Sequential Hybrid Model Variant 3 - Redispatch
cur_case(case) = no;
cur_case('redispatch_seq3') = yes;
*fix DA market quantities
qg.fx(i,k) = qg.l(i,k);
qc.fx(i,l) = qc.l(i,l);
*unfix regulation market quantities for
*for participating generators and consumers
qug.lo(i,k) = 0;
qdg.lo(i,k) = 0;
quc.lo(i,l) = 0;
qdc.lo(i,l) = 0;
qug.up(i,k) = inf;
qdg.up(i,k) = inf;
quc.up(i,l) = inf;
qdc.up(i,l) = inf;
*calculate market solution
solve redispatch using nlp maximizing surplus;
status('redispatch_seq3')= redispatch.modelstat;
$include store_results.gms

*unload all parameters and variables to a gdx file that can be read in R
execute_unload "results.gdx";
