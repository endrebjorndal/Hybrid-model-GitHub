sets
   i nodes
   z zones
   nodal_set(i) nodes that are in "nodal" pricing areas
   zonal_set(i) nodes that are in zones
   hybrid_set(i) nodes that are in nodal zones and directly connected to zones
   nodal_zone(z) zones that are comprised of nodal_set nodes
   l consumers or demand bid curve segments
   k producers or supply bid curve segments
   con_reg(i,l) consumers that participate in regulation market
   gen_reg(i,k) generators that participate in regulation market
   line(i,i) whether a line exists between two lines
   zconn(z,z) whether there is a connection from zone z to zone x
   zconn_hybrid2(i,z) node i is in a nodal area and z is a non-nodal zone
   zconn_hybrid3(i,i) lines connecting a nodal area to a non-nodal area
   zmem(i,z) whether node i is a member of zone z
   agent /gen, con, grid, tot/
   case /unc, nodal, zonal,
        zonal_sim1, zonal_sim2, zonal_sim3,
        zonal_seq1, zonal_seq2, zonal_seq3,
        redispatch,
        redispatch_sim1, redispatch_sim2, redispatch_sim3,
        redispatch_seq1, redispatch_seq2, redispatch_seq3/
   cur_case(case) current case
   DA_RT(case,case) /zonal.redispatch, zonal_sim1.redispatch_sim1,
                    zonal_sim2.redispatch_sim2, zonal_sim3.redispatch_sim3,
                    zonal_seq1.redispatch_seq1, zonal_seq2.redispatch_seq2,
                    zonal_seq3.redispatch_seq3/
   DA(case) /zonal, zonal_sim1, zonal_sim2, zonal_sim3,
                    zonal_seq1, zonal_seq2, zonal_seq3/
   DA_seq(case) /zonal_seq1, zonal_seq2, zonal_seq3/
   RT(case) /redispatch,
            redispatch_sim1, redispatch_sim2, redispatch_sim3,
            redispatch_seq1, redispatch_seq2, redispatch_seq3/;

parameters
   e(i,k) constant term for supply bid curve
   c(i,k) slope for spot market marginal cost curve
   cu(i,k) slope for upregulation by generation
   cd(i,k) slope for downregulation by generation
   gen_ub(i,k) generation upper bound
   gen_gamma(i,k) flexibility cost factor for generators
   a(i,l) constant term for demand bid curve
   b(i,l) slope for demand bid curve in spot market
   bu(i,l) slope for upregulation by consumption
   bd(i,l) slope for downregulation by consumption
   con_ub(i,l) consumption upper bound
   con_gamma(i,l) flexibility cost factor for load
   lcap(i,i) line capacity
   ptdf(i,i,i) ptdf matrix
   zcap(z,z,*) capacity for zonal interconnection
   zcap_hybrid(i,z,*) capacity between hybrid nodes and zones
   zcap_hybrid_3(i,i,*) capacity between hybrid nodes and zonal nodes
   status(case)
   price(i,case) dual prices for nodes
   cost(i,k,case) cost for individual generators
   benefit(i,l,case) benefit for individual consumers
   paym_tot(agent,case)
   paym_gen(i,k,case) payment for individual generators
   paym_con(i,l,case) payment for individual consumers
   surpl_tot(agent,case) total surplus
   surpl_gen(i,k,case) surplus for individual generators
   surpl_con(i,l,case) surplus for individual consumers
   pflow(i,i,*) physical line flows
   cflow(i,i,case) commercial flows
   generation(i,k,*)
   consumption(i,l,*)
   net_inj_store(i,*);

alias(i,j);
alias(i,jj);
alias(i,jjj);
alias(i,jjjj);
alias(z,x);
alias (case,ccase);

*load data set
$include 6_node_extra_line.gms

*assign values to regulation market bid curve parameters
cu(i,k) = gen_gamma(i,k)*c(i,k);
cd(i,k) = gen_gamma(i,k)*c(i,k);
bu(i,l) = con_gamma(i,l)*b(i,l);
bd(i,l) = con_gamma(i,l)*b(i,l);

*define hybrid set
hybrid_set(i) = no;
loop ((i,j)$line(i,j),
hybrid_set(i)$(nodal_set(i) and not nodal_set(j)) = yes;
hybrid_set(j)$(nodal_set(j) and not nodal_set(i)) = yes;
    );

*define nodal zone
nodal_zone(z) = yes;
loop(z,
         loop(i$zmem(i,z),
         if(not nodal_set(i),nodal_zone(z) = no);
         ););

*zonal set is complement of nodal and hybrid set
zonal_set(i) = not nodal_set(i) and not hybrid_set(i);


*define hybrid interconnections

*hybrid 2
zconn_hybrid2(i,z)$(hybrid_set(i) and not nodal_zone(z)) = yes;
*hybrid 3
zconn_hybrid3(i,j)$(line(i,j) and ((nodal_set(i) and zonal_set(j)) or (nodal_set(j) and zonal_set(i)))) = yes;

display zconn, zconn_hybrid2, zconn_hybrid3;

positive variables
   qg(i,k) spot market generation quantity
   qug(i,k) upregulation generation quantity
   qdg(i,k) downregulation generation quantity
   qc(i,l) spot market consumption quantity
   quc(i,l) upregulation consumption quantity
   qdc(i,l) downregulation consumption quantity;

variables
   q(i) net injection quantity
   q_imp(i,z) quantity imported into nodal pricing area from zonal area
   r(i) net injection quantity at a zonal node
   f(i,j) line flow
   theta(i) phase angle for node
   surplus total benefit minus total cost;

equations
   tot_surplus definition of total surplus
   tot_surplus_seq definition of total surplus
   net_inj(i) net injection in every node
   net_inj_seq(i) net injection at nodal nodes
   net_inj_zonal(i) net injection at nodes in zones
   net_inj_hybrid(i) net inject at hybrid nodes
   gcon_up upper bound on final generation
   gcon_lo lower bound on final generation
   ccon_up upper bound on final consumption
   ccon_lo lower bound on final consumption
   gcon_up_seq upper bound on final generation for the sequential model
   gcon_lo_seq lower bound on final generation for the sequential model
   ccon_up_seq upper bound on final consumption for the sequential model
   ccon_lo_seq lower bound on final consumption for the sequential model
   gen_redisp_down includes generation participating in downward regulation market
   gen_redisp_up   includes generation participating in upward regulation market
   con_redisp_down includes consumption participating in downward regulation market
   con_redisp_up   includes consumption participating in upward regulation market
   sum_NI sum of net injections
   sum_NI_seq sum of day ahead zonal line flows at a hybrid node
   aggr_net_export sets aggregate constraints between nodal and zonal areas 
   ptdf_flow calculates physical flows over given line
   ptdf_flow_hybrid calculates physical flows over given line connected to hybrid node
   zflow(x) links zonal injections to zonal flows
   zflow_hybrid calculates zonal ptdf flows for zonal nodes
   kirch1 kirchhoffs current law for every node
   kirch2 kirchhoffs voltage law for every line
   fwd_lconstr forward capacity constraint for line
   bwd_lconstr backward capacity constraint for line
   fwd_lconstr_zonalsim forward capacity constraint for line
   bwd_lconstr_zonalsim backward capacity constraint for line
   fwd_zconstr forward capacity constraint for zonal interconnection
   bwd_zconstr backward capacity constraint for zonal interconnection
   fwd_zconstr_hybrid forward capacity constraint for zonal interconnection
   bwd_zconstr_hybrid backward capacity constraint for zonal interconnection
   fwd_zconstr_hybrid_2 forward capacity constraint between hybrid nodes and zone
   bwd_zconstr_hybrid_2 backward capacity constraint between hybrid nodes and zone
   fwd_zconstr_hybrid_3 forward line capacity constraint specific to hybrid nodes and zonal nodes
   bwd_zconstr_hybrid_3 backward line capacity constraint specific to hybrid nodes and zonal nodes
   fixed_flow_redisp(x,z) fixed flows between zones from zonal day ahead model;

tot_surplus.. surplus =E=
   sum((i,l),
      a(i,l)*(qc(i,l) - quc(i,l) + qdc(i,l))
      -0.5*b(i,l)*(qc(i,l) - quc(i,l) + qdc(i,l))*(qc(i,l) - quc(i,l) + qdc(i,l))
      -0.5*(bu(i,l) - b(i,l))*quc(i,l)*quc(i,l)
      -0.5*(bd(i,l) - b(i,l))*qdc(i,l)*qdc(i,l))
   -sum((i,k),
      e(i,k)*(qg(i,k) + qug(i,k) - qdg(i,k))
      +0.5*c(i,k)*(qg(i,k) + qug(i,k) - qdg(i,k))*(qg(i,k) + qug(i,k) - qdg(i,k))
      +0.5*(cu(i,k) - c(i,k))*qug(i,k)*qug(i,k)
      +0.5*(cd(i,k) - c(i,k))*qdg(i,k)*qdg(i,k));
      
tot_surplus_seq.. surplus =E=
   sum((i,l)$nodal_set(i),
      a(i,l)*(qc(i,l) - quc(i,l) + qdc(i,l))
      -0.5*b(i,l)*(qc(i,l) - quc(i,l) + qdc(i,l))*(qc(i,l) - quc(i,l) + qdc(i,l))
      -0.5*(bu(i,l) - b(i,l))*quc(i,l)*quc(i,l)
      -0.5*(bd(i,l) - b(i,l))*qdc(i,l)*qdc(i,l))
   -sum((i,k)$nodal_set(i),
      e(i,k)*(qg(i,k) + qug(i,k) - qdg(i,k))
      +0.5*c(i,k)*(qg(i,k) + qug(i,k) - qdg(i,k))*(qg(i,k) + qug(i,k) - qdg(i,k))
      +0.5*(cu(i,k) - c(i,k))*qug(i,k)*qug(i,k)
      +0.5*(cd(i,k) - c(i,k))*qdg(i,k)*qdg(i,k));


gcon_up(i,k).. qg(i,k) + qug(i,k) - qdg(i,k) =L= gen_ub(i,k);

gcon_lo(i,k).. qg(i,k) + qug(i,k) - qdg(i,k) =G= 0;

ccon_up(i,l).. qc(i,l) - quc(i,l) + qdc(i,l) =L= con_ub(i,l);

ccon_lo(i,l).. qc(i,l) - quc(i,l) + qdc(i,l) =G= 0;

gcon_up_seq(i,k)$nodal_set(i).. qg(i,k) + qug(i,k) - qdg(i,k) =L= gen_ub(i,k);

gcon_lo_seq(i,k)$nodal_set(i).. qg(i,k) + qug(i,k) - qdg(i,k) =G= 0;

ccon_up_seq(i,l)$nodal_set(i).. qc(i,l) - quc(i,l) + qdc(i,l) =L= con_ub(i,l);

ccon_lo_seq(i,l)$nodal_set(i).. qc(i,l) - quc(i,l) + qdc(i,l) =G= 0;

gen_redisp_down(i,k)$(not gen_reg(i,k))..  qdg(i,k) =E= 0;

gen_redisp_up(i,k)$(not gen_reg(i,k))..  qug(i,k) =E= 0;

con_redisp_down(i,l)$(not con_reg(i,l))..  qdc(i,l) =E= 0;

con_redisp_up(i,l)$(not con_reg(i,l))..  quc(i,l) =E= 0;

*2d
net_inj(i).. q(i) =E=
   sum(k, qg(i,k) + qug(i,k) - qdg(i,k))
   - sum(l, qc(i,l) - quc(i,l) + qdc(i,l));
   
net_inj_seq(i)$(nodal_set(i)).. q(i) =E=
    sum(k, qg(i,k) + qug(i,k) - qdg(i,k))
    - sum(l, qc(i,l) - quc(i,l) + qdc(i,l));
*2e   
sum_NI.. sum(i, q(i)) =E= 0;

sum_NI_seq.. sum(i$nodal_set(i), q(i)) =E= sum(i$nodal_set(i),
    sum(j$(zonal_set(j) and line(i,j)), cflow(i,j,'zonal'))
   - sum(j$(zonal_set(j) and line(j,i)), cflow(j,i,'zonal')));
   
aggr_net_export.. sum(i$nodal_set(i),
    sum(j$(zonal_set(j) and line(i,j)), f(i,j))
   - sum(j$(zonal_set(j) and line(j,i)), f(j,i)))
   =E=
   sum(i$nodal_set(i),
   sum(j$(zonal_set(j) and line(i,j)), cflow(i,j,'zonal'))
   - sum(j$(zonal_set(j) and line(j,i)), cflow(j,i,'zonal')));

ptdf_flow(i,j)$line(i,j).. f(i,j) =E= sum(jj, ptdf(i,j,jj)*q(jj));

ptdf_flow_hybrid(i,j)$(line(i,j) and nodal_set(i) and nodal_set(j))..
*ptdf_flow_hybrid(i,j)$(line(i,j) and (nodal_set(i) or nodal_set(j)))..
   f(i,j) =E= sum(jj$nodal_set(jj), ptdf(i,j,jj)*(q(jj)
   + sum((jjj)$(zonal_set(jjj) and line(jjj,jj)), f(jjj,jj))
   - sum((jjj)$(zonal_set(jjj) and line(jj,jjj)), f(jj,jjj))));
   
fwd_lconstr(i,j)$line(i,j).. f(i,j) =L= lcap(i,j);

bwd_lconstr(i,j)$line(i,j).. f(i,j) =G= - lcap(i,j);

fwd_lconstr_zonalsim(i,j)$(line(i,j)and nodal_set(i) and nodal_set(j)).. f(i,j) =L= lcap(i,j);

bwd_lconstr_zonalsim(i,j)$(line(i,j)and nodal_set(i) and nodal_set(j)).. f(i,j) =G= - lcap(i,j);

*2f
zflow(x).. sum(i$zmem(i,x), q(i)) =E=
 sum((i,j,z)$(line(i,j) and zmem(i,x) and zmem(j,z) and (x.val ne z.val)), f(i,j))
 - sum((j,i,z)$(line(j,i) and zmem(i,x) and zmem(j,z) and (x.val ne z.val)), f(j,i));
 
fwd_zconstr(x,z)$zconn(x,z)..
   sum((i,j)$(line(i,j) and zmem(i,x) and zmem(j,z)), f(i,j))
   - sum((j,i)$(line(j,i) and zmem(i,x) and zmem(j,z)), f(j,i))
   =L= zcap(x,z,'fwd');

bwd_zconstr(x,z)$zconn(x,z)..
   sum((i,j)$(line(i,j) and zmem(i,x) and zmem(j,z)), f(i,j))
   - sum((j,i)$(line(j,i) and zmem(i,x) and zmem(j,z)), f(j,i))
   =G= - zcap(x,z,'bwd');
   
fwd_zconstr_hybrid_2(i,z)$zconn_hybrid2(i,z)..
   sum(j$(line(i,j) and zmem(j,z)), f(i,j))
   - sum(j$(line(j,i) and zmem(j,z)), f(j,i))
   =L= zcap_hybrid(i,z,'fwd');

bwd_zconstr_hybrid_2(i,z)$zconn_hybrid2(i,z)..
   sum(j$(line(i,j) and zmem(j,z)), f(i,j))
   - sum(j$(line(j,i) and zmem(j,z)), f(j,i))
   =G= - zcap_hybrid(i,z,'bwd');

fwd_zconstr_hybrid_3(i,j)$zconn_hybrid3(i,j)..
   f(i,j) =L= zcap_hybrid_3(i,j,'fwd');
   
bwd_zconstr_hybrid_3(i,j)$zconn_hybrid3(i,j)..
   f(i,j) =G= - zcap_hybrid_3(i,j,'bwd');
   
model zonal
    /tot_surplus, gcon_up, gcon_lo, ccon_up, ccon_lo,
    net_inj, sum_NI, zflow, fwd_zconstr, bwd_zconstr/;
    
model zonal_sim1
    /tot_surplus, gcon_up, gcon_lo, ccon_up, ccon_lo,
    net_inj, sum_NI, zflow,
    ptdf_flow_hybrid,
    fwd_zconstr, bwd_zconstr,
    fwd_lconstr_zonalsim, bwd_lconstr_zonalsim/;
    
model zonal_sim2
    /tot_surplus, gcon_up, gcon_lo, ccon_up, ccon_lo,
    net_inj, sum_NI, zflow,
    ptdf_flow_hybrid,
    fwd_zconstr_hybrid_2, bwd_zconstr_hybrid_2,
    fwd_lconstr_zonalsim, bwd_lconstr_zonalsim/;
    
model zonal_sim3
    /tot_surplus, gcon_up, gcon_lo, ccon_up, ccon_lo,
    net_inj,sum_NI,zflow,
    ptdf_flow_hybrid,
    fwd_zconstr_hybrid_3, bwd_zconstr_hybrid_3,
    fwd_lconstr_zonalsim, bwd_lconstr_zonalsim/;
    
model zonal_seq1
   /tot_surplus_seq, gcon_up_seq, gcon_lo_seq, ccon_up_seq, ccon_lo_seq,
   net_inj_seq,sum_NI_seq,
   ptdf_flow_hybrid,
   aggr_net_export,
   fwd_lconstr_zonalsim, bwd_lconstr_zonalsim/;
   
model zonal_seq2
   /tot_surplus_seq, gcon_up_seq, gcon_lo_seq, ccon_up_seq, ccon_lo_seq,
   net_inj_seq,sum_NI_seq,
   ptdf_flow_hybrid,
   aggr_net_export,
   fwd_zconstr_hybrid_2, bwd_zconstr_hybrid_2,
   fwd_lconstr_zonalsim, bwd_lconstr_zonalsim/;   

model zonal_seq3
   /tot_surplus_seq, gcon_up_seq, gcon_lo_seq, ccon_up_seq, ccon_lo_seq,
   net_inj_seq,sum_NI_seq,
   ptdf_flow_hybrid,
   aggr_net_export,
   fwd_zconstr_hybrid_3, bwd_zconstr_hybrid_3,
   fwd_lconstr_zonalsim, bwd_lconstr_zonalsim/;

model redispatch
   /tot_surplus, gcon_up, gcon_lo, ccon_up, ccon_lo,
   gen_redisp_down, gen_redisp_up, con_redisp_down, con_redisp_up,
   net_inj, sum_NI, ptdf_flow, fwd_lconstr, bwd_lconstr/;
   
model unconstrained
   /tot_surplus, gcon_up, gcon_lo, ccon_up, ccon_lo,
   gen_redisp_down, gen_redisp_up, con_redisp_down, con_redisp_up,
   net_inj, sum_NI, ptdf_flow/;
