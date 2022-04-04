*physical quantities
generation(i,k,cur_case) = qg.l(i,k) + qug.l(i,k) - qdg.l(i,k);
consumption(i,l,cur_case) = qc.l(i,l) - quc.l(i,l) + qdc.l(i,l);
net_inj_store(i,cur_case) = sum(k,generation(i,k,cur_case))-sum(l,consumption(i,l,cur_case));

*commercial flow
cflow(i,j,cur_case)$line(i,j) = f.l(i,j);

*physical flow
pflow(i,j,cur_case)$line(i,j) = sum(jj, ptdf(i,j,jj)*net_inj_store(jj,cur_case));

*store prices
price(i,cur_case)$(not DA_seq(cur_case)) = net_inj.m(i);
price(i,cur_case)$(DA_seq(cur_case) and zonal_set(i)) = price(i,'zonal');
price(i,cur_case)$(DA_seq(cur_case) and nodal_set(i)) = net_inj_seq.m(i);

*cost for individual generators
cost(i,k,cur_case) =
   (e(i,k)*(qg.l(i,k) + qug.l(i,k) - qdg.l(i,k))
   +0.5*c(i,k)*(qg.l(i,k) + qug.l(i,k) - qdg.l(i,k))*(qg.l(i,k) + qug.l(i,k) - qdg.l(i,k))
   +0.5*(cu(i,k) - c(i,k))*qug.l(i,k)*qug.l(i,k)
   +0.5*(cd(i,k) - c(i,k))*qdg.l(i,k)*qdg.l(i,k));

*benefit for individual consumers
benefit(i,l,cur_case) =
   (a(i,l)*(qc.l(i,l) - quc.l(i,l) + qdc.l(i,l))
   -0.5*b(i,l)*(qc.l(i,l) - quc.l(i,l) + qdc.l(i,l))*(qc.l(i,l) - quc.l(i,l) + qdc.l(i,l))
   -0.5*(bu(i,l) - b(i,l))*quc.l(i,l)*quc.l(i,l)
   -0.5*(bd(i,l) - b(i,l))*qdc.l(i,l)*qdc.l(i,l));

*payments
*a positive payment is equivalent to a payment from the system operator to generators/consumers
paym_gen(i,k,cur_case)$(not RT(cur_case)) = qg.l(i,k)*price(i,cur_case);
paym_con(i,l,cur_case)$(not RT(cur_case)) = -qc.l(i,l)*price(i,cur_case);

paym_gen(i,k,cur_case)$RT(cur_case) = sum(ccase$DA_RT(ccase,cur_case),paym_gen(i,k,ccase))
        + (qug.l(i,k) - qdg.l(i,k))*price(i,cur_case);
paym_con(i,l,cur_case)$RT(cur_case) = sum(ccase$DA_RT(ccase,cur_case),paym_con(i,l,ccase))
        + (quc.l(i,l) - qdc.l(i,l))*price(i,cur_case);

*summing payments
paym_tot('gen',cur_case) = sum((i,k), paym_gen(i,k,cur_case));
paym_tot('con',cur_case) = sum((i,l), paym_con(i,l,cur_case));
paym_tot('grid',cur_case) = paym_tot('gen',cur_case) + paym_tot('con',cur_case);

*computing surpluses
surpl_gen(i,k,cur_case) = paym_gen(i,k,cur_case) - cost(i,k,cur_case);
surpl_con(i,l,cur_case) = benefit(i,l,cur_case) + paym_con(i,l,cur_case);
surpl_tot('gen',cur_case) = sum((i,k), surpl_gen(i,k,cur_case));
surpl_tot('con',cur_case) = sum((i,l), surpl_con(i,l,cur_case));
surpl_tot('grid',cur_case) = -paym_tot('grid',cur_case);
surpl_tot('tot',cur_case) = surpl_tot('gen',cur_case)
                            + surpl_tot('con',cur_case) + surpl_tot('grid',cur_case);




