#activate gdxrrw package for importing data in gdx files
library(gdxrrw)
#point to gdx library functions in the gams system directory
#must be a fairly recent gams version, does not need to be the one you are using for the calculations
igdx("C:\\GAMS\\win64\\38")

#define function for plotting network
plot_network = function(case="nodal",scale=1,pflow=T,file="results.gdx")
{
  library(plotrix)
  library(network)
  par(family="serif",mar=c(0,0,0,0))
  
  #get results from gdx results file
  line_data=as.numeric(unlist(rgdx.set(file,"line")))
  line_data=matrix(c(line_data,rep(NA,length(line_data))),ncol=4)
  node_data=as.numeric(unlist(unique(rgdx.set(file,"i"))))
  node_data=matrix(c(node_data,rep(NA,length(node_data)*2)),ncol=3)
  price=rgdx.param(file,"price",squeeze = F)
  net_inj=rgdx.param(file,"net_inj_store",squeeze = F)
  for(i in 1:nrow(node_data))
  {
    r = which(price[[2]]==case & price[[1]]==node_data[i,1])
    if(length(r) == 1)
      node_data[i,2] = price[r,3]
    r = which(net_inj[[2]]==case & net_inj[[1]]==node_data[i,1])
    if(length(r) == 1)
      node_data[i,3] = net_inj[r,3]
  }
  
  if(pflow)
    flow=rgdx.param(file,"pflow",squeeze = F)
  else
    flow=rgdx.param(file,"cflow",squeeze = F)
  lcap=rgdx.param(file,"lcap",squeeze = F)
  for(i in 1:nrow(line_data))
  {
    r = which(flow[[3]] == case & flow[[1]] == line_data[i,1] & flow[[2]] == line_data[i,2])
    if(length(r) == 1)
      line_data[i,3] = flow[r,4]
    r = which(lcap[[1]] == line_data[i,1] & lcap[[2]] == line_data[i,2])
    if(length(r) == 1)
      line_data[i,4] = lcap[r,3]
  }
  
  #coordinates for nodes and text
  x=c(3  ,1.5,3  ,6  ,6  ,7.5)
  y=c(4.5,3  ,1.5,4.5,1.5,3  )
  #function for adjusting coordinates for line text  
  l.cor.adj = function(x,y,x.mid=4.5,y.mid=3)
    {
    epsilon=0.1
    if(x < x.mid-epsilon)
      x.adj=x-0.35
    else
      if(x > x.mid+epsilon)
         x.adj=x+0.35
    else
      x.adj=x
    if(y < y.mid-epsilon)
      y.adj=y-0.35
    else
      if(y > y.mid+epsilon)
        y.adj=y+0.35
    else
      y.adj=y
    if(x < x.mid+epsilon & y < y.mid+epsilon & x > x.mid-epsilon & y > y.mid-epsilon)
      {
      x.adj=x-0.35
      y.adj=y+0.35
      }
    return(c(x.adj,y.adj))
    }
  #function for adjusting coordinates for node text  
  n.cor.adj = function(x,y,x.mid=4.5,y.mid=3)
    {
    x.epsilon=2
    y.epsilon=1
    if(x < x.mid-x.epsilon)
      x.adj=x-0.85
    else
      if(x > x.mid+x.epsilon)
        x.adj=x+0.85
      else
        x.adj=x
    if(y < y.mid-y.epsilon)
        y.adj=y-0.6
    else
      if(y > y.mid+y.epsilon)
        y.adj=y+0.6
      else
        y.adj=y
    return(c(x.adj,y.adj))
    }

  #correspondence between GAMS case names and text
  cnames = matrix(c("unc","nodal","zonal","redispatch",
            "zonal_sim1","redispatch_sim1","zonal_sim2","redispatch_sim2","zonal_sim3","redispatch_sim3",
            "zonal_seq1","redispatch_seq1","zonal_seq2","redispatch_seq2","zonal_seq3","redispatch_seq3",
            "Unconstrained","Nodal","Zonal DA","Zonal RT",
            "Sim1 DA","Sim1 RT","Sim2 DA","Sim2 RT","Sim3 DA","Sim3 RT",
            "Seq1 DA","Seq1 RT","Seq2 DA","Seq2 RT","Seq3 DA","Seq3 RT"), nrow=2, byrow=T)
  
  #plot nodes with text
  plot(NA,NA,xlim=c(0,9),ylim=c(0,6),axes=F,xlab=NA,ylab=NA)
  text(x=4.5,y=6,labels=cnames[2,cnames[1,]==case],font=2,cex=1.5)
  for(i in 1:6)
  {
    draw.circle(x[i],y[i],radius=0.15)
    text(x[i],y[i],labels=i,cex=scale)
    xy.cor = n.cor.adj(x[i],y[i])
    text(xy.cor[1],xy.cor[2],labels=paste(round(node_data[i,3],1)," @ ",round(node_data[i,2],2),sep=""),col="blue",cex=scale) 
  }
  
  #plot lines with text
  for(l in 1:nrow(line_data))
  {
    if(abs(line_data[l,3]) < line_data[l,4]-0.00001)
      lcolor="pink"
    else
      lcolor="red"
    if(line_data[l,3] > 0)
      network.arrow(x[line_data[l,1]],y[line_data[l,1]],x[line_data[l,2]],y[line_data[l,2]],
                    offset.head = 0.3,offset.tail = 0.3,col=lcolor,border=lcolor,length=0.2,width=0.02)
    if(line_data[l,3] < 0)
      network.arrow(x[line_data[l,2]],y[line_data[l,2]],x[line_data[l,1]],y[line_data[l,1]],
                    offset.head = 0.3,offset.tail = 0.3,col=lcolor,border=lcolor,length=0.2,width=0.02)
    if(line_data[l,3] == 0)
      network.arrow(x[line_data[l,2]],y[line_data[l,2]],x[line_data[l,1]],y[line_data[l,1]],
                    offset.head = 0.3,offset.tail = 0.3,col=lcolor,border=lcolor,length=0.2,width=0.02,arrowhead=F)
    xy.cor = l.cor.adj(sum(x[line_data[l,1:2]])/2,sum(y[line_data[l,1:2]])/2)
    text(xy.cor[1],xy.cor[2]+0.15,labels=line_data[l,4],cex=scale)
    text(xy.cor[1],xy.cor[2]-0.15,labels=abs(round(line_data[l,3],1)),col="red",cex=scale)
  }
}

#function for creating tables
show_tables = function(file="results.gdx")
{
  #table with surpluses
  status=rgdx.param(file,"status")
  surpl_tot=rgdx.param(file,"surpl_tot",squeeze = F)
  surpl_tbl=array(NA,dim=c(4,4),dimnames=list(c("Gen.","Load","Grid","Total"),c("Unc.","Nodal","Zonal DA","Zonal RT")))
  surpl_tbl_sim=array(NA,dim=c(4,6),dimnames=list(c("Gen.","Load","Grid","Total"),c("Sim1 DA","Sim1 RT","Sim2 DA","Sim2 RT","Sim3 DA","Sim3 RT")))
  surpl_tbl_seq=array(NA,dim=c(4,6),dimnames=list(c("Gen.","Load","Grid","Total"),c("Seq1 DA","Seq1 RT","Seq2 DA","Seq2 RT","Seq3 DA","Seq3 RT")))
  rnames = c("gen","con","grid","tot")
  cnames = c("unc","nodal","zonal","redispatch")
  cnames_sim = c("zonal_sim1","redispatch_sim1","zonal_sim2","redispatch_sim2","zonal_sim3","redispatch_sim3")
  cnames_seq = c("zonal_seq1","redispatch_seq1","zonal_seq2","redispatch_seq2","zonal_seq3","redispatch_seq3")
  
  for(i in 1:nrow(surpl_tbl))
    for(j in 1:ncol(surpl_tbl))
    {
      r = which(surpl_tot[[1]] == rnames[i] & surpl_tot[[2]] == cnames[j])
      if(length(r) == 1)
        surpl_tbl[i,j] = round(surpl_tot[r,3],0)
    }
  
  for(i in 1:nrow(surpl_tbl_sim))
    for(j in 1:ncol(surpl_tbl_sim))
    {
      r = which(surpl_tot[[1]] == rnames[i] & surpl_tot[[2]] == cnames_sim[j])
      if(length(r) == 1)
        surpl_tbl_sim[i,j] = round(surpl_tot[r,3],0)
    }
  
  for(i in 1:nrow(surpl_tbl_seq))
    for(j in 1:ncol(surpl_tbl_seq))
    {
      r = which(surpl_tot[[1]] == rnames[i] & surpl_tot[[2]] == cnames_seq[j])
      if(length(r) == 1)
        surpl_tbl_seq[i,j] = round(surpl_tot[r,3],0)
    }
  
  #show tables
  status
  surpl_tbl
  surpl_tbl_sim
  surpl_tbl_seq
  
  return(list(status=status,surpl_tbl=surpl_tbl,surpl_tbl_sim=surpl_tbl_sim,surpl_tbl_seq=surpl_tbl_seq))
}