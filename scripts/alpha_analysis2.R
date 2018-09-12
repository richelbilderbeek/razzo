sim_pars2=c(0.2,0.1,0.08,0.2);sim_tree=mbd:::mbd_sim(pars = sim_pars2,soc = 2,age = 10,cond = 1);sim_tree$brts;#plot(sim_tree$tes)
q_grid=seq(from=0.02,to=0.5,by = 0.02)
maxalpha=40; utils::flush.console();
res2 <- matrix(NA, nrow = length(q_grid), ncol = maxalpha);
for (alpha in 1:maxalpha){
  print(alpha);
  q_index <- 1;
  for (q in q_grid)
    {
    if(is.na(res2[q_index,alpha]))
    {
      res2[q_index,alpha] <- try(mbd:::mbd_loglik(pars = c(sim_pars2[1:3],q),brts = sim_tree$brts,soc = 2,cond = 1,alpha = alpha),silent = T)
    }
    q_index <- q_index + 1;
  }
}
res <- res2[ , !is.na( res2[dim(res2)[1],] ) ]; dim(res)

Ldiff=matrix(NA,nrow = max(row(res)), ncol = max(col(res))-1)
for (q_index in 1:length(q_grid)){
  Ldiff[q_index,]=diff(log(-res[q_index,]))
}
# plot( (Ldiff)[24,] )
# (Ldiff)[24,]
# exp(Ldiff)[24,]

first.to.reach.threshold = rep(NA,dim(Ldiff)[1])
for ( ii in 1:dim(Ldiff)[1] ){
  first.to.reach.threshold[ii] = min( which ( Ldiff[ii,] < 10^-2 ) )
}
df = data.frame(x = q_grid, y = first.to.reach.threshold)
model2 <- stats::nls(data = df, formula = y~(a/(1+exp(b*(x-c)))+d), start = c(a=-20,b=25,c=0.2,d=max(first.to.reach.threshold)) )
coeff = stats::coef(model2)

plot.new()
plot(first.to.reach.threshold~q_grid,ylab = "alpha", xlab = "q",main = "alpha to use to keep the error below 1%\n",xlim = range(q_grid),ylim = range(first.to.reach.threshold))
graphics::par(new=T)
curve(coeff[1]/(1+exp(coeff[2]*(x-coeff[3])))+coeff[4],xlim = range(q_grid),ylim = range(first.to.reach.threshold),main=paste("\ncoefficients: a=",signif(coeff[1],2),", b=",signif(coeff[2],2),", c=",signif(coeff[3],2),",d=",signif(coeff[4],2),sep = ''),ylab = "",xlab="")
############################################################################



# myheatmap2(x=1:maxalpha,y=q_grid,z=(exp(Ldiff)),x.name="alpha",y.name="q",z.name="LL\nn difference")

myheatmap2 = function(x,y,z,x.name,y.name,z.name,x.splits,y.splits,plot.title){

  if (missing(x.splits)){x.splits=round( (length(x))/10 )}
  if (missing(y.splits)){y.splits=round( (length(y))/10 )}
  if (missing(x.name)){x.name=NULL}
  if (missing(y.name)){y.name=NULL}
  if (missing(z.name)){z.name=NULL}
  if (missing(plot.title)){plot.title=NULL}

  lX=x; lY=y;
  pretty.X.at	<-	pretty(range(lX),n=x.splits)
  pretty.X.lab	<-	round(pretty.X.at,2)
  pretty.Y.at	<-	pretty(range(lY),n=y.splits)
  pretty.Y.lab	<-	round(pretty.Y.at,2)

  jet.colors <- grDevices::colorRampPalette(c("#00007F", "blue", "#007FFF", "cyan","#7FFF7F", "yellow", "#FF7F00", "red", "#7F0000"))
  graphics::filled.contour(t(z), color = jet.colors, nlevels=100,#asp = 1, #frame.plot = T, axes=F,
                 ylab = y.name, xlab = x.name, main = plot.title,
                 plot.axes={
                   # axis(1,at=seq(0,1,length.out = x.splits+1),labels=pretty.X.lab)
                   # axis(2,at=seq(0,1,length.out = y.splits+1),labels=pretty.Y.lab)
                   graphics::axis(1,at=seq(0,1,length.out = length(pretty.X.lab) ),labels=pretty.X.lab)
                   graphics::axis(2,at=seq(0,1,length.out = length(pretty.Y.lab) ),labels=pretty.Y.lab)
                 },
                 key.title = graphics::title(main=z.name)
  )
}

# myheatmap2(x=1:maxalpha,y=q_grid,z=res,x.name="alpha",y.name="q",z.name="LL",plot.title = paste("LL evaluated for a tree simulated under pars:\n","lambda = ",sim_pars2[1],",","mu = ",sim_pars2[2],",","nu = ",sim_pars2[3],",","q = ",sim_pars2[4],sep = ""))
# pdf(file = paste(path,"/LL_surface_q_vs_alpha.pdf",sep=''))
# myheatmap2(x=1:maxalpha,y=q_grid,z=res,x.name="alpha",y.name="q",z.name="LL",plot.title = paste("LL evaluated for a tree simulated under pars:\n","lambda = ",sim_pars2[1],",","mu = ",sim_pars2[2],",","nu = ",sim_pars2[3],",","q = ",sim_pars2[4],sep = ""))
# grDevices::dev.off()
# myheatmap2(x=1:maxalpha,y=q_grid[1:6],z=exp(Ldiff[,1:6]),x.name="alpha",y.name="q",y.splits = 5)
# myheatmap2(x=1:maxalpha,y=q_grid,z=(abs(Ldiff)),x.name="alpha",y.name="q",z.name="LL\nn difference")


