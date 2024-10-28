
library(ggplot2)



# true parametric (exponential) log likelihood
LL_OML_Exp<-function(Parms,x,y,p){
  nParms<-length(Parms)
  ones<-matrix(ncol=1,nrow=length(y))
  ones[,1]<-1
  x1<-cbind(ones,x)
  q<-matrix(ncol=1,nrow=length(y))
  xB<-matrix(ncol=nParms,nrow=length(y))
  for(i in 1:length(y)){
    for(j in 1:nParms){
      xB[i,j]<-Parms[j]*x1[i,j]
    }
    q[i,1]<-sum(xB[i,])
  }
  lambda<-log(2)/q
  f<-lambda*exp(-y*lambda)
  LL<-sum(log(f))
  return(LL)
}



x<-matrix(ncol=3,nrow=length(y))
x[,1]<-2
x[,2]<-3
x[,3]<-4

Parms<-c(1,2,3,4)

# log likelihood for any order approximation
LL<-function(Parms,y,x,p,order,nParms){
  ones<-matrix(ncol=1,nrow=length(y))
  ones[,1]<-1
  x1<-cbind(ones,x)
  q<-matrix(ncol=1,nrow=length(y))
  xB<-matrix(ncol=nParms,nrow=length(y))
  for(i in 1:length(y)){
    for(j in 1:nParms){
      xB[i,j]<-Parms[j]*x1[i,j]
    }
    q[i,1]<-sum(xB[i,])
  }
  alpha<-vector(length=order)
  for(j in 1:order){
    alpha[j]<-Parms[j+nParms]
  }
  Tau<-log(p/(1-p))
  terms<-matrix(nrow=length(y),ncol=order)
  for(j in 1:order){
    terms[,j]<-(y[]-q[])^j*alpha[j]/factorial(j)
  }
  LF<-vector(length=length(y))
  for(j in 1:length(y)){
    LF[j]<-Tau+sum(terms[j,])
  }
  dTerms<-matrix(nrow=length(y),ncol=order)
  for(j in 1:order){
    dTerms[,j]<-(y[]-q)^(j-1)*alpha[j]/factorial(j-1)
  }
  dLF<-vector(length=length(y))
  for(j in 1:length(y)){
    dLF[j]<-sum(dTerms[j,])
  }
  L<-vector(length=length(y))
  for(j in 1:length(y)){
    L[j]<-1/(1+exp(-LF[j]))*1/(1+exp(LF[j]))*dLF[j]
  }
  LL<-log(L)
  return(sum(LL))
}


LL(c(1,1,1,1,1,1),y=y,x=x,p=p,order=2,nParms=4)

quantileReg<-function(Parms,y,x,p,maxOrder){
    nParms<-length(Parms)
    # fit models
    fit1<-optim(par=c(Parms,0.00001),fn=LL,y=y,x=x,nParms=nParms,p=p,order=1,control=list(fnscale=-1))
    fit<-fit1
    fitTemp<-fit1
    for(j in 2:maxOrder){
      fitTempLast<-fitTemp
      fitTemp<-optim(par=c(fitTempLast$par,0),fn=LL,y=y,x=x,nParms=nParms,p=p,order=j,control=list(fnscale=-1))
      parLast<-c(fitTempLast$par,0)
      parDif<-fitTemp$par-parLast
      LLDif<-fitTemp$value-fitTempLast$value
      fitTemp$parDif<-parDif
      fitTemp$LLDif<-LLDif
      fit<-c(fit,fitTemp)
    }
    return(fit)
}

quantileReg(1,y=y,p=0.5,maxOrder=50)

median(y)
Parms<-1
maxOrder<-3
j<-3
optim(1,LL_True,method="Brent",lower=0.0001,upper=200,control=list(fnscale=-1),y=y)

# set number of simulation iterations
iterations<-100

# set sample size
N<-1000

# set percentile to model (as a proportion)
p<-0.5

# set regression coefficients
B0<-1
B1<-0.1
B2<--0
B3<-0

Start<-c(1.2,0.2)
#Start<-c(1,0.1,0,0)

# set the maximum order of approximation
maxOrder<-7

# create empty results matrix
Results<-matrix(ncol=4+2*maxOrder,nrow=iterations)

# simulation loop
for(iteration in 1:iterations){
  
      # generate predictors
      set.seed(iteration)
      X1<-runif(n=N,min=0,max=30)
      X2<-rbinom(n=N,size=1,prob=0.5)
      X3<-X1*X2
      x<-cbind(X1,X2,X3)

      
      # define median
      median<-B0+B1*X1+B2*X2+B3*X3
      
      
      # define exponential rate/intensity parameter
      lambda<-log(2)/median
      
      # generate a sample from the exponential distribution
      y<-rexp(n=N,rate=lambda)
      
      # define Tau=logit(F(quantile))
      Tau<-log(p/(1-p))
      
      fit_OML<-optim(par=Start,fn=LL_OML_Exp,x=x,y=y,p=p,control=list(fnscale=-1))
      
      optim(par=Start,fn=LL_OML_Exp,x=x,y=y,p=p,control=list(fnscale=-1),method="Brent",lower=0.001,upper=20)
      quantileReg(Start,y=y,x=x,p=p,maxOrder=10)
      optim(par=c(Start,0.01,0,0,0,0,0,0,0,0,0),fn=LL,y=y,x=x,p=p,order=10,nParms=2)
      optim(par=c(Start,0.1,0),fn=LL,y=y,x=x,p=p,order=2,nParms=2)

      
      
      
      
      Results[iteration,]<-c(iteration,N,p,fit)
}


Results

means<-data.frame("Iterations"=iterations,"Sample Size"=N,"Percentile Modelled"=p*100,
                  "OLM Estimate"=mean(Results[,4]),
                  "1st-Order Approximation Estimate"=mean(Results[,5]),
                  "2nd-Order Approximation Estimate"=mean(Results[,6]),
                  "3rd-Order Approximation Estimate"=mean(Results[,7]),
                  "4th-Order Approximation Estimate"=mean(Results[,8]),
                  "5th-Order Approximation Estimate"=mean(Results[,9]),
                  "6th-Order Approximation Estimate"=mean(Results[,10]),
                  "7th-Order Approximation Estimate"=mean(Results[,11]),
                  "1st-Order Approximation Estimate (log scale)"=mean(Results[,12]),
                  "2nd-Order Approximation Estimate (log scale)"=mean(Results[,13]),
                  "3rd-Order Approximation Estimate (log scale)"=mean(Results[,14]),
                  "4th-Order Approximation Estimate (log scale)"=mean(Results[,15]),
                  "5th-Order Approximation Estimate (log scale)"=mean(Results[,16]),
                  "6th-Order Approximation Estimate (log scale)"=mean(Results[,17]),
                  "7th-Order Approximation Estimate (log scale)"=mean(Results[,18])
                  )

standardErrors<-data.frame("Iterations"=iterations,"Sample Size"=N,"Percentile Modelled"=p*100,
                  "OLM Estimate"=mean(Results[,4]),
                  "1st-Order Approximation SE"=sd(Results[,5]),
                  "2nd-Order Approximation SE"=sd(Results[,6]),
                  "3rd-Order Approximation SE"=sd(Results[,7]),
                  "4th-Order Approximation SE"=sd(Results[,8]),
                  "5th-Order Approximation SE"=sd(Results[,9]),
                  "6th-Order Approximation SE"=sd(Results[,10]),
                  "7th-Order Approximation SE"=sd(Results[,11]),
                  "1st-Order Approximation SE (log scale)"=sd(Results[,12]),
                  "2nd-Order Approximation SE (log scale)"=sd(Results[,13]),
                  "3rd-Order Approximation SE (log scale)"=sd(Results[,14]),
                  "4th-Order Approximation SE (log scale)"=sd(Results[,15]),
                  "5th-Order Approximation SE (log scale)"=sd(Results[,16]),
                  "6th-Order Approximation SE (log scale)"=sd(Results[,17]),
                  "7th-Order Approximation SE (log scale)"=sd(Results[,18])
)

maxs<-data.frame("Iterations"=iterations,"Sample Size"=N,"Percentile Modelled"=p*100,
                  "OLM Estimate"=mean(Results[,4]),
                  "1st-Order Approximation Max"=max(Results[,5]),
                  "2nd-Order Approximation Max"=max(Results[,6]),
                  "3rd-Order Approximation Max"=max(Results[,7]),
                  "4th-Order Approximation Max"=max(Results[,8]),
                  "5th-Order Approximation Max"=max(Results[,9]),
                 "6th-Order Approximation Max"=max(Results[,10]),
                 "7th-Order Approximation Max"=max(Results[,11]),
                 "1st-Order Approximation Max (log scale)"=max(Results[,12]),
                 "2nd-Order Approximation Max (log scale)"=max(Results[,13]),
                 "3rd-Order Approximation Max (log scale)"=max(Results[,14]),
                 "4th-Order Approximation Max (log scale)"=max(Results[,15]),
                 "5th-Order Approximation Max (log scale)"=max(Results[,16]),
                 "6th-Order Approximation Max (log scale)"=max(Results[,17]),
                 "7th-Order Approximation Max (log scale)"=max(Results[,18])
)

mins<-data.frame("Iterations"=iterations,"Sample Size"=N,"Percentile Modelled"=p*100,
                 "OLM Estimate"=mean(Results[,4]),
                 "1st-Order Approximation Min"=min(Results[,5]),
                 "2nd-Order Approximation Min"=min(Results[,6]),
                 "3rd-Order Approximation Min"=min(Results[,7]),
                 "4th-Order Approximation Min"=min(Results[,8]),
                 "5th-Order Approximation Min"=min(Results[,9]),
                 "6th-Order Approximation Min"=min(Results[,10]),
                 "7th-Order Approximation Min"=min(Results[,11]),
                 "1st-Order Approximation Min (log scale)"=min(Results[,12]),
                 "2nd-Order Approximation Min (log scale)"=min(Results[,13]),
                 "3rd-Order Approximation Min (log scale)"=min(Results[,14]),
                 "4th-Order Approximation Min (log scale)"=min(Results[,15]),
                 "5th-Order Approximation Min (log scale)"=min(Results[,16]),
                 "6th-Order Approximation Min (log scale)"=min(Results[,17]),
                 "7th-Order Approximation Min (log scale)"=min(Results[,18])
)


Order<-1:maxOrder
Mean<-vector(length=maxOrder)
Mean[1]<-means$X1st.Order.Approximation.Estimate
Mean[2]<-means$X2nd.Order.Approximation.Estimate
Mean[3]<-means$X3rd.Order.Approximation.Estimate
Mean[4]<-means$X4th.Order.Approximation.Estimate
Mean[5]<-means$X5th.Order.Approximation.Estimate
Mean[6]<-means$X6th.Order.Approximation.Estimate
Mean[7]<-means$X7th.Order.Approximation.Estimate

Mean_log<-vector(length=maxOrder)
Mean_log[1]<-means$X1st.Order.Approximation.Estimate..log.scale.
Mean_log[2]<-means$X2nd.Order.Approximation.Estimate..log.scale.
Mean_log[3]<-means$X3rd.Order.Approximation.Estimate..log.scale.
Mean_log[4]<-means$X4th.Order.Approximation.Estimate..log.scale.
Mean_log[5]<-means$X5th.Order.Approximation.Estimate..log.scale.
Mean_log[6]<-means$X6th.Order.Approximation.Estimate..log.scale.
Mean_log[7]<-means$X7th.Order.Approximation.Estimate..log.scale.

standardError<-vector(length=maxOrder)
standardError[1]<-standardErrors$X1st.Order.Approximation.SE
standardError[2]<-standardErrors$X2nd.Order.Approximation.SE
standardError[3]<-standardErrors$X3rd.Order.Approximation.SE
standardError[4]<-standardErrors$X4th.Order.Approximation.SE
standardError[5]<-standardErrors$X5th.Order.Approximation.SE
standardError[6]<-standardErrors$X6th.Order.Approximation.SE
standardError[7]<-standardErrors$X7th.Order.Approximation.SE

standardError_log<-vector(length=maxOrder)
standardError_log[1]<-standardErrors$X1st.Order.Approximation.SE..log.scale.
standardError_log[2]<-standardErrors$X2nd.Order.Approximation.SE..log.scale.
standardError_log[3]<-standardErrors$X3rd.Order.Approximation.SE..log.scale.
standardError_log[4]<-standardErrors$X4th.Order.Approximation.SE..log.scale.
standardError_log[5]<-standardErrors$X5th.Order.Approximation.SE..log.scale.
standardError_log[6]<-standardErrors$X6th.Order.Approximation.SE..log.scale.
standardError_log[7]<-standardErrors$X7th.Order.Approximation.SE..log.scale.

min<-vector(length=maxOrder)
min[1]<-mins$X1st.Order.Approximation.Min
min[2]<-mins$X2nd.Order.Approximation.Min
min[3]<-mins$X3rd.Order.Approximation.Min
min[4]<-mins$X4th.Order.Approximation.Min
min[5]<-mins$X5th.Order.Approximation.Min
min[6]<-mins$X6th.Order.Approximation.Min
min[7]<-mins$X7th.Order.Approximation.Min

min_log<-vector(length=maxOrder)
min_log[1]<-mins$X1st.Order.Approximation.Min..log.scale.
min_log[2]<-mins$X2nd.Order.Approximation.Min..log.scale.
min_log[3]<-mins$X3rd.Order.Approximation.Min..log.scale.
min_log[4]<-mins$X4th.Order.Approximation.Min..log.scale.
min_log[5]<-mins$X5th.Order.Approximation.Min..log.scale.
min_log[6]<-mins$X6th.Order.Approximation.Min..log.scale.
min_log[7]<-mins$X7th.Order.Approximation.Min..log.scale.

max<-vector(length=maxOrder)
max[1]<-maxs$X1st.Order.Approximation.Max
max[2]<-maxs$X2nd.Order.Approximation.Max
max[3]<-maxs$X3rd.Order.Approximation.Max
max[4]<-maxs$X4th.Order.Approximation.Max
max[5]<-maxs$X5th.Order.Approximation.Max
max[6]<-maxs$X6th.Order.Approximation.Max
max[7]<-maxs$X7th.Order.Approximation.Max

max_log<-vector(length=maxOrder)
max_log[1]<-maxs$X1st.Order.Approximation.Max..log.scale.
max_log[2]<-maxs$X2nd.Order.Approximation.Max..log.scale.
max_log[3]<-maxs$X3rd.Order.Approximation.Max..log.scale.
max_log[4]<-maxs$X4th.Order.Approximation.Max..log.scale.
max_log[5]<-maxs$X5th.Order.Approximation.Max..log.scale.
max_log[6]<-maxs$X6th.Order.Approximation.Max..log.scale.
max_log[7]<-maxs$X7th.Order.Approximation.Max..log.scale.


ggplot()+
  geom_line(aes(x=Order,y=Mean),color="blue")+
  geom_line(aes(x=Order,y=Mean_log),color="green")+
  geom_line(aes(x=Order,y=median))+
  geom_line(aes(x=Order,y=means$OLM.Estimate),color="red")+
  ylim(0,2)



