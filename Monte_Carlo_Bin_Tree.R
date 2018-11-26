#Monte Carlo Simulation
S0<-153.6900
mu<-0.01268
sigma<-mu*sqrt(252)
r<-0.03
option<-function(n,Call=TRUE){ #This function set the call option as default option, if input is FALSE it will
                               #given the value of corresponding put option
  Z<-rnorm(n)
  St<-S0*exp((r-0.5*sigma^2)*1+sigma*sqrt(1)*Z)
  if(Call==TRUE)
    V<-St-S0
  else
    V<-S0-St
  V[V<0]<-0
  Pmean<-mean(V)
  Pmean
}
#Call option with K=S, T=1 year
option(100)
option(1000)
option(5000)
option(10000)
option(50000)
option(100000)
option(500000)
option(1000000)
#Put option with K=S, T=1 year
option(100,FALSE)
option(1000,FALSE)
option(5000,FALSE)
option(10000,FALSE)
option(50000,FALSE)
option(100000,FALSE)
option(500000,FALSE)
option(1000000,FALSE)
#Binomial Tree
#Calculate the u
uVal<-function(m){
  u<-exp(sigma*sqrt(1/m)) 
  u
}
#Calculate the d
dVal<-function(u){
  d<-1/u
  d
}
#Calculate the upper probability
prob<-function(m){
  DeltaT<-1/m
  u<-uVal(m)
  d<-dVal(u)
  q<-(exp(r*DeltaT)-d)/(u-d)
  q
}
#Calculate the stock price of each knot in each period
StockEnd<-function(m,u){
  d<-dVal(u)
  numb<-m+1
  EndP<-c(1:numb)*0
  for(i in 1:numb){
    UTime<-m-i+1
    upper<-u^UTime
    LTime<-i-1
    lower<-d^LTime
    EndP[i]<-upper*lower*S0}
  EndP
}
#Calculate the probability of each knot
EndProb<-function(m){
  q<-prob(m)
  numb<-m+1
  ProbE<-c(1:numb)*0
  for(i in 1:numb){
    Comb<-factorial(m)/(factorial(m-i+1)*factorial(i-1))
    UTime<-m-i+1
    LTime<-i-1
    upper<-q^UTime
    lower<-(1-q)^LTime
    ProbE[i]<-Comb*upper*lower
  }
  ProbE
}
#Calculate the value of European option (the required input is m, strike price and option type)
#the default option is a call option
Eoption<-function(m,K,Call=TRUE){
  u<-uVal(m)
  if(Call==TRUE)
  OptionE<-StockEnd(m,u)-K
  else
    OptionE<-K-StockEnd(m,u)
  OptionE[OptionE<0]<-0
  InitialV<-sum(OptionE*EndProb(m))*exp(-1*r)
  c(InitialV,u,1/u,prob(m))
}
#Calculate the value of American option (the required input is m, strike price and option type)
#the default option is call option
Aoption<-function(m,K,Call=TRUE){
  q<-prob(m)
  u<-uVal(m)
  numb<-m+1
  if(Call==TRUE)
    EndPrice<-StockEnd(m,u)-K
  else
    EndPrice<-K-StockEnd(m,u)
  EndPrice[EndPrice<0]<-0
  for(j in m:1){
    BackPrice<-c(1:j)*0
  for(i in 1:j){
    BackPrice[i]<-(EndPrice[i]*q+EndPrice[i+1]*(1-q))*exp(-1/m*r)}
    if(Call==TRUE)
      EndPrice<-StockEnd(j-1,u)-K
    else
      EndPrice<-K-StockEnd(j-1,u)
    EndPrice[EndPrice<0]<-0
    for(k in 1:length(EndPrice)){
      if(EndPrice[k]<BackPrice[k])
        EndPrice[k]<-BackPrice[k]
    }
  }
  c(EndPrice,u,1/u,prob(m))
  
}
#The Output will be European call price with its u, d, p
Eoption(4,S0)
Eoption(8,S0)
Eoption(16,S0)
Eoption(32,S0)
Eoption(100,S0)
Eoption(500,S0)
#The Output will be American call price with its u, d, p
Aoption(4,S0)
Aoption(8,S0)
Aoption(16,S0)
Aoption(32,S0)
Aoption(100,S0)
Aoption(500,S0)
#The Output will be European put price with its u, d, p
Eoption(4,S0,FALSE)
Eoption(8,S0,FALSE)
Eoption(16,S0,FALSE)
Eoption(32,S0,FALSE)
Eoption(100,S0,FALSE)
Eoption(500,S0,FALSE)
#The Output will be American put price with its u, d, p
Aoption(4,S0,FALSE)
Aoption(8,S0,FALSE)
Aoption(16,S0,FALSE)
Aoption(32,S0,FALSE)
Aoption(100,S0,FALSE)
Aoption(500,S0,FALSE)

