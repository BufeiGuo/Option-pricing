#Multi-asset
#Under estimation of value of basket call on two asset
S0<-100 #Suppose the initial stock price is 100
L<-2;w1<-1/2;w2<-1/2;sigma1<-0.3;sigma2<-0.3;T<-1;rho<-0.5;r<-0.05;K<-S0
sigmaSquare<-(1/L^2)*2*rho*sigma1*sigma2
Gt<-(S0*S0)^(1/L)
d1<-(log(Gt/K)+(r+sigmaSquare-1/(2*L)*(sigma1^2+sigma2^2))*T)/(sqrt(sigmaSquare)*T)
d2<-d1-(sqrt(sigmaSquare)*T)
Expectation<-Gt*exp((r-1/(2*L)*(sigma1^2+sigma2^2))*T+1/2*sigmaSquare*T)
Expectation*pnorm(d1)-K*pnorm(d2)
#Under Monte Carlo Simulation
option1<-function(n){
  Z<-rnorm(n)
  S<-(S0*S0)^(1/L)*exp((r-0.5*sigmaSquare^2)*T+sigmaSquare*sqrt(T)*Z)
  V<-S-K
  V[V<0]<-0
  Pmean<-mean(V)
  Pmean
}
#Under n=100,1000,5000,10000,50000,100000,500000 and 1000000
option1(100)
option1(1000)
option1(5000)
option1(10000)
option1(50000)
option1(100000)
option1(500000)
option1(1000000)
#Under Binomial method
#Calculate the u
uVal<-function(m){
  u<-exp(sigmaSquare*sqrt(1/m)) 
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
StockEnd<-function(m){
  u<-uVal(m)
  d<-dVal(u)
  numb<-m+1
  EndP<-c(1:numb)*0
  for(i in 1:numb){
    UTime<-m-i+1
    upper<-u^UTime
    LTime<-i-1
    lower<-d^LTime
    EndP[i]<-upper*lower*(S0*S0)^(1/L)}
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
option2<-function(m){
  u<-uVal(m)
OptionE<-StockEnd(m)-K
  OptionE[OptionE<0]<-0
  InitialV<-sum(OptionE*EndProb(m))*exp(-1*r)
  InitialV
}
#Under m=4,8,16,32,100 and 500
option2(4)
option2(8)
option2(16)
option2(32)
option2(100)
option2(500)
