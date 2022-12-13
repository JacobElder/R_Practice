

logThreeContInter <- function(N,b0,b1,b2,b3,b4,b5){
  # coefficients input in odds ratios
  
  x1 = rnorm(N)           # a continuous variable 
  #x2 = rbinom(N,1,.5)
 #x2 = sample(c(1,2,3), N, replace = T, prob = c(.33,.33,.33))
  xcatdum <- t(rmultinom(N, c(1,2,3), prob=c(.33,.33,.33))) # a three level categorical dummy matrix
  xcatdumvect <- apply(xcatdum, 1, function(x) which(x==1)) # create a vector of 1, 2, 3 for dummy variable in df
  xcatdumcoef <- cbind(1, xcatdum[,-1]) %*% c(b0, b2,b3) # matrix multplication of dummy coded coefficients
  
  # log odds
  lb0 <- log(b0)
  lb1 <- log(b1)
  lb2 <- log(b2)
  lb3 <- log(b3)
  lb4 <- log(b4)
  lb5 <- log(b5)
  
  # coefficients for reference group and categorical main effects is xcatumcoef containing b0, b2, b3
  # lb1*x1 is slope for dummy reference group
  # lb4*x1*xcat[,2] is slope for second level
  # lb5*x1*xcat[,3] is slope for third level
  
  # z = xcatdumcoef + lb1*x1 + lb4*x1*xcatdum[,2] + lb5*x1*xcatdum[,3]
  z = xcatdumcoef + lb1*x1*xcatdum[,1] + 
    ( lb4 * (lb1*x1*xcatdum[,2]) - (lb1*x1*xcatdum[,1]) ) + 
    (lb5 *(lb1*x1*xcatdum[,3]) - (lb1*x1*xcatdum[,1]) )
  pr = 1/(1+exp(-z))      # inverse logit 
  y = rbinom(N,1,pr)      # bernoulli response
  
  #now add it to dataframe:
  df = data.frame(y=y,x1=x1,x2=as.factor(xcatdumvect))
  
  return(df)
}

logBinContInter <- function(N,b0,b1,b2,b3){
  x1 = rnorm(N)           # some continuous variables 
  x2 = rbinom(N,1,.5)
  
  b0 <- log(b0)
  b1 <- log(b1)
  b2 <- log(b2)
  b3 <- log(b3)
  
  z = b0 + b1*x1 + b2*x2 + b3*x1*x2   # linear combination with a bias
  pr = 1/(1+exp(-z))         # pass through an inv-logit function
  y = rbinom(N,1,pr)      # bernoulli response variable
  
  #now feed it to glm:
  df = data.frame(y=y,x1=x1,x2=as.factor(x2))
  
  return(df)
}

set.seed(500)

test<-logBinContInter(300,1,1.5,1.1, 1.8)
m<-glm( y~x1*x2,data=test,family="binomial")
exp(m$coefficients)

iter <- 1000
samples <- seq(from=150,to=500,by=25)
powerMat <- matrix(nrow=length(samples),ncol=2)
for(t in 1:length(samples)){
  i <- samples[t]
  simMat <- matrix(nrow=iter,ncol=1)
  for(s in 1:iter){
    curDf<-logBinContInter(i,1,1.5,1,2)
    m<-glm( y~x1*x2,data=curDf,family="binomial")
    simMat[s,] <- summary(m)$coefficients[16]
  }
  powerMat[t,] <- c(i,mean(simMat[,1]<.05) )
}
