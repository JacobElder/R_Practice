a = 2  #desired slope
b = 1  #estimated intercept
sd = 20  #estimated variability defined by standard deviation
nsim = 400  #400 simulations
pval = numeric(nsim)  #placeholder for the second for loop output
Nvec = seq(25, 100, by = 1)  #vector for the range of sample sizes to be tested
power.N = numeric(length(Nvec))   #create placeholder for first for loop output
for (j in 1:length(Nvec)) {
  N = Nvec[j]  
  x = seq(1, 20, length = Nvec[j])  #x value length needs to match sample size (Nvec) length
  for (i in 1:nsim) {   #for this value of N, create random error 400 times
    y_det = a + b * x
    y = rnorm(N, mean = y_det, sd = sd)
    m = lm(y ~ x)
    pval[i] = coef(summary(m))["x", "Pr(>|t|)"]  #all the p values for 400 sims
  }  #cycle through all N values
  power.N[j] = sum(pval < 0.05)/nsim  #the proportion of correct p-values (i.e the power)
}
power.N
plot(Nvec, power.N)  #need about 90 - 100 samples for 80% power


set.seed(666)
x1 = rnorm(1000)           # some continuous variables 
x2 = rnorm(1000)
z = 1 + 2*x1 + 3*x2        # linear combination with a bias
pr = 1/(1+exp(-z))         # pass through an inv-logit function
y = rbinom(1000,1,pr)      # bernoulli response variable


samplSizes <- seq(from=25,to=500, by=5)
diffs <- c(1,2,3,4)
powerMat <- matrix(nrow=length(samplSizes)*length(diffs),ncol=3)
iter <- 0
for(N in samplSizes){
  for(d in diffs){
    simMat <- matrix(nrow=1000,ncol=3)
    for(s in 1:nsims){
      labelnames <- c("X1", "a", "b", "X2", "c", "d")
      mu1 <- rnorm(1,0,1)
      mu2 <- (mu1+d)
      mu3 <- rnorm(1,10,1)
      mu4 <- (mu3-d)
      sd1 <- abs(rnorm(1,0,15))
      design_result <- ANOVA_design(design = "2b*2b", 
                                    n = N, 
                                    mu = c(mu1, mu2, mu3, mu4), 
                                    sd = sd1, 
                                    labelnames = labelnames,
                                    plot = F) 
      output<-power_twoway_between(design_result)
      simMat[s,] <- c(N,output$power_AB,d)
    }
    iter <- iter + 1
    powerMat[iter, ] <- c(colMeans(simMat,na.rm = T))
  }
}

powerDf <- as.data.frame(powerMat)
colnames(powerDf) <- c("N","power","diff")
powerDf$diff <- as.factor(powerDf$diff)
library(ggplot2)

ggplot(powerDf, aes(x=N, y=power, group=diff)) +
  geom_line(aes(color=diff))+
  geom_point(aes(color=diff))

samplSizes <- seq(from=25,to=500, by=5)
diffs <- c(1,2,3,4)
powerMat3w <- matrix(nrow=length(samplSizes)*length(diffs)*length(diffs),ncol=4)
iter <- 0
for(N in samplSizes){
  for(d in diffs){
    for(d2 in diffs){
    simMat <- matrix(nrow=1000,ncol=4)
    for(s in 1:nsims){
      labelnames <- c("X1", "a", "b", "X2", "c", "d", "X3", "e", "f")
      mu1 <- rnorm(1,0,1)
      mu2 <- (mu1+d)
      mu3 <- rnorm(1,10,1)
      mu4 <- (mu3-d)
      mu5 <- rnorm(1,0,1)
      mu6 <- (mu5-d2)
      mu7 <- rnorm(1,10,1)
      mu8 <- (mu7+d2)
      sd1 <- abs(rnorm(1,0,15))
      design_result <- ANOVA_design(design = "2b*2b*2b", 
                                    n = N, 
                                    mu = c(mu1, mu2, mu3, mu4, mu5, mu6, mu7, mu8), 
                                    sd = sd1, 
                                    labelnames = labelnames,
                                    plot = F) 
      output<-power_threeway_between(design_result)
      simMat[s,] <- c(N,output$power_AB,d,d2)
    }
    iter <- iter + 1
    powerMat3w[iter, ] <- c(colMeans(simMat,na.rm = T))
    }
  }
}

powerDf3w <- as.data.frame(powerMat3w)
colnames(powerDf3w) <- c("N","power","diff","diff2")
powerDf3w$diff <- as.factor(powerDf3w$diff)
powerDf3w$diff2 <- as.factor(powerDf3w$diff2)

ggplot(powerDf, aes(x=N, y=power, group=diff)) +
  geom_line(aes(color=diff))+
  geom_point(aes(color=diff))

ggplot(powerDf3w, aes(N, power, group = interaction(diff, diff2), 
               color = diff, linetype = diff2)) +
  geom_line() + geom_point()

