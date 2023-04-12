
library(tidyverse)
library(ggeffects)
library(mnormt)
library(lmerTest)
library(NatParksPalettes)
library(ggplot2)

set.seed(2208)  # set the seed
J <- 20  # number of individuals (clusters)
cs <- 4  # number of time points (cluster size)
gam <- c(0, 0.5)  # fixed effects
G <- matrix(c(0.25, 0, 
              0, 0.125), nrow = 2)  # random effect variances (G-matrix)
sigma2 <- 1  # within-person variance (lv-1)
X <- cbind(1, seq_len(cs) - 1)  # for each individual
X <- X[rep(seq_len(cs), J), ]  # repeat each row cs times
pid <- seq_len(J)  # individual id
pid <- rep(pid, each = cs)  # repeat each ID cs times
# Generate person-level (lv-2) random effects
uj <- rmnorm(J, mean = rep(0, 2), varcov = G)
# Generate repeated-measure-level (lv-1) error term
eij <- rnorm(J * cs, sd = sqrt(sigma2))
# Compute beta_j's
betaj <- matrix(gam, nrow = J, ncol = 2, byrow = TRUE) + uj
# Compute outcome:
y <- rowSums(X * betaj[pid, ]) + eij
# Form a data frame
sim_dat1 <- tibble(y, time = X[ , 2], pid)

pal <- colorRampPalette(natparks.pals("DeathValley"))(length(unique(sim_dat1$pid)))
sim_dat1$pid <- as.factor(sim_dat1$pid)

m <- lmer(y ~ time + (time | pid), data = sim_dat1)

mer <- ggpredict(m, terms = c("time", "pid"), type = "re")
mef <- ggpredict(m, terms = c("time"))

plot <- ggplot(mef, aes(x, predicted)) + 
  geom_line(data=mer, aes(x=x,y=predicted, color=group), alpha= .50)  + 
  scale_color_manual(values=pal) +  
  geom_line() + 
  geom_ribbon(aes(ymin=conf.low, ymax=conf.high), alpha=0.30) + 
  theme(axis.text=element_text(size=12), axis.title=element_text(size=12,face="bold"), legend.text = element_text(size=12), panel.border = element_rect(colour = "black", fill = NA, size =1), legend.title = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), axis.line = element_line(colour = "black"), legend.position = "none") + 
  xlab("Time") + 
  ylab("Y")
plot
