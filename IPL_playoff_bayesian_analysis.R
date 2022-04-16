#########################################################################
#data processing
#########################################################################
data <- read.csv("ipldata.csv", header=TRUE)
#exclude data of 2008, 2009 and 2010 due to different structure of playoffs
ipl.data <- data[!(data$Year==2008|data$Year==2009|data$Year==2010),c(1,2,12,4)] 
#check the dataset
head(ipl.data)

#make the end position (position at end of tournament) a factor
ipl.data$End_Position <- as.factor(ipl.data$End_Position)

#make the end position (position at end of league stage / beginning of playoff) a factor
ipl.data$League_Position <- as.factor(ipl.data$League_Position)

#dataset for team finishing league stage at position 1
data1 <- ipl.data[ipl.data$League_Position==1,] 

#dataset for team finishing league stage at position 2
data2 <- ipl.data[ipl.data$League_Position==2,] 

#dataset for team finishing league stage at position 3
data3 <- ipl.data[ipl.data$League_Position==3,]

#dataset for team finishing league stage at position 4
data4 <- ipl.data[ipl.data$League_Position==4,]

#########################################################################
## JAGS Model ##
#########################################################################
library(rjags)
library(reshape2)
library(ggplot2)
library(plyr)
library(Rmisc)

jags_model = "
model {
  for (i in 1:n){
    y[i] ~ dcat(prob[])
  }
  prob ~ ddirich(a)
}"

##############################################
##for team placed first in the league stages##
##############################################
dat1 = list(n = length(data1$End_Position), y = data1$End_Position, a=rep(1,3))
m1 = jags.model(textConnection(jags_model), data=dat1, n.chains = 3)
res1 =coda.samples(m1, 'prob', 1000)
summary(res1)
plot(res1)

# plot to compare posterior density plot of pi
dr1 = ldply(res1, function(x) {as.data.frame(x)})
m1 = melt(dr1[,grep("prob", names(dr1))], variable.name="estimate", value.name="sample")
(ggplot(m1, aes(x=sample, color=estimate))+geom_density())

###############################################
##for team placed second in the league stages##
###############################################
dat2 = list(n = length(data2$End_Position), y = data2$End_Position, a=rep(1,3))
m2 = jags.model(textConnection(jags_model), data=dat2, n.chains = 3)
res2 =coda.samples(m2, 'prob', 1000)
summary(res2)
plot(res2)

# plot to compare posterior of pi
dr2 = ldply(res2, function(x) {as.data.frame(x)})
m2 = melt(dr2[,grep("prob", names(dr2))], variable.name="estimate", value.name="sample")
(ggplot(m2, aes(x=sample, color=estimate))+geom_density())


###############################################
##for team placed third in the league stages##
###############################################
dat3 = list(n = length(data3$End_Position), y = data3$End_Position, a=rep(1,4))
m3 = jags.model(textConnection(jags_model), data=dat3, n.chains = 3)
res3 =coda.samples(m3, 'prob', 1000)
summary(res3)
plot(res3)

# plot to compare posterior of pi
dr3 = ldply(res3, function(x) {as.data.frame(x)})
m3 = melt(dr3[,grep("prob", names(dr3))], variable.name="estimate", value.name="sample")
(ggplot(m3, aes(x=sample, color=estimate))+geom_density())


###############################################
##for team placed forth in the league stages##
###############################################
dat4 = list(n = length(data4$End_Position), y = data4$End_Position, a=rep(1,4))
m4 = jags.model(textConnection(jags_model), data=dat4, n.chains = 3)
res4 =coda.samples(m4, 'prob', 1000)
summary(res4)
plot(res4)

# plot to compare posterior of pi
dr4 = ldply(res4, function(x) {as.data.frame(x)})
m4 = melt(dr4[,grep("prob", names(dr4))], variable.name="estimate", value.name="sample")
(ggplot(m4, aes(x=sample, color=estimate))+geom_density())

#########
# Plots #
#########
library(bayesplot)
library(mcmcOutput)
pos1 <- mcmcOutput(res1) 
pos2 <- mcmcOutput(res2)
pos3 <- mcmcOutput(res3)
pos4 <- mcmcOutput(res4)

data.seed.1 <- cbind(pos1$prob)
data.seed.2 <- cbind(pos2$prob)
data.seed.3 <- cbind(pos3$prob)
data.seed.4 <- cbind(pos4$prob)

colnames(data.seed.1) <- colnames(data.seed.2) <- paste0("pos[", 1:3, "]")
colnames(data.seed.3) <- colnames(data.seed.4) <- paste0("pos[", 1:4, "]")

#for seed 1
colnames(data.seed.1) <- c("winner", "runner up", "3rd place")
a1 <- mcmc_intervals_data(data.seed.1, prob = 0.9)

ggplot(a1, aes(x = m, y = parameter, color = parameter))+
  geom_linerange(aes(xmin = l, xmax = h), size=2)+
  geom_point(color="black", size=3)+
  ylab("result") + xlab("probability") + xlim(0, 1)+
  ggtitle("Posterior probability credible intervals for team placed first in group stage")


#for seed 2
colnames(data.seed.2) <- c("winner", "runner up", "3rd place")
a2 <- mcmc_intervals_data(data.seed.2, prob = 0.9, prob_outer = 0.9)

ggplot(a2, aes(x = m, y = parameter, color = parameter))+
  geom_linerange(aes(xmin = l, xmax = h), size=2)+
  geom_linerange(aes(xmin = ll, xmax = hh))+
  geom_point(color="black", size=3)+xlim(0, 1)+
  ylab("result") + xlab("probability")+
  ggtitle("Posterior probability credible intervals for team placed second in group stage")


#for seed 3
colnames(data.seed.3) <- c("winner", "runner up", "3rd place", "4th place")
a3 <- mcmc_intervals_data(data.seed.3, prob = 0.9, prob_outer = 0.9)

ggplot(a3, aes(x = m, y = parameter, color = parameter))+
  geom_linerange(aes(xmin = l, xmax = h), size=2)+
  geom_linerange(aes(xmin = ll, xmax = hh), size=3)+
  geom_point(position = pos, color="black")+xlim(0, 1)+
  ylab("result") + xlab("probability") + 
  ggtitle("Posterior probability credible intervals for team placed third in group stage")


#for seed 3
colnames(data.seed.4) <- c("winner", "runner up", "3rd place", "4th place")
a4 <- mcmc_intervals_data(data.seed.4, prob = 0.9, prob_outer = 0.9)

ggplot(a4, aes(x = m, y = parameter, color = parameter))+
  geom_linerange(aes(xmin = l, xmax = h), size=2)+
  geom_point(position = pos, color="black", size=3)+xlim(0, 1)+
  ylab("result") + xlab("probability") + 
  ggtitle("Posterior probability credible intervals for team placed forth in group stage")