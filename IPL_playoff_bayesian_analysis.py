import numpy as np
import pandas as pd
import matplotlib.pyplot as plt
import pystan
import arviz # to plot posterior and traceplots

df = pd.read_csv("ipldata.csv")
print(df.head())

#data processing
ipl = df[df["Year"]>2010] #consider data from year 2011 upto 2021
print(ipl.head())

#separate the data for teams placed first upto fourth in league stages
ipl_pos1 = ipl[ipl["League_Position"].isin([1])]
ipl_pos2 = ipl[ipl["League_Position"].isin([2])]
ipl_pos3 = ipl[ipl["League_Position"].isin([3])]
ipl_pos4 = ipl[ipl["League_Position"].isin([4])]

#STAN model (Categorical-Dirichlet)
categorical_stan="""
data {
  int<lower = 1> N;
  int<lower = 1> m;
  int<lower = 1, upper = m> y[N];
}
transformed data {}
parameters {
  simplex[m] prob;
}
transformed parameters {}
model {
  prob ~ dirichlet(rep_vector(1, m));
  for(n in 1:N)
      y[n] ~ categorical(prob);
}
generated quantities{}
"""

# model for the posterior probabilities for team placed first at end of league stage
dat_pos1 = {
    "N": len(ipl_pos1.index),
    "m": 3,
    "y": ipl_pos1["End_Position"]
}
fit_pos1 = pystan.stan(model_code = categorical_stan, data=dat_pos1, iter=10000, chains=1)
print(fit_pos1)  # highest probability of finishing second
arviz.plot_trace(fit_pos1) # posterior density and traceplots

# model for the posterior probabilities for team placed second at end of league stage
dat_pos2 = {
    "N": len(ipl_pos2.index),
    "m": 3,
    "y": ipl_pos2["End_Position"]
}
fit_pos2 = pystan.stan(model_code = categorical_stan, data=dat_pos2, iter=10000, chains=3)
print(fit_pos2) # highest probability of finishing first
arviz.plot_trace(fit_pos2) # posterior density and traceplots

# model for the posterior probabilities for team placed third at end of league stage
dat_pos3 = {
    "N": len(ipl_pos3.index),
    "m": 4,
    "y": ipl_pos3["End_Position"]
}
fit_pos3 = pystan.stan(model_code = categorical_stan, data=dat_pos3, iter=10000, chains=1)
print(fit_pos3)  # highest probability of finishing third
arviz.plot_trace(fit_pos3) # posterior density and traceplots

# model for the posterior probabilities for team placed fourth at end of league stage
dat_pos4 = {
    "N": len(ipl_pos4.index),
    "m": 4,
    "y": ipl_pos4["End_Position"]
}
fit_pos4 = pystan.stan(model_code = categorical_stan, data=dat_pos4, iter=10000, chains=1)
print(fit_pos4)  # highest probability of finishing fourth
arviz.plot_trace(fit_pos4) # posterior density and traceplots
