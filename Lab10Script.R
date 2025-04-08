#Lab 10
install.packages("tidyverse")
install.packages("ggplot2")
install.packages("patchwork")
library(tidyverse)
library(ggplot2)
library(patchwork)

simulations = 10000
sample.size1 = 1004
p.true = 0.39
poll.data1 = rbinom(simulations, sample.size1, p.true)/sample.size1
df1 = data.frame(x=poll.data1)
histogram.plot1 = ggplot(data = df1, aes(x=x))+         
  geom_histogram(aes(y=after_stat(density)),
                 breaks=seq(0.3,0.5,0.01)) +
  geom_hline(yintercept=0)+                      
  theme_bw()+                                     
  geom_density(color = "blue") +
  xlab("Sample Propportion")+                 
  ylab("Density")+             
  labs(color = "", title = "Proportion of people satisfied from polls with n = 1004")
histogram.plot1

range1 = quantile(poll.data1, c(0.025, 0.975)) #(.3595618, .4203187), range = 0.0607569
MOE1 = (range1[2]-range1[1])/2 #MOE = 0.03037845, approx 3%

sample.size2 = 2008
poll.data2 = rbinom(simulations, sample.size2, p.true)/sample.size2
df2 = data.frame(x=poll.data2)
histogram.plot2 = ggplot(data = df2, aes(x=x))+                 
  geom_histogram(aes(y=after_stat(density)),
                 breaks=seq(0.3,0.5,0.01)) +
  geom_hline(yintercept=0)+                                 
  theme_bw()+                                                     
  geom_density(color = "blue") +
  xlab("Sample Propportion")+                                     
  ylab("Density")+                              
  labs(color = "", title = "Proportion of people satisfied from polls with n = 2008")
histogram.plot2

range2 = quantile(poll.data2, c(0.025, 0.975)) #(0.3685259, 0.4113546), range = 0.0428287 
MOE2 = (range2[2]-range2[1])/2 #MOE = 0.02141435, approx 2%

#Task 2: resampling
n = 1004
yes = round(n*p.true)
og.sample = tibble(id = 1:n,
                   measurement = c(rep(1, yes), rep(0,n-yes)))
og.sample |>
  summarize(mean = mean(measurement))
resamples = numeric(10000)
for(i in 1:10000){
  resample = sample(x = og.sample$measurement,
                    size = n,
                    replace = T)
  resamples[i] = mean(resample)
}
resamples.df = data.frame(x = resamples)
resampling.plot = ggplot(data = resamples.df, aes(x=x))+                 
  geom_histogram(aes(y=after_stat(density)),
                 breaks=seq(0.3,0.5,0.01)) +
  geom_hline(yintercept=0)+                                 
  theme_bw()+                                                     
  geom_density(color = "blue") +
  xlab("Resample Propportion")+                                     
  ylab("Density")+                              
  labs(color = "", title = "Resampling proportion of people satisfied with n = 1004")

resampling.range = quantile(resamples, c(0.025, 0.975)) #(0.3595618, 0.4213147 ), range = 0.0617529
resampling.MOE = (resampling.range[2]-resampling.range[1])/2 #MOE = 0.03087649 , approx 3%

#Task 3: Simulation over n and p
simulations = 10000
n.vec = seq(100, 3000, by = 10)
p.vec = seq(.01, .99, by = .01)
MOE.data = data.frame(n = numeric(0), p = numeric(0), MOE = numeric(0)) #data frame for MOEs
for(i in 1:length(n.vec)){
  for(j in 1:length(p.vec)){
    n.val = n.vec[i]
    p.val = p.vec[j]
    data = rbinom(simulations, n.val, p.val)/n.val #new set of data
    range = quantile(data, c(0.025,0.975)) #range for current data
    MOE = (range[2]-range[1])/2 #MOE for current data
    MOE.data = rbind(MOE.data, data.frame(n = n.val, p = p.val, MOE = MOE)) #add row with current n, p, and MOE
  }
}

#geom_raster plot of estimated MOE
MOE.plot = ggplot(MOE.data, aes(x=n, y = p, fill = MOE)) +
  geom_raster() +
  scale_fill_viridis_c(name = "Margin of Error") +
  labs(title = "Estimated MOE as a function of sample size and true probability") +
  xlab("n") +
  ylab("p")

#Task 4: Actual Margin of Error Calculation
n.vec = seq(100, 2000, by = 10)
p.vec = seq(.01, .99, by = .01)
alpha = 0.05
Z = qnorm(1- alpha/2)
Wilson.data = data.frame(n = numeric(0), p = numeric(0), Wilson.MOE = numeric(0)) #data frame for Wilson MOEs
for(i in 1:length(n.vec)){
  for(j in 1:length(p.vec)){
    n.val = n.vec[i]
    p.val = p.vec[j]
    Wilson.MOE = Z*(sqrt(n.val*p.val*(1-p.val) + ((Z^2)/4)))/(n.val + Z^2) #Wilson MOE for current data
    Wilson.data = rbind(Wilson.data, data.frame(n = n.val, p = p.val, Wilson.MOE = Wilson.MOE)) #add row with current n, p, and Wilson MOE
  }
}

Wilson.plot = ggplot(Wilson.data, aes(x=n, y = p, fill = Wilson.MOE)) +
  geom_raster() +
  scale_fill_viridis_c(name = "Wilson Margin of Error") +
  labs(title = "Wilson MOE as a function of sample size and true probability") +
  xlab("n") +
  ylab("p") +
  theme_minimal()

