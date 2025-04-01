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


