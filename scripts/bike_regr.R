# Analysis of bike share data
#install.packages("remotes") # uncomment if not installed
library(remotes)
install_github("mattiasvillani/progdataanalys")
library(progdataanalys)
lmfit = lm(nRides ~ temp + hum + windspeed, data = bike)
lmsummary(lmfit, anova = T, conf_intervals = T, vif_factors = T)
?lmsummary
?bike
