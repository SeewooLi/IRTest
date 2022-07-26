source("Functions_KDE_DC.R")
source("Davidian curve.R")

# ICC Example
#################################################################################################################
P

P(theta = 0, b = 0)
P(0, b=0)
P(0, b=1)
P(1, b=0)

P(0, a = 2, b=1)
P(0, a = 2, b=1, c=0.2)

# Data Generation  Example
#################################################################################################################
DataGeneration
DataGeneration()

DataGeneration(seed = 10, N=200, nitem = 15, prob = 0.5, a_l = 0.6, a_u = 2)
DataGeneration(N=100, prob = 0.67, latent_dist = "normal")
DataGeneration(N=100, latent_dist = "beta")
DataGeneration(N=100, latent_dist = "chi")


Data <- DataGeneration(N=200, nitem = 10)
Data
item <- Data$item
initialitem <- Data$initialitem
data <- Data$data
theta <- Data$theta

# log-Likelihood 
#################################################################################################################
logLikeli
logLikeli(item = item,data = data, theta = theta)

logLikeli(item, data, theta)
logLikeli(item, data, theta)[200,10]
logLikeli(item, data, theta)[200,]
logLikeli(item, data, theta)[,10]

# Distribution
#################################################################################################################
dnormal

dnormal(0)
dnormal(1)
dnormal(-1)

dnormal(1, mean = 1)
dnormal(1, mean = 1, sd = 2)

# 2C-Normal Mixture Distribution
dist

dist(0)
dist(0, s = c(1,2))
dist(0, prob = 0.33, s = c(1,2))
dist(0, prob = 0.67, s = c(1,2))

dist(1, m = c(1,1))
dist(0, m = c(-1,1))
dist(0, m = c(-2,2))

dist(0, prob = 0.2, m = c(-1,1))
dist(0, prob = 0.8, m = c(-1,1))
dist(0, prob = 0.2, m = c(-1,1), s = c(1,2))
dist(0, prob = 0.8, m = c(-1,1), s = c(1,2))

# re-parameterized 2C-Normal Mixture Distribution (Li, 2021)
dist2

dist2(0)
dist2(0, overallmean = 0, overallsd = 1)
dist2(0, overallmean = 1)
dist2(0, overallmean = 1, overallsd = 2)

dist2(0, sd_ratio = 2)
dist2(0, prob = 0.67, sd_ratio =2)
dist2(0, prob = 0.33, sd_ratio =2)

# recovering original parameters of 2C-Normal Mixture Distribution from re-parameterized parameters
distribution_par 
distribution_par()

distribution_par(overallmean = 1)
distribution_par(overallmean = 1, overallsd =2)

distribution_par(sd_ratio = 2)
distribution_par(prob = 0.67, sd_ratio =2)
distribution_par(prob = 0.33, sd_ratio =2)

# E step
#################################################################################################################
Estep
Estep(item, data)
Estep(item, data, c(-6,6), q =200)

E <- Estep(item, data)

# M1 step
#################################################################################################################
M1step
M1step(E, item)
M1step(E, item, max_iter = 20, threshold=1e-5)
M1step(E, item, max_iter = 200, threshold=1e-9)

# M2 step
#################################################################################################################
M2step
M2step(E)

# Linear interpolation / extrapolation
#################################################################################################################
lin_inex

# Estimation
#################################################################################################################
Bimodal_MMLE

Bimodal_MMLE(initialitem, data, range = c(-4,4), q = 8, max_iter=100, threshold=0.001)

M1<- Bimodal_MMLE(initialitem, data,latent_dist = "Normal")
M2<- Bimodal_MMLE(initialitem, data,latent_dist = "EHM")
M3<- Bimodal_MMLE(initialitem, data,latent_dist = "Mixture")
M4<- Bimodal_MMLE(initialitem, data,latent_dist = "KDE")

M5<- Bimodal_MMLE(initialitem, data,latent_dist = "DC", h = 10)

# Plotting LD
#################################################################################################################
plot_LD

plot_LD(M1, -4, 4)

plot_LD(M1, add = T)
plot_LD(M2, add = T)

