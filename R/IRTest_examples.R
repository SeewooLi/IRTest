AllData <- DataGeneration(
  seed = 1,
  N = 4000,
  nitem = 20,
  latent_dist = "normal",   # 1. Generate theta values 
  prob = 0.333,             #    from the 2 component Gaussian mixture
  d = 1.664,                #    distribution 
  sd_ratio = 2,             #    with its parameters pi, delta, zeta (Li, 2021).
  model = 3,                # 2. Generate data from 3PL model
  a_l = 0.8,                # 3. lower limit of "a" parameter
  a_u = 2.5,                # 4. upper limit of "a" parameter
  c_l = 0,                  # 5. lower limit of "c" parameter
  c_u = 0.2)                # 6. upper limit of "c" parameter

item <- AllData$item
initialitem <- AllData$initialitem
theta <- AllData$theta
data <- AllData$data

M_N <- IRTest(initialitem = initialitem, # normality assumption
              data = data,
              range = c(-6,6),
              q = 121,
              model = c(rep(1,5),        # item 1-5 are 1PL,
                        rep(2,10),       # item 6-15 are 2PL,
                        rep(3,5)),       # and item 16-20 are 3PL.
              latent_dist = "Normal",
              max_iter = 200,
              threshold = 0.0001)

M_EHM <- IRTest(initialitem = initialitem, # Empirical Histogram Method
                data = data,               # An error may occur using EHM 
                range = c(-6,6),           # when "seed=1" is used.
                q = 121,
                model = c(rep(1,5),
                          rep(2,10),
                          rep(3,5)),
                latent_dist = "EHM",
                max_iter = 200,          
                threshold = 0.0001)

M_2NM <- IRTest(initialitem = initialitem,
                data = data,
                range = c(-6,6),
                q = 121,
                model = c(rep(1,5),
                          rep(2,10),
                          rep(3,5)),
                latent_dist = "Mixture",
                max_iter = 200,
                threshold = 0.0001)

M_KDE <- IRTest(initialitem = initialitem,
                data = data,
                range = c(-6,6),
                q = 121,
                model = c(rep(1,5),
                          rep(2,10),
                          rep(3,5)),
                latent_dist = "KDE",  # Sheather & Jones's solve-the-equation
                bandwidth = "SJ-ste", # method is used for bandwidth estimation.
                max_iter = 200,
                threshold = 0.0001)

M_DC5 <- IRTest(initialitem = initialitem,
               data = data,
               range = c(-6,6),
               q = 121,
               model = c(rep(1,5),
                         rep(2,10),
                         rep(3,5)),
               latent_dist = "DC",
               h = 5,                 # the number of hyper-parameters in DC
               max_iter = 200,
               threshold = 0.0001)

# True latent density
plot(seq(-6,6, by=0.1),
     dist2(seq(-6,6, by=0.1), 
           prob = 0.333,
           d = 1.664,
           sd_ratio = 2),
     type = "l")

# Estimated density using true theta values
plot(density(theta, bw = "SJ-ste", from = -6, to = 6, n=121))

# Estimated density with IRTest
plot_LD(M_2NM)

# structure of the estimated parameters
M_KDE$par_est

# structure of the asymptotic standard errors
M_KDE$se

# "a" parameter
plot(item[6:20,1], M_N$par_est[6:20,1])
plot(item[6:20,1], M_DC5$par_est[6:20,1])
plot(item[6:20,1], M_2NM$par_est[6:20,1])
plot(item[6:20,1], M_KDE$par_est[6:20,1])

# "b" parameter
plot(item[,2], M_N$par_est[,2])
plot(item[,2], M_DC5$par_est[,2])
plot(item[,2], M_2NM$par_est[,2])
plot(item[,2], M_KDE$par_est[,2])

# "c" parameter
plot(item[16:20,3], M_DC5$par_est[16:20,3])
plot(item[16:20,3], M_KDE$par_est[16:20,3])
plot(item[16:20,3], M_2NM$par_est[16:20,3])










