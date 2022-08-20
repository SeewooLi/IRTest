theta <- 0
a <- 1:3
b <- matrix(c(-.5,0,.5,NA,-1,-.8,-.5,-.2,-.2,0,NA,NA), nrow=3, byrow = T)
ps <- cbind(1,exp(t(apply(a*(theta-b),1,cumsum))))
ps <- ps/rowSums(ps, na.rm = T)
ps

category <- c(rep(3,10),rep(5,10),rep(7,10))
Alldata <- DataGeneration(seed = 1,
                          model = "GPCM",
                          N=2000,
                          nitem = 30,
                          categ = category,
                          d = 1.664,
                          sd_ratio = 2,
                          prob = 0.3)

data <- Alldata$data
item <- Alldata$item
initialitem <- Alldata$initialitem
theta <- Alldata$theta

E <- Estep_Poly(item, data)
M <- Mstep_Poly(E, item, data, model = "GPCM")

M1 <- IRTest_Poly(initialitem = initialitem,
            data = data,
            model = "GPCM",
            latent_dist = "DC",
            bandwidth = "SJ-ste",
            max_iter = 200,
            threshold = .001,
            h=4)

plot(M1$par_est[,1], item[,1])
abline(a=0,b=1)
plot(M1$par_est[,2], item[,2])
abline(a=0,b=1)
plot(M1$par_est[,3], item[,3])
abline(a=0,b=1)
plot(M1$par_est[,4], item[,4])
abline(a=0,b=1)
plot(M1$par_est[,5], item[,5])
abline(a=0,b=1)
plot(M1$par_est[,6], item[,6])
abline(a=0,b=1)
plot(M1$par_est[,7], item[,7])
abline(a=0,b=1)
plot(M1$quad, M1$fk)

plot_LD(M1)


