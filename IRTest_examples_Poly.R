theta <- 0
a <- 1:3
b <- matrix(c(-.5,0,.5,NA,-1,-.8,-.5,-.2,-.2,0,NA,NA), nrow=3, byrow = T)
ps <- cbind(1,exp(t(apply(a*(theta-b),1,cumsum))))
ps <- ps/rowSums(ps, na.rm = T)
ps

category <- rep(3:4,each = 5)#category <- c(rep(3,10),rep(5,10),rep(7,10))
Alldata <- DataGeneration(seed = 1,
                          model_D = rep(1:3, each=5),
                          N=2000,
                          nitem_D = 15,
                          nitem_P = 10,
                          categ = category,
                          d = 1.664,
                          sd_ratio = 2,
                          prob = 0.3)

data <- Alldata$data
item <- Alldata$item
initialitem <- Alldata$initialitem
theta <- Alldata$theta

#E <- Estep_Poly(item, data)
#M <- Mstep_Poly(E, item, data, model = "GPCM")

M1 <- IRTest_Poly(initialitem = initialitem,
            data = data,
            model = "GPCM",
            latent_dist = "EHM",
            bandwidth = "SJ-ste",
            max_iter = 200,
            threshold = .001,
            h=4)
M1 <- IRTest_Dich(initialitem = initialitem,
                  data = data,
                  model = rep(3,40),
                  latent_dist = "KDE",
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


# Mixed format


category <- rep(3:7,each = 5)
Alldata <- DataGeneration(seed = 1,
                          model_D = rep(1:3, each=10),
                          N=5000,
                          nitem_D = 30,
                          nitem_P = 25,
                          categ = category,
                          d = 1.664,
                          sd_ratio = 2,
                          prob = 0.3)


DataD <- Alldata$data_D
DataP <- Alldata$data_P
itemD <- Alldata$item_D
itemP <- Alldata$item_P
initialitemD <- Alldata$initialitem_D
initialitemP <- Alldata$initialitem_P
theta <- Alldata$theta
M1 <- IRTest_Mix(initialitem_D = initialitemD,
                 initialitem_P = initialitemP,
                 data_D = DataD,
                 data_P = DataP,
                 model_D = rep(1:3, each=10),
                 latent_dist = "Normal",
                 bandwidth = "SJ-ste",
                 max_iter = 200,
                 threshold = .001,
                 h=4)

plot(itemD[,1],M1$par_est[[1]][,1])
abline(a=0,b=1)
plot(itemD[,2],M1$par_est[[1]][,2])
abline(a=0,b=1)
plot(itemD[,3],M1$par_est[[1]][,3])
abline(a=0,b=1)

plot(itemP[,1],M1$par_est[[2]][,1])
abline(a=0,b=1)
plot(itemP[,2],M1$par_est[[2]][,2])
abline(a=0,b=1)
plot(itemP[,3],M1$par_est[[2]][,3])
abline(a=0,b=1)
plot(itemP[,4],M1$par_est[[2]][,4])
abline(a=0,b=1)
plot(itemP[,5],M1$par_est[[2]][,5])
abline(a=0,b=1)
plot(itemP[,6],M1$par_est[[2]][,6])
abline(a=0,b=1)
plot(itemP[,7],M1$par_est[[2]][,7])
abline(a=0,b=1)






