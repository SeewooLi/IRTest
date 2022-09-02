test_that("testing basic operations for IRTest_Mix", {
  Alldata <- DataGeneration(seed = 1,
                            model_D = rep(1,5),
                            model_P = "GPCM",
                            categ = rep(3,5),
                            N=1000,
                            nitem_D = 5,
                            nitem_P = 5,
                            d = 1.664,
                            sd_ratio = 1,
                            prob = 0.5)

  DataD <- Alldata$data_D
  DataP <- Alldata$data_P
  itemD <- Alldata$item_D
  itemP <- Alldata$item_P
  initialitemD <- Alldata$initialitem_D
  initialitemP <- Alldata$initialitem_P
  theta <- Alldata$theta

  Mod1 <- IRTest_Mix(initialitem_D = initialitemD,
                     initialitem_P = initialitemP,
                     data_D = DataD,
                     data_P = DataP,
                     model_D = rep(1,5),
                     model_P = "GPCM",
                     latent_dist = "KDE",
                     bandwidth = "SJ-ste",
                     max_iter = 2,
                     threshold = .001)
  expect_equal(dim(Mod1$par_est$Dichotomous), dim(itemD))
  expect_equal(dim(Mod1$par_est$Polytomous), dim(itemP))
  expect_equal(dim(Mod1$theta), dim(theta))
})
