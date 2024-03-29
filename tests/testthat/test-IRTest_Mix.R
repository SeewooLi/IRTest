test_that("testing basic operations for IRTest_Mix", {
  Alldata <- DataGeneration(seed = 1,
                            model_D = rep(1,5),
                            model_P = "GPCM",
                            categ = rep(3,5),
                            N=1000,
                            nitem_D = 5,
                            nitem_P = 5,
                            latent_dist = "2NM",
                            d = 1.664,
                            sd_ratio = 1,
                            prob = 0.5)

  DataD <- Alldata$data_D
  DataP <- Alldata$data_P
  itemD <- Alldata$item_D
  itemP <- Alldata$item_P
  theta <- Alldata$theta

  # Normal Distribution
  Mod1 <- IRTest_Mix(data_D = DataD,
                     data_P = DataP,
                     model_D = rep(1,5),
                     model_P = "PCM",
                     latent_dist = "N",
                     max_iter = 2,
                     threshold = .001)
  expect_equal(dim(Mod1$par_est$Dichotomous), dim(itemD))
  expect_equal(dim(Mod1$par_est$Polytomous), dim(itemP))
  expect_equal(length(Mod1$theta), length(theta))

  # EHM
  Mod1 <- IRTest_Mix(data_D = DataD,
                     data_P = DataP,
                     model_D = rep(1,5),
                     model_P = "PCM",
                     latent_dist = "EHM",
                     max_iter = 2,
                     threshold = .001)
  expect_equal(dim(Mod1$par_est$Dichotomous), dim(itemD))
  expect_equal(dim(Mod1$par_est$Polytomous), dim(itemP))
  expect_equal(length(Mod1$theta), length(theta))

  # Two-component normal distribution
  Mod1 <- IRTest_Mix(data_D = DataD,
                     data_P = DataP,
                     model_D = rep(1,5),
                     model_P = "PCM",
                     latent_dist = "2NM",
                     max_iter = 2,
                     threshold = .001)
  expect_equal(dim(Mod1$par_est$Dichotomous), dim(itemD))
  expect_equal(dim(Mod1$par_est$Polytomous), dim(itemP))
  expect_equal(length(Mod1$theta), length(theta))

  # KDE
  Mod1 <- IRTest_Mix(data_D = DataD,
                     data_P = DataP,
                     model_D = rep(1,5),
                     model_P = "PCM",
                     latent_dist = "KDE",
                     bandwidth = "SJ-ste",
                     max_iter = 2,
                     threshold = .001)
  expect_equal(dim(Mod1$par_est$Dichotomous), dim(itemD))
  expect_equal(dim(Mod1$par_est$Polytomous), dim(itemP))
  expect_equal(length(Mod1$theta), length(theta))

  # Davidian curve
  Mod1 <- IRTest_Mix(data_D = DataD,
                     data_P = DataP,
                     model_D = rep(1,5),
                     model_P = "PCM",
                     latent_dist = "DC",
                     max_iter = 2,
                     threshold = .001,
                     h=2)
  expect_equal(dim(Mod1$par_est$Dichotomous), dim(itemD))
  expect_equal(dim(Mod1$par_est$Polytomous), dim(itemP))
  expect_equal(length(Mod1$theta), length(theta))

  # LLS
  Mod1 <- IRTest_Mix(data_D = DataD,
                     data_P = DataP,
                     model_D = rep(1,5),
                     model_P = "PCM",
                     latent_dist = "LLS",
                     max_iter = 2,
                     threshold = .001,
                     h=2)
  expect_equal(dim(Mod1$par_est$Dichotomous), dim(itemD))
  expect_equal(dim(Mod1$par_est$Polytomous), dim(itemP))
  expect_equal(length(Mod1$theta), length(theta))

  # GPCM
  Mod1 <- IRTest_Mix(data_D = DataD,
                     data_P = DataP,
                     model_D = c(2,3,1,1,1),
                     model_P = "GPCM",
                     latent_dist = "N",
                     max_iter = 2,
                     threshold = .001,
                     h=2)
  expect_equal(dim(Mod1$par_est$Dichotomous), dim(itemD))
  expect_equal(dim(Mod1$par_est$Polytomous), dim(itemP))
  expect_equal(length(Mod1$theta), length(theta))
  plot_item(Mod1,1,type="d")
  plot_item(Mod1,2,type="d")

  expect_error(
    IRTest_Mix(data_D = DataD[1:900,],
               data_P = DataP,
               model_D = rep(1,5),
               model_P = "PCM",
               latent_dist = "DC",
               max_iter = 2,
               threshold = .001,
               h=2)
  )
  Mod1 <- IRTest_Mix(data_D = DataD,
                     data_P = DataP,
                     model_D = c(2,3,1,1,1),
                     model_P = "GRM",
                     latent_dist = "N",
                     max_iter = 2,
                     threshold = .001,
                     ability_method = "MLE")
}
)
