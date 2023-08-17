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
  initialitemD <- Alldata$initialitem_D
  initialitemP <- Alldata$initialitem_P
  theta <- Alldata$theta

  # Normal Distribution
  Mod1 <- IRTest_Mix(initialitem_D = initialitemD,
                     initialitem_P = initialitemP,
                     data_D = DataD,
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
  Mod1 <- IRTest_Mix(initialitem_D = initialitemD,
                     initialitem_P = initialitemP,
                     data_D = DataD,
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
  Mod1 <- IRTest_Mix(initialitem_D = initialitemD,
                     initialitem_P = initialitemP,
                     data_D = DataD,
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
  Mod1 <- IRTest_Mix(initialitem_D = initialitemD,
                     initialitem_P = initialitemP,
                     data_D = DataD,
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
  Mod1 <- IRTest_Mix(initialitem_D = initialitemD,
                     initialitem_P = initialitemP,
                     data_D = DataD,
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
  Mod1 <- IRTest_Mix(initialitem_D = initialitemD,
                     initialitem_P = initialitemP,
                     data_D = DataD,
                     data_P = DataP,
<<<<<<< HEAD
                     model_D = c(1,1,1,2,3),
                     model_P = "GPCM",
=======
                     model_D = rep(1,5),
                     model_P = "PCM",
>>>>>>> 5ddb6abf59c95ae754b8eaa6b0b498619a20f638
                     latent_dist = "LLS",
                     max_iter = 2,
                     threshold = .001,
                     h=2)
  expect_equal(dim(Mod1$par_est$Dichotomous), dim(itemD))
  expect_equal(dim(Mod1$par_est$Polytomous), dim(itemP))
  expect_equal(length(Mod1$theta), length(theta))
<<<<<<< HEAD
  plot_item(Mod1, 4, type="d")
  plot_item(Mod1, 5, type="d")
=======
>>>>>>> 5ddb6abf59c95ae754b8eaa6b0b498619a20f638

  # GPCM
  Mod1 <- IRTest_Mix(initialitem_D = initialitemD,
                     initialitem_P = initialitemP,
                     data_D = DataD,
                     data_P = DataP,
                     model_D = 1,
<<<<<<< HEAD
                     model_P = "PCM",
=======
                     model_P = "GPCM",
>>>>>>> 5ddb6abf59c95ae754b8eaa6b0b498619a20f638
                     latent_dist = "N",
                     max_iter = 2,
                     threshold = .001,
                     h=2)
  expect_equal(dim(Mod1$par_est$Dichotomous), dim(itemD))
  expect_equal(dim(Mod1$par_est$Polytomous), dim(itemP))
  expect_equal(length(Mod1$theta), length(theta))

  expect_error(
    IRTest_Mix(initialitem_D = initialitemD,
               initialitem_P = initialitemP,
               data_D = DataD[1:900,],
               data_P = DataP,
               model_D = rep(1,5),
               model_P = "PCM",
               latent_dist = "DC",
               max_iter = 2,
               threshold = .001,
               h=2)
  )
  }
)
