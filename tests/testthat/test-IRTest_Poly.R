test_that("testing basic operations for IRTest_Poly", {
  Alldata <- DataGeneration(seed = 1,
                            model_P = "GPCM",
                            categ = rep(c(3,7), each = 5),
                            N=500,
                            nitem_D = 0,
                            nitem_P = 10,
                            latent_dist = "2NM",
                            d = 1.414,
                            sd_ratio = 2,
                            prob = 0.5)

  data <- Alldata$data_P
  item <- Alldata$item_P
  initialitem <- Alldata$initialitem_P
  theta <- Alldata$theta

  # Normal distribution
  Mod1 <- IRTest_Poly(initialitem = initialitem,
                      data = data,
                      model = "PCM",
                      latent_dist = "N",
                      max_iter = 2,
                      threshold = .001)
  expect_equal(dim(Mod1$par_est), dim(item))
  expect_equal(length(Mod1$theta), length(theta))

  # EHM
  Mod1 <- IRTest_Poly(initialitem = initialitem,
                      data = data,
                      model = "PCM",
                      latent_dist = "EHM",
                      max_iter = 2,
                      threshold = .001)
  expect_equal(dim(Mod1$par_est), dim(item))
  expect_equal(length(Mod1$theta), length(theta))

  # Two-component normal distribution
  Mod1 <- IRTest_Poly(initialitem = initialitem,
                      data = data,
                      model = "PCM",
                      latent_dist = "2NM",
                      max_iter = 2,
                      threshold = .001)
  expect_equal(dim(Mod1$par_est), dim(item))
  expect_equal(length(Mod1$theta), length(theta))

  # KDE
  Mod1 <- IRTest_Poly(initialitem = initialitem,
                      data = data,
                      model = "PCM",
                      latent_dist = "KDE",
                      max_iter = 2,
                      threshold = .001)
  expect_equal(dim(Mod1$par_est), dim(item))
  expect_equal(length(Mod1$theta), length(theta))

  # DC
  Mod1 <- IRTest_Poly(initialitem = initialitem,
                      data = data,
                      model = "PCM",
                      latent_dist = "DC",
                      max_iter = 2,
                      threshold = .001,
                      h=10)
  expect_equal(dim(Mod1$par_est), dim(item))
  expect_equal(length(Mod1$theta), length(theta))

  # LLS
  Mod1 <- IRTest_Poly(initialitem = initialitem,
                      data = data,
                      model = "PCM",
                      latent_dist = "LLS",
                      max_iter = 2,
                      threshold = .001,
                      h=5)
  expect_equal(dim(Mod1$par_est), dim(item))
  expect_equal(length(Mod1$theta), length(theta))

  expect_error(
    IRTest_Poly(initialitem = initialitem,
                data = data,
                model = NA,
                latent_dist = "DC",
                max_iter = 2,
                threshold = .001,
                h=10)
  )

  # PCM
  Alldata <- DataGeneration(seed = 1,
                            model_P = "PCM",
                            categ = rep(c(3,7), each = 5),
                            N=500,
                            nitem_D = 0,
                            nitem_P = 10,
                            latent_dist = "2NM",
                            d = 1.414,
                            sd_ratio = 2,
                            prob = 0.5)

  data <- Alldata$data_P
  item <- Alldata$item_P
  initialitem <- Alldata$initialitem_P
  theta <- Alldata$theta

  Mod1 <- IRTest_Poly(initialitem = initialitem,
                      data = data,
                      model = "PCM",
                      latent_dist = "N",
                      max_iter = 2,
                      threshold = .001)
  # MLE
  Mod1 <- IRTest_Poly(initialitem = initialitem,
                      data = data,
                      model = "PCM",
                      ability_method = "MLE",
                      latent_dist = "N",
                      max_iter = 2,
                      threshold = .001)

})
