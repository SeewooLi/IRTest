test_that(
  "testing basic operations for IRTest_Dich",
          {
  Alldata <- DataGeneration(seed = 123456789,
                            model_D = rep(1, 10),
                            N=500,
                            nitem_D = 10,
                            nitem_P = 0,
                            latent_dist = "2NM",
                            d = 1.664,
                            sd_ratio = 2,
                            prob = 0.3)
  data <- Alldata$data_D
  item <- Alldata$item_D
  initialitem <- Alldata$initialitem_D
  theta <- Alldata$theta

  # Normal distribution
  # EAP
  Mod1 <- IRTest_Dich(initialitem = initialitem,
                      data = data,
                      model = rep(1, 10),
                      latent_dist = "N",
                      max_iter = 2,
                      threshold = .0001)
  expect_equal(dim(Mod1$par_est), dim(item))
  expect_equal(length(Mod1$theta), length(theta))

  # MLE
  Mod1 <- IRTest_Dich(initialitem = initialitem,
                      data = data,
                      model = rep(1, 10),
                      latent_dist = "N",
                      max_iter = 2,
                      threshold = .0001,
                      ability_method = "MLE"
  )
  expect_equal(dim(Mod1$par_est), dim(item))
  expect_equal(length(Mod1$theta), length(theta))

  # 2PL
  Mod1 <- IRTest_Dich(initialitem = initialitem,
                      data = data,
                      model = rep(2, 10),
                      latent_dist = "N",
                      max_iter = 2,
                      threshold = .0001)
  expect_equal(dim(Mod1$par_est), dim(item))
  expect_equal(length(Mod1$theta), length(theta))

  # 3PL
  Mod1 <- IRTest_Dich(initialitem = initialitem,
                      data = data,
                      model = rep(3, 10),
                      latent_dist = "N",
                      max_iter = 2,
                      threshold = .0001)
  expect_equal(dim(Mod1$par_est), dim(item))
  expect_equal(length(Mod1$theta), length(theta))

  # EHM
  Mod1 <- IRTest_Dich(initialitem = initialitem,
                      data = data,
                      model = rep(1, 10),
                      latent_dist = "EHM",
                      max_iter = 2,
                      threshold = .0001)
  expect_equal(dim(Mod1$par_est), dim(item))
  expect_equal(length(Mod1$theta), length(theta))

  # Two-component normal distribution
  Mod1 <- IRTest_Dich(initialitem = initialitem,
                      data = data,
                      model = rep(1, 10),
                      latent_dist = "2NM",
                      max_iter = 2,
                      threshold = .0001)
  expect_equal(dim(Mod1$par_est), dim(item))
  expect_equal(length(Mod1$theta), length(theta))

  # KDE
  Mod1 <- IRTest_Dich(initialitem = initialitem,
                      data = data,
                      model = rep(1, 10),
                      latent_dist = "KDE",
                      max_iter = 2,
                      threshold = .0001)
  expect_equal(dim(Mod1$par_est), dim(item))
  expect_equal(length(Mod1$theta), length(theta))

  # Davidian curve
  Mod1 <- IRTest_Dich(initialitem = initialitem,
                      data = data,
                      model = rep(1, 10),
                      latent_dist = "DC",
                      max_iter = 2,
                      threshold = .0001,
                      h=2)
  expect_equal(dim(Mod1$par_est), dim(item))
  expect_equal(length(Mod1$theta), length(theta))

  # LLS
  Mod1 <- IRTest_Dich(initialitem = initialitem,
                      data = data,
                      model = rep(1, 10),
                      latent_dist = "LLS",
                      max_iter = 2,
                      threshold = .0001,
                      h=2)
  expect_equal(dim(Mod1$par_est), dim(item))
  expect_equal(length(Mod1$theta), length(theta))

  expect_warning(
    IRTest_Dich(initialitem = initialitem,
                data = data,
                model = NA,
                latent_dist = "2NM",
                max_iter = 2,
                threshold = .0001)
  )
          }
)
