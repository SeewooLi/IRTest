test_that("testing basic operations for IRTest_Dich", {
  Alldata <- DataGeneration(seed = 123456789,
                            model_D = rep(1, 5),
                            N=500,
                            nitem_D = 5,
                            nitem_P = 0,
                            d = 1.664,
                            sd_ratio = 2,
                            prob = 0.3)
  data <- Alldata$data_D
  item <- Alldata$item_D
  initialitem <- Alldata$initialitem_D
  theta <- Alldata$theta

  Mod1 <- IRTest_Dich(initialitem = initialitem,
                      data = data,
                      model = rep(1, 5),
                      latent_dist = "EHM",
                      max_iter = 2,
                      threshold = .0001)
  expect_equal(dim(Mod1$par_est), dim(item))
  expect_equal(dim(Mod1$theta), dim(theta))
})
