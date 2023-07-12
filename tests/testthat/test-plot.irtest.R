test_that("testing basic operations for plot_LD", {
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

  Mod1 <- IRTest_Dich(initialitem = initialitem,
                      data = data,
                      model = rep(1, 10),
                      latent_dist = "EHM",
                      max_iter = 2,
                      threshold = .0001)
  expect_visible(plot(Mod1))

  Mod1 <- IRTest_Dich(initialitem = initialitem,
                      data = data,
                      model = rep(1, 10),
                      latent_dist = "2NM",
                      max_iter = 2,
                      threshold = .0001)
  expect_visible(plot(Mod1))

  Mod1 <- IRTest_Dich(initialitem = initialitem,
                      data = data,
                      model = rep(1, 10),
                      latent_dist = "N",
                      max_iter = 2,
                      threshold = .0001)
  expect_visible(plot(Mod1))
}
)
