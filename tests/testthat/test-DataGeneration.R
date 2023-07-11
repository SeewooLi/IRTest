test_that("testing basic operations for DataGeneration", {
  Alldata <- DataGeneration(seed = 1,
                            model_D = 1:3,
                            model_P = "GPCM",
                            categ = rep(c(3,7), each = 5),
                            N=500,
                            nitem_D = 3,
                            nitem_P = 10,
                            latent_dist = "2NM",
                            d = 1.414,
                            sd_ratio = 2,
                            prob = 0.5)


  Alldata2 <- DataGeneration(seed = 1,
                             model_D = 1:3,
                             model_P = 'GPCM',
                             categ = rep(c(3,7), each = 5),
                             N=500,
                             nitem_D = 3,
                             nitem_P = 10,
                             item_D = Alldata$item_D,
                             item_P = Alldata$item_P,
                             latent_dist = "2NM",
                             d = 1.414,
                             sd_ratio = 2,
                             prob = 0.5
  )

  expect_equal(Alldata$item_D, Alldata$item_D)
  expect_equal(Alldata$item_P, Alldata$item_P)
  expect_equal(Alldata$theta, Alldata$theta)

  expect_no_error(
    DataGeneration(seed = 1,
                   model_D = 1:3,
                   model_P = "GPCM",
                   categ = rep(c(3,7), each = 5),
                   N=500,
                   nitem_D = 3,
                   nitem_P = 10,
                   latent_dist = 'normal')
  )

  expect_no_error(
    DataGeneration(seed = 1,
                   model_D = 1:3,
                   model_P = "GPCM",
                   categ = rep(c(3,7), each = 5),
                   N=500,
                   nitem_D = 3,
                   nitem_P = 10,
                   latent_dist = 'beta')
  )

  expect_no_error(
    DataGeneration(seed = 1,
                   model_D = 1:3,
                   model_P = "GPCM",
                   categ = rep(c(3,7), each = 5),
                   N=500,
                   nitem_D = 3,
                   nitem_P = 10,
                   latent_dist = 'chi')
  )

  expect_error(
    DataGeneration(seed = 1,
                   model_D = 1:3,
                   model_P = "GPCM",
                   categ = rep(c(3,7), each = 5),
                   N=500,
                   nitem_D = 3,
                   nitem_P = 10,
                   latent_dist = NULL)
  )

  expect_error(
    DataGeneration(seed = 1,
                   model_D = 1:3,
                   model_P = "GPCM",
                   categ = rep(c(3,7), each = 5),
                   N=500,
                   nitem_D = 3,
                   nitem_P = 10,
                   latent_dist = "gamma")
  )
}
)
