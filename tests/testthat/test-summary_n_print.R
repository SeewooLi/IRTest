test_that(
  "testing basic operations for summary.irtest, print.irtest, and print.irtest_summary", {
  Alldata <- DataGeneration(seed = 123456789,
                            model_D = rep(1:3, 3),
                            N=500,
                            nitem_D = 9,
                            nitem_P = 0,
                            latent_dist = "2NM",
                            d = 1.664,
                            sd_ratio = 2,
                            prob = 0.3)
  data <- Alldata$data_D
  item <- Alldata$item_D
  theta <- Alldata$theta

  Mod1 <- IRTest_Dich(data = data,
                      model = rep(1:3, 3),
                      latent_dist = "EHM",
                      max_iter = 1,
                      threshold = .5)
  print(Mod1)
  print(summary(Mod1))
  plot_item(Mod1,1)
  plot_item(Mod1,2)
  plot_item(Mod1,3)

  Mod1 <- IRTest_Dich(data = data,
                      model = rep(1, 10),
                      latent_dist = "2NM",
                      max_iter = 2,
                      threshold = .9)
  print(Mod1)
  print(summary(Mod1))

  Mod1 <- IRTest_Dich(data = data,
                      model = rep(1, 10),
                      latent_dist = "N",
                      max_iter = 3,
                      threshold = .9)
  print(Mod1)
  print(summary(Mod1))

  Mod1 <- IRTest_Dich(data = data,
                      model = rep(1, 10),
                      latent_dist = "KDE",
                      max_iter = 4,
                      threshold = .9)
  print(Mod1)
  print(summary(Mod1))

  Mod1 <- IRTest_Dich(data = data,
                      model = rep(1, 10),
                      latent_dist = "DC",
                      max_iter = 2,
                      threshold = .0001,
                      h=3)
  print(Mod1)
  print(summary(Mod1))
  item_fit(Mod1)
  item_fit(Mod1, bin.center="median")
  plot(Mod1)
  reliability(Mod1)


  # Polytomous
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


  Mod1 <- IRTest_Poly(data = data,
                      model = "GPCM",
                      latent_dist = "N",
                      max_iter = 2,
                      threshold = .001)
  print(Mod1)
  print(summary(Mod1))
  item_fit(Mod1)
  item_fit(Mod1, bin.center="median")
  plot(Mod1)
  reliability(Mod1)
  plot_item(Mod1,1)

  Mod1 <- IRTest_Poly(data = data,
                      model = "PCM",
                      latent_dist = "LLS",
                      h=2,
                      max_iter = 2,
                      threshold = .001)
  print(Mod1)
  print(summary(Mod1))
  plot_item(Mod1,1)

  # mix
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


  Mod1 <- IRTest_Mix(data_D = DataD,
                     data_P = DataP,
                     model_D = rep(1,5),
                     model_P = "PCM",
                     latent_dist = "N",
                     max_iter = 2,
                     threshold = .001)
  print(Mod1)
  print(summary(Mod1))
  item_fit(Mod1)
  item_fit(Mod1, bin.center="median")
  plot(Mod1)
  reliability(Mod1)
  plot_item(Mod1,1,type="d")
  plot_item(Mod1,1,type="p")

  Mod1 <- IRTest_Mix(data_D = DataD,
                     data_P = DataP,
                     model_D = rep(1,5),
                     model_P = "GPCM",
                     latent_dist = "N",
                     max_iter = 2,
                     threshold = .001)
  print(Mod1)
  print(summary(Mod1))
  item_fit(Mod1)
  item_fit(Mod1, bin.center="median")
  plot(Mod1)
  reliability(Mod1)
  plot_item(Mod1,1,type="d")
  plot_item(Mod1,1,type="p")
}
)
