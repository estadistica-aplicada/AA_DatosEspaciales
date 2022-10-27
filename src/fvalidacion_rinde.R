validacion_rinde <- function (fold) {
  require(sp)
  require(caret)
  require(gstat)
  require(sf)
  
  source("src/near.obs.R")
  
  datos <- st_read("data/Rinde_soja.gpkg")[-c(1:3)]
  
  set.seed(17)
  datos$id <-
    sample(rep(1:10, nrow(datos), length.out = nrow(datos)))
  
  list <- 1:10
  prediccion <- data.frame()
  testset <- data.frame()
  
  training <- subset(datos, id %in% list[-fold])
  testing <- subset(datos, id %in% c(fold))
  
  # Kriging Ordinario
  vario <- variogram(REND ~ 1, training, cutoff = 300)
  vario_teorico_Kg <-
    fit.variogram(vario, vgm(c("Gau", "Exp", "Sph")))
  
  KO <-
    krige(REND ~ 1,
          training,
          testing,
          model = vario_teorico_Kg,
          nmax = 25)
  
  # Random Forest Spatial Interpolation
  nn_training <- near.obs(
    locations = as_Spatial(training),
    observations = as_Spatial(training),
    zcol = "REND",
    n.obs = 4
  )
  
  training_nn <- cbind(training[, -3], nn_training)
  
  nn_testing <- near.obs(
    locations = as_Spatial(testing),
    observations = as_Spatial(training),
    zcol = "REND",
    n.obs = 4
  )
  
  testing_nn <- cbind(testing[, -3], nn_testing)
  
  fitControl <- trainControl(method = "none")
  set.seed(7)
  train_rfsi <- train(
    REND ~ .,
    data = st_drop_geometry(training_nn) ,
    method = "rf",
    trControl = fitControl,
    verbose = FALSE
  )
  
  test_rfsi <- predict(train_rfsi, newdata = testing_nn)
  
  # Tabla observados y predichos
  testset <- rbind(testset, as.data.frame(testing[, "REND"]))
  result <- data.frame(
    data.frame(
      "k-fold" = fold,
      "Observado" = testset[, 1],
      "KO" = KO$var1.pred,
      "RFSI" = test_rfsi
    )
  )
  
  result <- tidyr::pivot_longer(
    data = result,
    cols = c('KO', 'RFSI'),
    values_to = 'Predicho',
    names_to = 'Metodo'
  )
  
  return(result)
  
}