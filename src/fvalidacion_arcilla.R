validacion_arcilla <- function (fold) {
  require(sf)
  require(caret)
  require(gstat)
  require(sp)
  
  source("src/near.obs.R")
  
  datos <- st_read("data/Arcilla_Muestra.gpkg")

  set.seed(17)
  datos$id <-
    sample(rep(1:10, nrow(datos), length.out = nrow(datos)))
  
  list <- 1:10
  prediccion <- data.frame()
  testset <- data.frame()
  
  training <- subset(datos, id %in% list[-fold])
  testing <- subset(datos, id %in% c(fold))
  
  # Kriging Ordinario
  vario <- variogram(Arcilla ~ 1, training)
  vario_teorico_Kg <-
    fit.variogram(vario, vgm(c("Gau")))
  plot(vario, vario_teorico_Kg)
  
  KO <-
    krige(Arcilla ~ 1,
          training,
          testing,
          model = vario_teorico_Kg,
          nmax = 25)
  
  # Random Forest
  fitControl <- trainControl(method = "cv", number = 10)
  #fitControl <- trainControl(method = "none")
  training$id <- NULL
  set.seed(7)
  rf <- train(
    Arcilla ~ .,
    data = st_drop_geometry(training),
    method = "rf",
    trControl = fitControl,
    verbose = FALSE
  )
  
  test_rf <- predict(rf, newdata = testing)
  
  # Random Forest + Kriging Ordinario
  training$residuos_rf <-
    training$Arcilla - predict(rf, newdata = training)
  vario_rf <- variogram(residuos_rf ~ 1, training)
  model_rf_ko <-
    fit.variogram(vario_rf, vgm(c("Sph", "Exp", "Gau")))
  test_ko <- krige(residuos_rf ~ 1 ,
                   training,
                   testing,
                   model_rf_ko,
                   nmax = 25)
  test_rf_ko <- test_rf + test_ko$var1.pred
  
  # Random Forest Spatial Interpolation
  nn_training <- near.obs(
    locations = as_Spatial(training),
    observations = as_Spatial(training),
    zcol = "Arcilla",
    n.obs = 4,
    idw = T
  )
  training_nn <- cbind(training[, -7], nn_training)
  
  
  nn_testing <- near.obs(
    locations = as_Spatial(testing),
    observations = as_Spatial(training),
    zcol = "Arcilla",
    n.obs = 4,
    idw = T
  )
  testing_nn <- cbind(testing[, -7], nn_testing)
  
  set.seed(7)
  train_rfsi <- train(
    Arcilla ~ .,
    data = st_drop_geometry(training_nn) ,
    method = "rf",
    trControl = fitControl,
    verbose = FALSE,
    importance = T
    
  )
  
  test_rfsi <- predict(train_rfsi, newdata = testing_nn)
  
  
  # Tabla observados y predichos
  testset <- rbind(testset, as.data.frame(testing[, "Arcilla"]))
  result <- data.frame(
    data.frame(
      "k-fold" = fold,
      "Observado" = testset[, 1],
      "KO" = KO$var1.pred,
      "RF" = test_rf,
      "RF_KO" = test_rf_ko,
      "RFSI" = test_rfsi
    )
  )
  
  result <- tidyr::pivot_longer(
    data = result,
    cols = c('KO', 'RF', 'RF_KO', 'RFSI'),
    values_to = 'Predicho',
    names_to = 'Metodo'
  )
  
  return(result)
  
}