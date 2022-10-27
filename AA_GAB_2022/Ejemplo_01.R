## ---------------------------------------
library(gstat)
library(spdep)
library(mapview)
library(tmap)
library(leaflet)
library(stars)
library(ggplot2)
library(caret)
library(randomForest)
library(nabor)
library(parallel)
library(openair)

source("src/near.obs.R")
source("src/fvalidacion_arcilla.R")

tmap_options(basemaps = c(
  'Satelital' = leaflet::providers$Esri.WorldImagery,
  'OSM' = leaflet::providers$OpenStreetMap))


## ---------------------------------------
datos <- st_read("data/Arcilla_Muestra.gpkg")
head(datos)

grilla <- st_read("data/Arcilla_Grilla.gpkg")
head(grilla)



## ---------------------------------------
tmap_mode('view')



## ---------------------------------------
tm_shape(datos) +
  tm_dots(col = 'Arcilla')


## ---------------------------------------
tm_shape(grilla) +
  tm_dots(col = 'Pe')


## ---------------------------------------
semiva_exp <- variogram(Arcilla ~ 1 , datos)

semiva_teorico <-
  fit.variogram(semiva_exp , vgm(c("Exp", "Sph", "Gau")))

vgLine <-
  cbind(variogramLine(semiva_teorico, maxdist = max(semiva_exp$dist)), id =
          "Semivariograma  Teórico")

ggplot(semiva_exp, aes(x = dist, y = gamma, color = id)) +
  geom_line(data = vgLine) +
  geom_point() +
  labs(title = "Semivariograma experimental y teorico ajustado")  +
  xlab("Distancia") +
  ylab("Semivarianza")  +
  scale_color_discrete(name = "Semivariograma",
                       labels = c("Teórico", "Experimental"))



## ---------------------------------------
kriging <-
  krige(Arcilla ~ 1,
        datos,
        grilla,
        model = semiva_teorico,
        nmax = 25)

grilla$Kriging <- kriging$var1.pred


## ---------------------------------------
grilla_rast <-
  st_rasterize(grilla, dx = 10, dy = 10)

tmap_mode('view')
mapa_prediccion_Kriging <-
  tm_shape(grilla_rast,
           name = "Kriging") +
  tm_raster(
    col = "Kriging",
    title = "Arcilla (%) Kriging",
    style = "fixed",
    palette = "YlOrBr",
    contrast = c(0.1, 1),
    breaks = c(30, 31, 32, 33, 34, 35, 36, 37)
  ) +
  tm_layout(legend.format = list(
    scientific = TRUE,
    format = "f",
    digits = 0
  ))
mapa_prediccion_Kriging


## ---------------------------------------
fitControl <- trainControl(method = "CV", number = 10)

set.seed(7)
train_rf <- train(
  Arcilla ~ .,
  data = st_drop_geometry(datos) ,
  method = "rf",
  trControl = fitControl
)
train_rf


## ---------------------------------------
datos$residuosRF <-
  datos$Arcilla - predict(train_rf, newdata = datos)


## ---------------------------------------
semiva_exp_residuos <- variogram(residuosRF ~ 1 , datos)

semiva_teorico_residuos <-
  fit.variogram(semiva_exp_residuos , vgm(c("Exp", "Sph", "Gau")))
plot(semiva_exp_residuos , semiva_teorico_residuos)



## ---------------------------------------
kgresRF <-
  krige(residuosRF ~ 1,
        datos,
        grilla,
        model = semiva_teorico_residuos,
        nmax = 25)


## ---------------------------------------
grilla$RF <-
  predict(train_rf, newdata = grilla)

grilla$RF_KO <-
  predict(train_rf, newdata = grilla) + kgresRF$var1.pred


## ---------------------------------------
grilla_rast <-
  st_rasterize(grilla, dx = 10, dy = 10)

mapa_prediccionRF <-
  tm_shape(grilla_rast,
           name = "RF") +
  tm_raster(
    col = "RF",
    title = "Arcilla (%) RF",
    style = "fixed",
    palette = "YlOrBr",
    contrast = c(0.1, 1),
    breaks = c(30, 31, 32, 33, 34, 35, 36, 37)
  ) +
  tm_layout(legend.format = list(
    scientific = TRUE,
    format = "f",
    digits = 0
  ))

mapa_prediccionRF



## ---------------------------------------
grilla_rast <-
  st_rasterize(grilla, dx = 10, dy = 10)

mapa_prediccionRF_KO <-
  tm_shape(grilla_rast,
           name = "RF-KO") +
  tm_raster(
    col = "RF_KO",
    title = "Arcilla (%) RF-KO",
    style = "fixed",
    palette = "YlOrBr",
    contrast = c(0.1, 1),
    breaks = c(30, 31, 32, 33, 34, 35, 36, 37)
  ) +
  tm_layout(legend.format = list(
    scientific = TRUE,
    format = "f",
    digits = 0
  ))

mapa_prediccionRF_KO


## ---------------------------------------
nn_datos <- near.obs(
  locations = as_Spatial(datos),
  observations = as_Spatial(datos),
  zcol = "Arcilla",
  n.obs = 4,
  idw = T
)
datos <- cbind(datos[, -7], nn_datos)



## ---------------------------------------
set.seed(7)
train_rfsi <- train(
  Arcilla ~ .,
  data = st_drop_geometry(datos) ,
  method = "rf",
  trControl = fitControl,
  importance = T
  
)
train_rfsi


## ---------------------------------------
importancia <- as.data.frame(importance(train_rfsi$finalModel))
importancia$Variable <- rownames(importancia)

ggplot(data = importancia, aes(
  x = reorder(Variable, `%IncMSE`),
  y = `%IncMSE`,
  fill = `%IncMSE`
)) +
  labs(x = "Variable", title = "Incremento de MSE (%)") +
  geom_col() +
  coord_flip() +
  theme_bw() +
  theme(legend.position = "bottom")



## ---------------------------------------
datos$residuosRFSI <-
  datos$Arcilla - predict(train_rfsi, newdata = datos)

semiva_exp_residuos_rfsi <- variogram(residuosRFSI ~ 1 , datos)
plot(semiva_exp_residuos_rfsi)


## ---------------------------------------
grilla_nn <-
  near.obs(
    locations = as_Spatial(grilla),
    observations = as_Spatial(datos),
    zcol = "Arcilla",
    n.obs = 4,
    idw = T
  )
grilla <- cbind(grilla, grilla_nn)

grilla$RFSI <-
  predict(train_rfsi, newdata = grilla)


## ---------------------------------------
grilla_rast <-
  st_rasterize(grilla, dx = 10, dy = 10)

mapa_prediccionRFSI <-
  tm_shape(grilla_rast,
           name = "RFSI") +
  tm_raster(
    col = "RFSI",
    title = "Arcilla (%) RFSI",
    style = "fixed",
    palette = "YlOrBr",
    contrast = c(0.1, 1),
    breaks = c(30, 31, 32, 33, 34, 35, 36, 37)
  ) +
  tm_layout(legend.format = list(
    scientific = TRUE,
    format = "f",
    digits = 0
  ))

mapa_prediccionRFSI



## ---------------------------------------
mapa_muestra <- tm_shape(datos,
                         name = "Muestra") +
  tm_dots(
    "Arcilla",
    title = "Arcilla (%) Muestra",
    style = "fixed",
    palette = "YlOrBr",
    contrast = c(0.1, 1),
    breaks = c(30, 31, 32, 33, 34, 35, 36, 37),
    size = 0.1,
    popup.vars = T,
    popup.format = list(
      digits = 1,
      decimal.mark = ",",
      big.mark = "."
    )
  ) +
  tm_layout(legend.format = list(scientific = TRUE, format = "f"))

mapas <-
  mapa_prediccion_Kriging + mapa_prediccionRF_KO + mapa_prediccionRF + mapa_prediccionRFSI + mapa_muestra
mapas



## ---------------------------------------
num_cores <- max(detectCores() - 1, 1)
cl <- makeCluster(num_cores)

tablavalidacion <- do.call(rbind, parLapply(cl, 1:10, validacion_arcilla))
stopCluster(cl)

TaylorDiagram(
  tablavalidacion,
  obs = "Observado",
  mod = "Predicho",
  group = c("Metodo"),
  cols = c("orange", "red", "#53ad5d", "#251cad"),
  annotate = "RMSE",
  xlab = "Desvio Estandar",
  ylab = "Desvio Estandar"
)

