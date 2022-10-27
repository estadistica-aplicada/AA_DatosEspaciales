## ---------------------------------------
library(gstat)
library(spdep)
library(tmap)
library(leaflet)
library(stars)
library(ggplot2)
library(caret)
library(randomForest)
require(parallel)
library(openair)

source("src/near.obs.R")
source("src/fvalidacion_rinde.R")

tmap_options(
  basemaps = c(
    'Satelital' = leaflet::providers$Esri.WorldImagery,
    'OSM' = leaflet::providers$OpenStreetMap
  )
)


## ---------------------------------------
datos <- st_read("data/Rinde_soja.gpkg")[-c(1:3)]
head(datos)


## ---------------------------------------
semiva_exp <- variogram(REND ~ 1 , datos, cutoff=300)

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
limites <- st_read("data/Limites_Rinde_soja.gpkg")
plot(limites)

grilla <- st_bbox(limites) %>%
  st_as_stars(dx = 10) %>%
  st_crop(limites)



## ---------------------------------------
kriging <-
  krige(REND ~ 1,
        datos,
        grilla,
        model = semiva_teorico,
        nmax = 25)

grilla$KO <- kriging$var1.pred


## ---------------------------------------
nn_datos <- near.obs(
  locations = as_Spatial(datos),
  observations = as_Spatial(datos),
  zcol = "REND",
  n.obs = 4
)

datos_nn <- cbind(datos, nn_datos)


## ---------------------------------------
fitControl <-
  trainControl(method = "none")

set.seed(7)
train_rfsi_rinde <- train(
  REND ~ .,
  data = st_drop_geometry(datos_nn),
  method = "rf",
  trControl = fitControl,
  importance = T
)
train_rfsi_rinde


## ---------------------------------------
importancia_rinde <-
  as.data.frame(importance(train_rfsi_rinde$finalModel))
importancia_rinde$Variable <- rownames(importancia_rinde)

ggplot(data = importancia_rinde, aes(
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
datos_nn$residuosRFSI <-
  datos_nn$REND - predict(train_rfsi_rinde, newdata = datos_nn)

semiva_exp_residuos_rfsi <- variogram(residuosRFSI ~ 1 , datos_nn)
plot(semiva_exp_residuos_rfsi)


## ---------------------------------------
grilla_nn_rinde <-
  near.obs(
    locations = as_Spatial(st_as_sf(grilla, as_points = T)),
    observations = as_Spatial(datos_nn),
    zcol = "REND",
    n.obs = 4
  )

grilla_nn_rinde <-
  cbind(st_as_sf(grilla, as_points = T), grilla_nn_rinde)

grilla_nn_rinde$RFSI <-
  predict(train_rfsi_rinde, newdata = grilla_nn_rinde)


## ---------------------------------------
grilla_nn_rinde_rast <-
  st_rasterize(grilla_nn_rinde, dx = 10, dy = 10)

tmap_options(
  basemaps = c(
    'Satelital' = leaflet::providers$Esri.WorldImagery,
    'OSM' = leaflet::providers$OpenStreetMap
  )
)

tmap_mode('view')

breaks_custom <- pretty(c(grilla_nn_rinde_rast$KO,
                          grilla_nn_rinde_rast$RFSI),
                        n = 6)

kriging_rinde <-
  tm_shape(grilla_nn_rinde_rast,
           name = "Kriging") +
  tm_raster(
    col = "KO",
    title = "Rendimiento (t/ha) Kriging",
    style = "cont",
    palette = "YlOrBr",
    contrast = c(0.1, 1),
    breaks = breaks_custom
  ) +
  tm_layout(legend.format = list(scientific = TRUE,
                                 format = "f"))

RFSI_rinde <-
  tm_shape(grilla_nn_rinde_rast,
           name = "RFSI") +
  tm_raster(
    col = "RFSI",
    title = "Rendimiento (t/ha) RFSI",
    style = "cont",
    palette = "YlOrBr",
    contrast = c(0.1, 1),
    breaks = breaks_custom
  ) +
  tm_layout(legend.format = list(scientific = TRUE,
                                 format = "f"))


## ---------------------------------------
RFSI_rinde + kriging_rinde



## ---- eval = FALSE----------------------
## num_cores <- max(detectCores() - 1, 1)
## cl <- makeCluster(num_cores)
## tablavalidacion <- do.call(rbind, parLapply(cl, 1:10, validacion_rinde))
## stopCluster(cl)
## saveRDS(tablavalidacion, 'data/tablavalidacion.RDS')


## ---------------------------------------
tablavalidacion <- readRDS('data/tablavalidacion.RDS')


## ---------------------------------------
TaylorDiagram(
  tablavalidacion,
  obs = "Observado",
  mod = "Predicho",
  group = c("Metodo"),
  cols = c("orange", "#251cad"),
  cor.col = 'brown',
  rms.col = 'black',
  annotate = "RMSE",
  xlab = "Desvio Estandar",
  ylab = "Desvio Estandar"
)


