# unconditional simulations on a 100 x 100 grid using gstat
library(gstat)

set.seed(1990)

n_obs <- 30
# create structure observed
x <- runif(n = n_obs, min = 0, max = 100)
y <- runif(n = n_obs, min = 0, max = 100)
xy_obs <- data.frame(x, y)
names(xy_obs) <- c("x", "y")
xy_obs$orig <- 'observed'

xy_obs_sf <- sf::st_as_sf(xy_obs, coords = c('x', 'y'))
# create structure predicted
xy_pred <- expand.grid(1:100, 1:100)
names(xy_pred) <- c("x", "y")
xy_pred$orig <- 'predicted'

xy <- rbind(xy_obs, xy_pred)
xy_sf <- sf::st_as_sf(xy, coords = c('x', 'y'))
# define the gstat object (spatial model)
g.dummy <-
  gstat(
    formula = z ~ 1,
    locations =  ~ x + y,
    dummy = TRUE,
    beta = 1,
    model = vgm(
      psill = 0.025,
      model = "Exp",
      range = 5
    ),
    nmax = 20
  )

# make four simulations based on the stat object
grid_pred <- sf::st_as_sf(sf::st_make_grid(xy_sf, cellsize = c(5,5)))


plot(grid_pred)



yy_obs <- predict(g.dummy, newdata = xy_obs_sf, nsim = 1)
yy_pred <- predict(g.dummy, newdata = sf::st_centroid(grid_pred), nsim = 1)

predicted <- sf::st_join(grid_pred, yy_pred)
# show one realization
my_cell_to_pred <- grid_pred[1 + nrow(grid_pred) / 2, ]
my_cell_to_pred$text <- "?" 
library(ggplot2)
ggplot() + 
  geom_sf(data = sf::st_boundary(sf::st_union(grid_pred))) +
# ggplot() + 
  geom_sf(data = yy_obs, aes(color = sim1)) +
# ggplot() +
  geom_sf(data = grid_pred, fill = NA) +
# ggplot() + 
  geom_sf(data = my_cell_to_pred, fill = 'red') +
# ggplot() + 
  geom_sf_text(data = my_cell_to_pred, aes(label = text), colour = "white") + 
  geom_sf(data = predicted, aes(fill = sim1))
  
  
  
  plot(sf::st_boundary(sf::st_union(grid_pred)), border = 1)
plot(yy_obs, add = TRUE)
plot(grid_pred, add = TRUE)
plot(my_cell_to_pred, col = 'red', add = TRUE)
plot(my_cell_to_pred, lab = '?', col = 'red', add = TRUE)
plot(predicted, add = TRUE)
