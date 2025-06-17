


tre_cov2percent_har <- tre_cov2  %>% 
  group_by(PlotID) %>% 
  mutate(
    tot_ba = sum(BA_m2ha),
    tot_den = sum(density)
  ) %>% 
  mutate(
    per_ba = BA_m2ha / tot_ba,
    per_den = density / tot_den
  ) %>% 
  filter(type == "Hardwood")


library(gstat)  
library(sp)
library(geoR)


for_plots_sf



# Extract the variable of interest (e.g., per_ba)
data_var <- test_spa$BA_m2ha

GEODATA  <-  as.geodata(cbind(coords,data_var))
plot(GEODATA)

EMP_VARIOGRAM  <-  variog(GEODATA)
 
## variog: computing  omnidirectional  variogram
 
FIT_VARIOGRAM  <-  variofit(EMP_VARIOGRAM)
 
## variofit: covariance model used is  matern
## variofit: weights   used:  npairs
## variofit: minimisation function  used:  optim
 
## Warning in variofit(EMP_VARIOGRAM): initial values  not  provided - running the default  search
 
## variofit: searching for  best  initial value  ... selected values:
##                        	sigmasq phi   	tausq  kappa
## initial.value "9.19"   "3.65" "0"        	"0.5"
## status          	"est"  	"est"   "est"  "fix"
## loss value: 401.578968904954

plot(EMP_VARIOGRAM)
lines(FIT_VARIOGRAM)

res  <-  50
grid <-  expand.grid(seq(697277.7, 700322.2,res),
                     seq(4833220, 4835201,res))

krico <-  krige.control(type.krige="OK",
                        obj.model=FIT_VARIOGRAM)

krobj <- krige.conv(GEODATA,
                    locations=grid, 
                    krige=krico)

image(krobj, col=rainbow(100))
legend.krige(col=rainbow(100),
                     x.leg=c(6.2,6.7),  y.leg=c(2,6),
                     vert=T, off=-0.5,
                     values=krobj$predict)


contour(krobj, add=T)
colPoints(x,y,z, col=rainbow2(100),  legend=F)
points(x,y)

## krige.conv: model with  constant mean
## krige.conv: Kriging performed  using  global  neighbourhood
 
# KRigingObjekt



library(geoR)
library(dplyr)

# Join and filter as before
test_spa <- left_join(for_plots_sf, tre_cov2percent_har, by = "PlotID") %>% filter(park == "MABI")

# Extract and clean
coords <- sf::st_coordinates(test_spa)
data_var <- test_spa$BA_m2ha
df <- as.data.frame(cbind(coords, data_var))
df <- df[complete.cases(df), ]

# Create geodata object
GEODATA <- as.geodata(df, coords.col = 1:2, data.col = 3)
plot(GEODATA)

# Variogram and fit
EMP_VARIOGRAM <- variog(GEODATA)
plot(EMP_VARIOGRAM)
FIT_VARIOGRAM <- variofit(EMP_VARIOGRAM)
lines(FIT_VARIOGRAM)

# Kriging grid
res <- 50
grid <- expand.grid(seq(min(df$X), max(df$X), res),
                    seq(min(df$Y), max(df$Y), res))

plot(grid)
points(df$X, df$Y, col = df$data_var, pch = 16, cex = 4)

krico <- krige.control(type.krige = "OK", obj.model = FIT_VARIOGRAM)
krobj <- krige.conv(GEODATA, locations = grid, krige = krico)

image(krobj, col = rainbow(100))
legend.krige(col = rainbow(100),
             x.leg = c(6.2, 6.7), y.leg = c(2, 6),
             vert = TRUE, off = -0.5,
             values = krobj$predict)
contour(krobj, add = TRUE)
points(df[,1], df[,2], pch = 21, bg = "red")


#### book
meuse <- as.data.frame(coords)
colnames(meuse) <- c("x", "y")

# Convert to SpatialPoints
sp::coordinates(meuse) <- c("x", "y")

f <- df$data_var ~ sqrt(coords)
vt <- variogram(f, data = meuse)

lz <- krige(data_var ~ 1, df, grid, v.fit)


##  https://rpubs.com/leydetd/spatialinterpolation1
library(raster)
library(sf)
library(gstat)

test_spa <- left_join(for_plots_sf, tre_cov2percent_har, by = "PlotID")  %>% filter(park == "MABI")
test_spa2 <- as_Spatial(test_spa%>% filter(BA_m2ha < 250))

# Extract coordinates as a matrix
coords <- sf::st_coordinates(test_spa)
class(test_spa)

# elevation
r <- raster("/Users/bamaral/Downloads/Elevation_DEM10M0809.img")
r_matched <- projectRaster(r, crs = st_crs(test_spa)$proj4string)

# soil type
soil <- raster("/Users/bamaral/Downloads/VT-selected/RSS_VT/spatial/MURASTER_10m_VT_2025.tif")
soil_matched <- projectRaster(soil, crs = st_crs(test_spa)$proj4string)

crs(r_matched)
crs(test_spa)

e <- extent(min(df$X) - 150, max(df$X) + 150,
            min(df$Y) - 150, max(df$Y) + 150)  # <-- change to your desired area
# Crop the raster
r_crop <- crop(r_matched, e)
#pts <- rasterToPoints(r_crop, spatial = TRUE)
#r_sf <- st_as_sf(pts)
#plot(r_sf)

soil_crop <- crop(soil_matched, e)

plot(soil_crop)
plot(r_crop)
points(coords, cex = 2, col = "black", pch = 16)

par(mfrow = c(1,1))
hist(log(test_spa$BA_m2ha))
hist(test_spa2$BA_m2ha)


test_spa$logba <- log(test_spa$BA_m2ha)

ba.var <- variogram(logba ~ 1, data = test_spa)
ba.var <- variogram(BA_m2ha ~ 1, data = test_spa, locations = coords)
ba.var <- variogram(BA_m2ha ~ , data = test_spa2, locations = coords)

plot(ba.var, plot.numbers = TRUE, pch = '+')

##Build the model
ppt.vgm1 = vgm(psill = modsill2,
               model = "Sph",
               range = modrange2,
               nugget = modnug)

ppt.vgm2 <- fit.variogram(ba.var, vgm("Exp"))

library(sf)

# Suppose grid columns are named Var1 and Var2 (from expand.grid)
grid_sf <- st_as_sf(grid, coords = c("Var1", "Var2"), crs = st_crs(test_spa))

# Example: kriging for BA_m2ha
ba.pred <- krige(
  BA_m2ha ~ 1,           # formula
  locations = test_spa,  # must be sf or Spatial*
  newdata = grid_sf,        # grid should also be sf or Spatial*
  model = ppt.vgm2,
  nmax = 40
)

plot(ba.pred, pch = 15, cex = 2.3)
points(coords, cex = 2, col = "black", pch = 16)

# Convert raster to points (SpatialPointsDataFrame)
ele_pts <- rasterToPoints(r_crop, spatial = TRUE)
soil_pts <- rasterToPoints(soil_crop, spatial = TRUE)
# Convert to sf object
ele_sf <- st_as_sf(ele_pts)
soil_sf <- st_as_sf(soil_pts)

ele.pred <- krige(BA_m2ha ~ 1,
                    locations = test_spa,
                    newdata = ele_sf,
                    model = ppt.vgm2)

combined_sf <- rbind(ele_sf, soil_sf)

elesoil.pred <- krige(BA_m2ha ~ 1,
                    locations = test_spa,
                    newdata = soil_sf,
                    model = ppt.vgm2)

plot(ele.pred["var1.pred"],
     main = "Interpolated Basal Area Values",
     reset = FALSE, cex = 1.5, pch = 15)
points(coords, cex = 1, col = "black", pch = 16)
xy_sf <- st_transform(xy_sf, st_crs(test_spa))
points(xy_sf %>% filter(park == "MABI") %>% st_coordinates(), cex = 1, col = "white", pch = 16)

plot(ele.pred["var1.var"],
     main = "Interpolated Basal Area Values",
     reset = FALSE, cex = 1.5, pch = 15)
points(coords, cex = 1, col = "black", pch = 16)
points(xy_sf %>% filter(park == "MABI") %>% st_coordinates(), cex = 1, col = "white", pch = 16)


plot(elesoil.pred["var1.pred"],
     main = "Interpolated Basal Area Values",
     reset = FALSE, cex = 1.5, pch = 15)
points(coords, cex = 1, col = "black", pch = 16)
xy_sf <- st_transform(xy_sf, st_crs(test_spa))
points(xy_sf %>% filter(park == "MABI") %>% st_coordinates(), cex = 1, col = "white", pch = 16)
