#### 0. Setting ####
# this is just a comment

1+1
a = 1+1

# clear workspace
rm(list = ls())
# clear plot
graphics.off()
# clear console
cat("\014")

# set working directory
setwd("C:/Introduction to Basic Geostatistics using R")

# use gstat geostatistics package
if (!require("gstat")) {
  install.packages("gstat")
}

# this package is for plotting
if (!require("lattice")) {
  install.packages("lattice")
}
# this package is for plotting
if (!require("sp")) {
  install.packages("sp")
}
# this package is for plotting
if (!require("latticeExtra")) {
  install.packages("latticeExtra")
}
# disable scientific notation when plotting
options(scipen=999)



#### 1. Data Preparation ####

# load the data
data_latihan = read.csv("latihan.csv", header = TRUE, sep = ",")

# plot the data location
plot(data_latihan$X,data_latihan$Y)

# create the histogram of variable Nilai
hist(data_latihan$Nilai)



#### 2. Experimental Variogram ####

# create a gstat object containing data+coordinates
gs = gstat(id = "Nilai", formula = Nilai~1, locations = ~X+Y, data = data_latihan)

# calculate the experimental variogram
exp_var = variogram(gs)

# plot the experimental variogram
plot(exp_var)

# limit the lag distance for variogram calculation
# exp_var = variogram(gs, cutoff=240000)

# plot the experimental variogram
# plot(exp_var)



#### 3. Variogram modelling ####

# create a variogram model
vg_model = vgm(psill = 19, model = "Sph", range = 90000, nugget = 10)
vg_model = fit.variogram(exp_var, model=vg_model)
# vg_model

# plot the variogram model
plot(exp_var, model=vg_model)

# update the gstat object
gs = gstat(id = "Nilai", formula = Nilai~1, locations = ~X+Y, data = data_latihan, nmax=30, model=vg_model)



#### 4. Kriging ####

# creating grid for kriging
summary(data_latihan)
x = seq(from=372000, to=862000, by=5000)
y = seq(from=7371000, to=7881000, by=5000)
# the variable X & Y are the same name of the gs object
xy_grid = expand.grid(X=x, Y=y)

# do kriging
hasil_kriging = predict(gs, newdata=xy_grid, debug.level=-1)

# plot the result
aux = hasil_kriging$Nilai.pred
# convert a vector to a matrix (for plotting purpose)
dim(aux)=c(length(x),length(y))
image(x,y,aux,asp=1)
# plot the data location
points(data_latihan$X, data_latihan$Y, pch = "+")

# other plotting methods
# use lattice package
levelplot(hasil_kriging$Nilai.pred ~ hasil_kriging$X + hasil_kriging$Y, col.regions=heat.colors(100))
# use sp package
aux = hasil_kriging
coordinates(aux) <- c("X","Y")
p <- spplot(aux["Nilai.pred"], asp=1, colorkey = TRUE, xlim=c(372000,862000), ylim=c(7371000,7881000), scales = list(draw = TRUE))
p + layer(panel.points(X, Y, col="black", pch=1), data=data_latihan)

# plot the kriging variance result
aux_var = hasil_kriging$Nilai.var
# convert a vector to a matrix (for plotting purpose)
dim(aux_var)=c(length(x),length(y))
image(x,y,aux_var,asp=1)
points(data_latihan$X, data_latihan$Y, pch = "+")
# other plotting methods
# use lattice package
levelplot(hasil_kriging$Nilai.var ~ hasil_kriging$X + hasil_kriging$Y, col.regions=heat.colors(100))
# use sp package
aux_var = hasil_kriging
coordinates(aux_var) <- c("X","Y")
p <- spplot(aux_var["Nilai.var"], asp=1, colorkey = TRUE, xlim=c(372000,862000), ylim=c(7371000,7881000), scales = list(draw = TRUE))
p + layer(panel.points(X, Y, col="black", pch=1), data=data_latihan)


# cropping the prediction
aux = hasil_kriging$Nilai.pred
aux[hasil_kriging$Nilai.var>25]=NA
# convert a vector to a matrix (for plotting purpose)
dim(aux)=c(length(x),length(y))
image(x,y,aux,asp=1)
points(data_latihan$X, data_latihan$Y, pch = "+")
# another plotting method
# use lattice package
levelplot(aux ~ hasil_kriging$X + hasil_kriging$Y, col.regions=heat.colors(100))
# use sp package
aux = hasil_kriging$Nilai.pred
aux[hasil_kriging$Nilai.var>25]=NA
aux_sp = hasil_kriging
aux_sp["Nilai.pred"]=aux
coordinates(aux_sp) <- c("X","Y")
p <- spplot(aux_sp["Nilai.pred"], asp=1, colorkey = TRUE, xlim=c(372000,862000), ylim=c(7371000,7881000), scales = list(draw = TRUE))
p + layer(panel.points(X, Y, col="black", pch=1), data=data_latihan)



#### 5. Cross validation ####

# do cross validation
cross_val = gstat.cv(gs, nfold=nrow(data_latihan))
plot(observed~Nilai.pred, data=cross_val, asp=1)
abline(a=0, b=1, col=2, lwd=2)
# calculate RMSE (Root Mean Squared Error)
# sqrt(sum(cross_val$residual^2)/length(cross_val$residual))
sqrt(mean(cross_val$residual^2))
# calculate mean error
# should be close to 0
mean(cross_val$residual)
# calculate MSDR (Mean Squared Deviation Ratio)
# should be close to 1
mean(cross_val$residual^2/cross_val$Nilai.var)



#### 6. Simulation ####

# do simulation
hasil_simulasi = predict(gs, newdata=xy_grid, debug.level=-1, nsim=10)
aux = hasil_simulasi$sim3
# convert a vector to a matrix (for plotting purpose)
dim(aux)=c(length(x),length(y))
image(x,y,aux,asp=1)
points(data_latihan$X, data_latihan$Y, pch = "+")
# another plotting method
# use lattice package
levelplot(aux ~ hasil_simulasi$X + hasil_simulasi$Y, col.regions=heat.colors(100))
# use sp package
aux = hasil_simulasi
coordinates(aux) <- c("X","Y")
spplot(aux, colorkey = TRUE)



#### 7. Variogram map ####

# create a variogram map
var_map = variogram(gs, cutoff = 240000, width = 20000, map = TRUE)
plot(var_map)

# anisotropic variograms
exp_vg_aniso = variogram(gs, alpha=c(0,315,135,45,225))
plot(exp_vg_aniso)
vg_model_aniso = vgm(psill = 20, "Sph", range = 90000, nugget = 10, anis = c(315, .5))
plot(exp_vg_aniso, vg_model_aniso, as.table = TRUE)

# update the gstat object
gs_aniso = gstat(id = "Nilai", formula = Nilai~1, locations = ~X+Y, data = data_latihan, nmax=30, model=vg_model_aniso)

# kriging using anisotropic variograms
hasil_kriging_aniso = predict(gs_aniso, newdata=xy_grid, debug.level=-1)

# plot
# use lattice package
levelplot(hasil_kriging_aniso$Nilai.pred ~ hasil_kriging_aniso$X + hasil_kriging_aniso$Y, col.regions=heat.colors(100))
