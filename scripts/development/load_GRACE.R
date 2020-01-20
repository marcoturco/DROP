rm(list = ls())
graphics.off()
gc()

library(ncdf4)
library(sp)
library(maptools) # loads sp library too
library(fields)
library(maps)
library(SPEI)
library(Hmisc)

data(wrld_simpl)




## fix parameters
dir_oss = '/Users/marco/Documents/dati/obs'

anni=1981:2016
mesi = rep(1:12, length(anni))

## mask
load(file.path(dir_oss, "GPCPv2_3/lon_GPCP_1981_2017.RData"))
load(file.path(dir_oss, "GPCPv2_3/lat_GPCP_1981_2017.RData"))

points <- expand.grid(lon, lat)
data(wrld_simpl)
pts = SpatialPoints(points, proj4string = CRS(proj4string(wrld_simpl)))
ii <- !is.na(over(pts, wrld_simpl))
inout = ii[, 1]
dim(inout) <- c(length(lon), length(lat))
inout[inout == 0] = NA

image.plot(lon, lat, inout)
plot(wrld_simpl, add = TRUE)

## load data and spi calculation

dir_spi='/Users/marco/Documents/dati/GRACE'
fname<-file.path(dir_spi, 'GRACE_DSI_JPLMasconV2_2002_2017_25.nc')
obs.nc <- nc_open(fname)
obs.nc$dim$time$vals -> tiempo
obs <- ncvar_get(obs.nc,"GRACE-DSI") 
obs[obs=="NaN"] <- NA

# grace has 192 time step, from 2002 to 2017, but actually has data for the period: April 2002 to June 2017 
obs=obs[,,-(181:192)] #elimino dati del 2017
   
  
obs = obs[, ncol(obs):1,]
dum = obs
dum[1:(nrow(obs) / 2), ,] = obs[(nrow(obs) / 2 + 1):nrow(obs), ,]
dum[(nrow(obs) / 2 + 1):nrow(obs), ,] = obs[1:(nrow(obs) / 2), ,]
rm(obs)
obs = dum
rm(dum)
  

image(lon, lat, apply(obs, c(1, 2), mean,na.rm=TRUE))
plot(wrld_simpl, add = TRUE)

for (i in 1:dim(obs)[3]) {
  obs[, , i] = inout * obs[, , i]
}


  
aux = apply(obs, c(1, 2), mean, na.rm = TRUE)
image.plot(lon, lat, aux / aux)
plot(wrld_simpl, add = TRUE)

for (i in 1:dim(obs)[3]) {
  obs[, , i] = aux / aux * obs[, , i]
}
  
image.plot(lon, lat, apply(obs, c(1, 2), mean, na.rm = TRUE))
plot(wrld_simpl, add = TRUE)
  
### SAVE
save(obs, file = file.path(dir_spi, paste0("GRACE_200204_201612.RData")))


