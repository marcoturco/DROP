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


## datasets
datasets = c('CAMS_OPI')

# datasets = c('ERA5',
#              'CHIRPS',
#              'CPC',
#              'PRECL',
#              'CAMS_OPI',
#              'GPCC',
#              'GPCP',
#              'JRA55',
#              'NCEP',
#              'MSWEP')

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
for (idata in 1:length(datasets)) {
  dataset = datasets[idata]
  print(dataset)
   if (dataset == "MSWEP") {
     dir_spi='/Users/marco/Documents/dati/obs/MSWEP'
     fname<-file.path(dir_spi, 'global_monthly_2.50deg.nc.nc4')
     obs.nc <- nc_open(fname)
     obs <- ncvar_get(obs.nc,"precipitation") 
     obs[obs=="-9999.f"] <- NA
     obs=obs[,,-(1:24)] #elimino 1979 e 1980
   } else if (dataset == "MERRA2") {
     dir_spi='/Users/marco/Documents/dati/MERRA2'
     fname<-file.path(dir_spi, 'MERRA2_1981_2016_25_monthly.nc')
     obs.nc <- nc_open(fname)
     obs <- ncvar_get(obs.nc,"PRECTOTLAND")
     obs[obs=="999999986991104"] <- NA
     
     
     # obs.nc$dim$time$vals -> time
     
     # ## precipitation in mm/month
     # PRE=obs*NA
     # k = 0
     # anni=1981:2016
     # for (i in 1:length(anni)) {
     #   for (j in 1:12) {
     #     k = k + 1
     #     PRE[, , k] = obs[, , k] * monthDays(as.Date(paste(anni[i], "-", j, "-01", sep = "")))*86400
     #   }
     # }
     # obs=PRE
     # image.plot(lon, lat, obs[,,1])
     # plot(wrld_simpl, add = TRUE)
     #obs=obs[,,1:(dim(obs)[3]-12)]
  } else if (dataset == "JRA55") {
    dir_spi='/Users/marco/Documents/dati/obs/JRA55'
    fname<-file.path(dir_spi, 'prlr_1981_2017.nc')
    obs.nc <- nc_open(fname)
    obs <- ncvar_get(obs.nc,"prlr")
    obs=obs[,,1:(dim(obs)[3]-12)]
  } else if (dataset == "GPCP") {
    dir_spi='/Users/marco/Documents/dati/obs/GPCPv2_3/'
    fname <- file.path(dir_spi, 'gpcp_cdr_v23rB1_1981_2017.nc')
    obs.nc <- nc_open(fname)
    obs <- ncvar_get(obs.nc, "precip")
    obs = obs[, , 1:(dim(obs)[3] - 12)]
    obs[obs == "-9999"] <- NA
 } else if (dataset == "GPCC") {
   dir_spi='/Users/marco/Documents/dati/obs/GPCCv2018'
   fname<-file.path(dir_spi, 'prec_1981_2017.nc')
   obs.nc <- nc_open(fname)
   obs <- ncvar_get(obs.nc,"precip") 
   obs=obs[,,1:(dim(obs)[3]-12)]
   obs[obs=="-99999.9921875"] <- NA
  } else if (dataset == "CAMS_OPI") {
    dir_spi='/Users/marco/Documents/dati/obs/CAMS_OPI'
    fname<-file.path(dir_spi, 'prec_1981_2017.nc')
    obs.nc <- nc_open(fname)
    obs <- ncvar_get(obs.nc,"prcp") 
    obs=obs[,,1:(dim(obs)[3]-12)]
    obs[obs=="32767"] <- NA
  } else if (dataset == "PRECL") {
    dir_spi='/Users/marco/Documents/dati/obs/PRECL'
    fname<-file.path(dir_spi, 'precip.mon.mean.2.5x2.5.1981_2017.nc')
    obs.nc <- nc_open(fname)
    obs <- ncvar_get(obs.nc,"precip") 
    obs=obs[,,1:(dim(obs)[3]-12)]
    obs[obs=="-9.96920996838687e+36"] <- NA
  } else if (dataset == "CPC") {
    dir_spi='/Users/marco/Documents/dati/obs/CPC_GLOBAL_PRECIP'
    fname<-file.path(dir_spi, 'precip_1981_2017_monthly.nc')
    obs.nc <- nc_open(fname)
    obs <- ncvar_get(obs.nc,"precip") 
    obs=obs[,,1:(dim(obs)[3]-12)]
    obs[obs=="-9.96920996838687e+36"] <- NA
  } else if (dataset == "CHIRPS") {
    dir_spi='/Users/marco/Documents/dati/SPIF/data'
    fname<-file.path(dir_spi, 'chirps-v2.0.monthly-2.5_1981_2017.nc')
    obs.nc <- nc_open(fname)
    obs <- ncvar_get(obs.nc,"precip") 
    obs=obs[,,1:(dim(obs)[3]-12)]
    obs[obs=="-9999"] <- NA
  } else if (dataset == "ERA5") {
    dir_spi='/Users/marco/Documents/dati/obs/ERA5'
    fname<-file.path(dir_spi, 'precip_1981_2017_monthly.nc')
    obs.nc <- nc_open(fname)
    obs <- ncvar_get(obs.nc,"prlr") 
    obs=obs[,,1:(dim(obs)[3]-12)]
  } else if (dataset == "NCEP") {
    dir_spi='/Users/marco/Documents/dati/obs/NCEP'
    fname<-file.path(dir_spi, 'precip_1981_2017_monthly.nc')
    obs.nc <- nc_open(fname)
    obs <- ncvar_get(obs.nc,"prate") 
    obs=obs[,,1:(dim(obs)[3]-12)]
  } else {
    print('dataset not known')
  }
  
  obs = obs[, ncol(obs):1, ]
  dum = obs
  dum[1:(nrow(obs) / 2), , ] = obs[(nrow(obs) / 2 + 1):nrow(obs), , ]
  dum[(nrow(obs) / 2 + 1):nrow(obs), , ] = obs[1:(nrow(obs) / 2), , ]
  rm(obs)
  obs = dum
  rm(dum)
  
  image.plot(lon, lat, obs[,,100])
  plot(wrld_simpl, add = TRUE)
  
  # image(lon,lat,apply(obs,c(1,2),mean))
  # plot(wrld_simpl,add=TRUE)
  # #map("world2", add = TRUE)
  
  for (i in 1:dim(obs)[3]) {
    obs[, , i] = inout * obs[, , i]
  }
  

  
  aux=apply(obs,c(1,2),mean,na.rm=TRUE)
  image.plot(lon,lat,aux/aux)
  plot(wrld_simpl,add=TRUE)
  
  for (i in 1:dim(obs)[3]) {
    obs[,,i] = aux/aux * obs[,,i]
  }
  
  image.plot(lon, lat, apply(obs, c(1, 2), mean, na.rm = TRUE))
  plot(wrld_simpl, add = TRUE)
  
  image.plot(lon, lat, obs[,,432])
  plot(wrld_simpl, add = TRUE)
  
  
  ## calculate SPI
  
  spi1 = obs * NA
  spi3 = obs * NA
  spi6 = obs * NA
  spi12 = obs * NA
  for (i in 1:length(lon)) {
    print(paste0('grid ',i,' of ',length(lon)))   
    for (j in 1:length(lat)) {
      #if (!is.na(inout[i, j])) {
      if (!is.na(aux[i, j]/aux[i, j])) {
        
        
        #print(paste0('grid ',j,' of ',length(lat)))
        dum <- spi(obs[i, j,], 1, na.rm = TRUE)
        spi1[i, j,] = dum$fitted
        rm(dum)
        
        dum <- spi(obs[i, j,], 3, na.rm = TRUE)
        spi3[i, j,] = dum$fitted
        rm(dum)
        
        dum <- spi(obs[i, j,], 6, na.rm = TRUE)
        spi6[i, j,] = dum$fitted
        rm(dum)
        
        dum <- spi(obs[i, j,], 12, na.rm = TRUE)
        spi12[i, j,] = dum$fitted
        rm(dum)
        
      }
    }
  }
  
  ## plot
  anno_2000 = which(anni == 2003)
  mese_aug = which(mesi == 8)
  mese_aug2000 = mese_aug[anno_2000]
  # 
  image.plot(lon, lat, spi6[, , mese_aug2000])
  plot(wrld_simpl, add = TRUE)
  
  
  ### SAVE
  save(spi1, file = file.path(dir_spi, paste0("SPI1_", dataset, "_1981_2016.RData")))
  save(spi3, file = file.path(dir_spi, paste0("SPI3_", dataset, "_1981_2016.RData")))
  save(spi6, file = file.path(dir_spi, paste0("SPI6_", dataset, "_1981_2016.RData")))
  save(spi12, file = file.path(dir_spi, paste0("SPI12_", dataset, "_1981_2016.RData")))
  
}
