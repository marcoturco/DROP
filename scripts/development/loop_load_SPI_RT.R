rm(list = ls())
graphics.off()
gc()

library(ncdf4)
library(sp)
library(maptools) # loads sp library too
library(fields)
library(maps)
library(SPEI)

data(wrld_simpl)


## datasets
# datasets = c('MERRA2')
# 
datasets = c('ERA5',
             'CHIRPS',
             'CPC',
             'PRECL',
             'CAMS_OPI',
             'GPCC',
             'GPCP',
             'JRA55',
             'NCEP',
             'MSWEP')

## fix parameters
dir_oss = '/data/disk1/'

anni = 1981:2019
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
   if (dataset == "JRA55") {
    dir_spi='/data/disk1/JRA55'
    fname<-file.path(dir_spi, 'JRA55_1981_2019_25_monthly.nc')
    obs.nc <- nc_open(fname)
    obs <- ncvar_get(obs.nc,"prlr")
  } else if (dataset == "GPCP") {
    dir_spi='/data/disk1/GPCPv2_3/'
    fname <- file.path(dir_spi, 'gpcp_cdr_v23rB1_1981_2019.nc')
    obs.nc <- nc_open(fname)
    obs <- ncvar_get(obs.nc, "precip")
    obs[obs == "-9999"] <- NA
 } else if (dataset == "GPCC") {
   dir_spi='/data/disk1/GPCCv2018'
   fname<-file.path(dir_spi, 'prec_1981_2019.nc')
   obs.nc <- nc_open(fname)
   obs <- ncvar_get(obs.nc,"precip") 
   obs[obs=="-99999.9921875"] <- NA
  } else if (dataset == "CAMS_OPI") {
    dir_spi='/data/disk1/CAMS_OPI/cams_opi_v0208'
    fname<-file.path(dir_spi, 'camsopi_timecorrect-2.5-1981-2019.nc')
    obs.nc <- nc_open(fname)
    obs <- ncvar_get(obs.nc,"prcp") 
    # obs=obs[,,1:(dim(obs)[3]-12)]
    obs[obs=="-9.99e+08"] <- NA
    obs[,,63]=NA*obs[,,63] #BE CAREFULL, 198603 is missing
    
  } else if (dataset == "PRECL") {
    dir_spi='/data/disk1/PRECL'
    fname<-file.path(dir_spi, 'precip.mon.mean.2.5x2.5.1981_2019.nc')
    obs.nc <- nc_open(fname)
    obs <- ncvar_get(obs.nc,"precip") 
  } else if (dataset == "CPC") {
    dir_spi='/data/disk1/CPC_GLOBAL_PRECIP'
    fname<-file.path(dir_spi, 'precip_1981_2019_monthly.nc')
    obs.nc <- nc_open(fname)
    obs <- ncvar_get(obs.nc,"precip") 
    obs=obs[,,-dim(obs)[3]] #eliminate december 2019
    obs[obs=="-9.96920996838687e+36"] <- NA
  } else if (dataset == "CHIRPS") {
    dir_spi='/data/disk1/CHIRPS'
    fname<-file.path(dir_spi, 'chirps-v2.0.monthly-2.5.nc')
    obs.nc <- nc_open(fname)
    obs <- ncvar_get(obs.nc,"precip")
    obs[obs=="-9999"] <- NA
  } else if (dataset == "ERA5") {
    dir_spi='/data/disk1/ERA5'
    fname<-file.path(dir_spi, 'ERA5-drop.nc')
    obs.nc <- nc_open(fname)
    obs <- ncvar_get(obs.nc,"tp") 
    obs[obs=="-32767s"] <- NA
  } else if (dataset == "NCEP") {
    dir_spi='/data/disk1/NCEP'
    fname<-file.path(dir_spi, 'precip_1981_2019_monthly.nc')
    obs.nc <- nc_open(fname)
    obs <- ncvar_get(obs.nc,"prate") 
  } else if (dataset == "MERRA2") {
    dir_spi='/data/disk1/MERRA2'
    fname<-file.path(dir_spi, 'MERRA2_1981_2019_25_monthly.nc')
    obs.nc <- nc_open(fname)
    obs <- ncvar_get(obs.nc,"PRECTOTLAND")
    obs[obs=="999999986991104"] <- NA
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
  
  
   image(lon,lat,apply(obs,c(1,2),mean))
   plot(wrld_simpl,add=TRUE)
  # dim(obs)

  prec = array(data = NA,dim = c(nrow(obs),ncol(obs),length(anni)*12))
  prec[,,1:dim(obs)[3]]=obs
  
  
  #map("world2", add = TRUE)
  
  for (i in 1:dim(prec)[3]) {
    prec[, , i] = inout * prec[, , i]
  }
  

  
  aux=apply(prec,c(1,2),mean,na.rm=TRUE)
  image.plot(lon,lat,aux/aux)
  plot(wrld_simpl,add=TRUE)
  
  for (i in 1:dim(prec)[3]) {
    prec[,,i] = aux/aux * prec[,,i]
  }
  
  image.plot(lon, lat, apply(prec, c(1, 2), mean, na.rm = TRUE))
  plot(wrld_simpl, add = TRUE)
  
  ## calculate SPI
  
  spi1 = prec * NA
  spi3 = prec * NA
  spi6 = prec * NA
  spi12 = prec * NA
  for (i in 1:length(lon)) {
    print(paste0('grid ',i,' of ',length(lon)))   
    for (j in 1:length(lat)) {
      #if (!is.na(inout[i, j])) {
      if (!is.na(aux[i, j]/aux[i, j])) {
        
        
        #print(paste0('grid ',j,' of ',length(lat)))
        dum <- spi(prec[i, j,], 1, na.rm = TRUE)
        spi1[i, j,] = dum$fitted
        rm(dum)
        
        dum <- spi(prec[i, j,], 3, na.rm = TRUE)
        spi3[i, j,] = dum$fitted
        rm(dum)
        
        dum <- spi(prec[i, j,], 6, na.rm = TRUE)
        spi6[i, j,] = dum$fitted
        rm(dum)
        
        dum <- spi(prec[i, j,], 12, na.rm = TRUE)
        spi12[i, j,] = dum$fitted
        rm(dum)
        
      }
    }
  }
  
  # ## plot
  anno_2000 = which(anni == 2019)
  mese_aug = which(mesi == 10)
  mese_aug2000 = mese_aug[anno_2000]
  
  library(RColorBrewer)
  cols <- brewer.pal(9, "BrBG")
  image.plot(lon, lat, spi6[, , mese_aug2000],zlim = c(-2, 2), col = cols)
  plot(wrld_simpl, add = TRUE)
  
  
  ### SAVE
  save(spi1, file = file.path(dir_spi, paste0("SPI1_", dataset, "_1981_2019.RData")))
  save(spi3, file = file.path(dir_spi, paste0("SPI3_", dataset, "_1981_2019.RData")))
  save(spi6, file = file.path(dir_spi, paste0("SPI6_", dataset, "_1981_2019.RData")))
  save(spi12, file = file.path(dir_spi, paste0("SPI12_", dataset, "_1981_2019.RData")))
  
}
