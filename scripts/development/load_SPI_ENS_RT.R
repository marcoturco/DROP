rm(list = ls())
graphics.off()
gc()

library(sp)
library(maptools) # loads sp library too
library(RColorBrewer) # creates nice color schemes
library(classInt) # finds class intervals for continuous variables
library(fields)
library(maps)

##

data(wrld_simpl)


## fix parameters
dir_oss = '/data/disk1/'
dir_out = '/data/disk1/DROP/'
dir_data = '/home/mt/Dropbox/estcena/scripts/obs_uncertainty/app_drop/data/'
anni = 1981:2020
mesi = rep(1:12, length(anni))
mesi_8 = which(mesi == 12)


load(file.path(dir_oss, "GPCPv2_3/lon_GPCP_1981_2017.RData"))
load(file.path(dir_oss, "GPCPv2_3/lat_GPCP_1981_2017.RData"))


## load data
pred = array(data = NA, dim = c(length(lon), length(lat), length(mesi), 10))


time_scale = c(1,3,6,12)

for (isc in 1:length(time_scale)) {
  sc = time_scale[isc]
  
  nam <- paste("spi", sc, sep = "")
  print(nam)
  
  load(file.path(
    paste(dir_oss,
      "/CAMS_OPI/cams_opi_v0208/SPI",
      sc,
      "_CAMS_OPI_1981_2020.RData",
      sep = ""
    )
  ))
  aux = get(nam)
  pred[, , , 1] = aux[, ,]
  
  load(file.path(
    paste(dir_oss,"/CHIRPS/SPI",
      sc,
      "_CHIRPS_1981_2020.RData",
      sep = ""
    )
  ))
  aux = get(nam)
  pred[, , , 2] = aux[, ,]
  
  load(file.path(
    paste(
      dir_oss,
      "/CPC_GLOBAL_PRECIP/SPI",
      sc,
      "_CPC_1981_2020.RData",
      sep = ""
    )
  ))
  aux = get(nam)
  pred[, , , 3] = aux[, ,]
  
  load(file.path(
    paste(
      dir_oss,
      "/ERA5/SPI",
      sc,
      "_ERA5_1981_2020.RData",
      sep = ""
    )
  ))
  aux = get(nam)
  pred[, , , 4] = aux[, ,]
  
  
  load(file.path(
    paste(dir_oss, "/GPCCv2018/SPI", sc, "_GPCC_1981_2020.RData", sep = "")
  ))
  aux = get(nam)
  pred[, , , 5] = aux[, ,]
  
  load(file.path(
    paste(dir_oss, "/GPCPv2_3/SPI", sc, "_GPCP_1981_2020.RData", sep = "")
  ))
  aux = get(nam)
  pred[, , , 6] = aux[, ,]
  
  
  load(file.path(
    paste(dir_oss, "/JRA55/SPI", sc, "_JRA55_1981_2020.RData", sep = "")
  ))
  aux = get(nam)
  pred[, , , 7] = aux[, ,]
  
  load(file.path(
    paste(dir_oss,
      "/MERRA2/SPI",
      sc,
      "_MERRA2_1981_2020.RData",
      sep = ""
    )
  ))
  aux = get(nam)
  pred[, , , 8] = aux[, ,]
  
  load(file.path(
    paste(dir_oss, "/NCEP/SPI", sc, "_NCEP_1981_2020.RData", sep = "")
  ))
  aux = get(nam)
  pred[, , , 9] = aux[, ,]
  
  load(file.path(
    paste(dir_oss, "/PRECL/SPI", sc, "_PRECL_1981_2020.RData", sep = "")
  ))
  aux = get(nam)
  pred[, , , 10] = aux[, ,]
  
  spi = (apply(pred, c(1, 2, 3), mean, na.rm = TRUE))
  anno_2000 = which(anni == 2019)
  mese_aug = which(mesi == 12)
  mese_aug2000 = mese_aug[anno_2000]
  spi[,,mese_aug2000:dim(spi)[3]]=NA
  
  for (i in 1:dim(pred)[1]) {
    for (j in 1:dim(pred)[2]) {
      if (sum(!is.na(spi[i,j,]))!=0) {
        spi[i,j,]=scale(spi[i,j,])
      }
    }
  }
  save(spi, file = paste0(dir_out, "/SPI", sc, "_ENS_1981_2020.RData"))
  save(spi, file = paste0(dir_data, "/SPI", sc, "_ENS_1981_2020.RData"))
  
  image.plot(lon, lat, spi[, , mese_aug2000-2])
  plot(wrld_simpl, add = TRUE)
  # 

  spi_sd = apply(pred, c(1, 2, 3), sd, na.rm = TRUE)
  spi_sd[,,mese_aug2000:dim(spi)[3]]=NA
  save(spi_sd, file = paste0(dir_out, "/SPI", sc, "_ENS_SPREAD_1981_2020.RData"))
  save(spi_sd, file = paste0(dir_data, "/SPI", sc, "_ENS_SPREAD_1981_2020.RData"))
  
  points <- expand.grid(lon, lat)
  pts = SpatialPoints(points, proj4string = CRS(proj4string(wrld_simpl)))
  ii <- !is.na(over(pts, wrld_simpl))
  inout = ii[, 1]
  dim(inout) <- c(nrow(pred), ncol(pred))
  inout[inout == 0] = NA
  
  spi_prob = array(data = NA, dim = c(length(lon), length(lat), dim(pred)[3]))
  for (im in 1:dim(pred)[3]) {
    ## Plot Probability Moderate Drought
    
    for (i in 1:dim(pred)[1]) {
      for (j in 1:dim(pred)[2]) {
        aux = pred[i, j,im,]
        spi_prob[i, j,im] = (sum(aux[!is.na(aux)] <= -0.8) / sum(!is.na(aux)))
        
      }
    }
    spi_prob[, ,im]=spi_prob[, ,im]*inout
  } 
  spi_prob[,,mese_aug2000:dim(spi)[3]]=NA
  save(spi_prob, file = paste0(dir_out, "/SPI_PROB", sc, "_DROP_1981_2020.RData"))
  save(spi_prob, file = paste0(dir_data, "/SPI_PROB", sc, "_DROP_1981_2020.RData"))
  
  ## traffic light
  spi_tl = array(data = NA, dim = c(length(lon), length(lat), dim(pred)[3]))
  for (im in 1:dim(pred)[3]) {
    for (i in 1:dim(pred)[1]) {
      for (j in 1:dim(pred)[2]) {
        aux = pred[i, j, im, ]
        if (sum(!is.na(aux))!=0) {
          if (sum(aux[!is.na(aux)] <= -1.3) / sum(!is.na(aux)) > 0 &
              sum(aux[!is.na(aux)] <= -1.3) / sum(!is.na(aux)) <= 0.25)  {
            spi_tl[i, j, im] = 2 #yellow code
          } else if (sum(aux[!is.na(aux)] <= -1.3) / sum(!is.na(aux)) > 0.25 &
                     sum(aux[!is.na(aux)] <= -1.3) / sum(!is.na(aux)) <= 0.75)  {
            spi_tl[i, j, im] = 3 #orange code
          } else if (sum(aux[!is.na(aux)] <= -1.3) / sum(!is.na(aux)) > 0.75)  {
            spi_tl[i, j, im] = 4 #red code
          } else  if (sum(aux[!is.na(aux)] > -1.30 &
                          aux[!is.na(aux)] <= -0.8) / sum(!is.na(aux)) > 0 &
                      sum(aux[!is.na(aux)] > -1.30 &
                          aux[!is.na(aux)] <= -0.8) / sum(!is.na(aux)) <= 0.5)  {
            spi_tl[i, j, im] = 2 #yellow code
          } else if (sum(aux[!is.na(aux)] > -1.30 &
                         aux[!is.na(aux)] <= -0.8) / sum(!is.na(aux)) > 0.5)  {
            spi_tl[i, j, im] = 3 #orange code
          } else if (sum(aux[!is.na(aux)] > -0.8 &
                         aux[!is.na(aux)] <= -0.5) / sum(!is.na(aux)) > 0.5)  {
            spi_tl[i, j, im] = 2 #yellow code
          } else  if (sum(aux[!is.na(aux)] > -0.8 &
                          aux[!is.na(aux)] <= -0.5) / sum(!is.na(aux)) > 0 &
                      sum(aux[!is.na(aux)] > -0 - 8 &
                          aux[!is.na(aux)] <= -0.5) / sum(!is.na(aux)) <= 0.5)  {
            spi_tl[i, j, im] = 1 #green code
          } else  {
            spi_tl[i, j, im] = 1 #green code
          }
        }
      }
    }
    spi_tl[, , im] = spi_tl[, , im] * inout
  } 
  spi_tl[,,mese_aug2000:dim(spi)[3]]=NA
  save(spi_tl, file = paste0(dir_out, "/SPI_TRAF_LIG_", sc, "_DROP_1981_2020.RData"))
  save(spi_tl, file = paste0(dir_data, "/SPI_TRAF_LIG_", sc, "_DROP_1981_2020.RData"))
}

#ni = length(lon)
#nj = length(lat)


#
# anno_for = which(anni == 2003)
# 
# image.plot(lon, lat, spi_prob[, , anno_for])
# plot(wrld_simpl, add = TRUE)
# 
# image.plot(lon, lat, spi_tl[, , anno_for])
# plot(wrld_simpl, add = TRUE)


# inout = ENS[, , anno_for] * 0
# image.plot(lon, lat, inout)
# plot(wrld_simpl, add = TRUE)
# 