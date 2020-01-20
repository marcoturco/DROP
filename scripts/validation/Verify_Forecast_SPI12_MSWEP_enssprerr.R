rm(list = ls())
graphics.off()
gc()

#library(ncdf)
library(sp)
library(maptools) # loads sp library too
library(RColorBrewer) # creates nice color schemes
library(classInt) # finds class intervals for continuous variables
library(fields)
library(s2dverification)
library(maps)
library(easyVerification)

source("/Users/marco/Dropbox/estcena/scripts/BSC/R/common/ColorBarM.R")
source("/Users/marco/Dropbox/estcena/scripts/BSC/R/common/mioplot_global.R")
source("/Users/marco/Dropbox/estcena/scripts/BSC/R/common/CorrMIO.R")


## fixed parameters
data(wrld_simpl)
# time_scale = c(1, 3, 6, 12)
time_scale = c(12)


dir_oss = '/Users/marco/Documents/dati/obs'
dir_esa = '/Users/marco/Documents/dati/esacci_sm_v045/ESACCI-SOILMOISTURE-L3S-SSMV-COMBINED_1978-2018-v04.5'
dir_out = '/Users/marco/Documents/output/obs_uncertainties'
noaaout <- "/Users/marco/Documents/dati/obs/GHCN"

#brk_corr <- seq(-1, 1, 0.2)
#col_corr <- (colorRampPalette(brewer.pal(11, "PRGn"))(10))

brk_cor <- seq(0, 2, 0.2)
col_cor <- (colorRampPalette(brewer.pal(11, "PiYG"))(10))

anni = 1981:2016
#SON 36 ANNI
anniok = 32 # 33/37 ?? circa il 90%

load(file.path(dir_oss, "GPCPv2_3/lon_GPCP_1981_2017.RData"))
load(file.path(dir_oss, "GPCPv2_3/lat_GPCP_1981_2017.RData"))
lonGPCP = lon
latGPCP = lat

mesi = rep(1:12, length(anni))

datasets = c(
  'ENS',
  'ERA5',
  'CHIRPS',
  'NCEP',
  'CPC',
  'PRECL',
  'CAMS_OPI',
  'GPCC',
  'GPCP',
  'JRA55',
  'MERRA2',
  'MSWEP'
)


# datasets = c('MERRA2')

## load gpcp

for (isc in 1:length(time_scale)) {
  sc = time_scale[isc]
  
  nam <- paste("spi", sc, sep = "")
  print(nam)
  
  load(file.path(
    dir_oss,
    paste("MSWEP/SPI", sc, "_MSWEP_1981_2016.RData", sep = "")
  ))
  
  mswep = get(nam)
  image.plot(lon,lat,apply(mswep,c(1,2),mean,na.rm=TRUE))
  plot(wrld_simpl, add = TRUE)
  points(-121.25,41.25,cex = 1.5)

  
  if (sc==12) {
    mswep = mswep[, , seq(12, dim(mswep)[3], 12)]
  } else if (sc==6) {
    mswep = mswep[, , seq(6, dim(mswep)[3], 6)]
  } else if (sc==3) {
    mswep = mswep[, , seq(3, dim(mswep)[3], 3)]
  }
  mswep[is.infinite(mswep)]=NA
  
  gpcp = mswep
  rm(mswep)
  
  load(file.path(dir_esa, "ESACCI-SM-1981-2016.RData"))
  
  for (i in 1:dim(obs)[1]) {
    for (j in 1:dim(obs)[2]) {
      obs[i, j, ] = scale(obs[i, j, ])
    }
  }
  esa=obs
  rm(obs)

  
  
  # grace=obs
  # rm(obs)
  

  
  ## load data
  pred = array(data = NA, dim = c(dim(gpcp)[1], dim(gpcp)[2], length(mesi), 10))
  
  load(file.path(
    paste(dir_oss, "/GPCPv2_3/SPI", sc, "_GPCP_1981_2016.RData", sep = "")
  ))
  aux = get(nam)
  pred[, , , 1] = aux[, ,]
  
  load(file.path(
    paste(
      dir_oss,
      "/CAMS_OPI/SPI",
      sc,
      "_CAMS_OPI_1981_2016.RData",
      sep = ""
    )
  ))
  aux = get(nam)
  pred[, , , 2] = aux[, ,]
  
  load(file.path(
    paste(
      "/Users/marco/Documents/dati/SPIF/data/SPI",
      sc,
      "_CHIRPS_1981_2016.RData",
      sep = ""
    )
  ))
  aux = get(nam)
  pred[, , , 3] = aux[, ,]
  
  
  load(file.path(
    paste(
      dir_oss,
      "/CPC_GLOBAL_PRECIP/SPI",
      sc,
      "_CPC_1981_2016.RData",
      sep = ""
    )
  ))
  aux = get(nam)
  pred[, , , 4] = aux[, ,]
  
  load(file.path(
    paste(dir_oss, "/GPCCv2018/SPI", sc, "_GPCC_1981_2016.RData", sep = "")
  ))
  aux = get(nam)
  pred[, , , 5] = aux[, ,]
  
  load(file.path(
    paste(dir_oss, "/JRA55/SPI", sc, "_JRA55_1981_2016.RData", sep = "")
  ))
  aux = get(nam)
  pred[, , , 6] = aux[, ,]
  
  load(file.path(
    paste(dir_oss, "/PRECL/SPI", sc, "_PRECL_1981_2016.RData", sep = "")
  ))
  aux = get(nam)
  pred[, , , 7] = aux[, ,]
  
  load(file.path(
    paste(dir_oss, "/ERA5/SPI", sc, "_ERA5_1981_2016.RData", sep = "")
  ))
  aux = get(nam)
  pred[, , , 8] = aux[, ,]
  
  load(file.path(
    paste(dir_oss, "/NCEP/SPI", sc, "_NCEP_1981_2016.RData", sep = "")
  ))
  aux = get(nam)
  pred[, , , 9] = aux[, ,]
  
  load(file.path(
    paste(
      "/Users/marco/Documents/dati/MERRA2/SPI",
      sc,
      "_MERRA2_1981_2016.RData",
      sep = ""
    )
  ))
  aux = get(nam)
  pred[, , , 10] = aux[, ,]
  
  # pred=pred[,,((2001-1981+1)*12+1):dim(pred)[3],]
  
  pred[is.infinite(pred)] = NA
  
  
  
  if (sc == 12) {
    # obs = obs[, , seq(12, dim(obs)[3], 12)]
    pred = pred[, , seq(12, dim(pred)[3], 12), ]
  } else if (sc == 6) {
    # obs = obs[, , seq(6, dim(obs)[3], 6)]
    pred = pred[, , seq(6, dim(pred)[3], 6), ]
  } else if (sc == 3) {
    # obs = obs[, , seq(3, dim(obs)[3], 3)]
    pred = pred[, , seq(3, dim(pred)[3], 3), ]
  }
  
  
  
  
  # if (sc == 12) {
  #   gpcp = gpcp[, , seq(12, dim(gpcp)[3], 12)]
  #   
  # } else if (sc == 6) {
  #   gpcp = gpcp[, , seq(6, dim(gpcp)[3], 6)]
  #   
  # } else if (sc == 3) {
  #   gpcp = gpcp[, , seq(3, dim(gpcp)[3], 3)]
  #   
  # }
  
  # # gpcp only from 2002/04 --> i eliminated (2001-1981+1)*12
  # gpcp=gpcp[,,((2001-1981+1)*12+1):dim(gpcp)[3]]
  
  ni = dim(gpcp)[1]
  nj = dim(gpcp)[2]
  nt = dim(gpcp)[3]
  
  corre <- matrix(data = NA,
                  nrow = ni,
                  ncol = nj)
  
  
  gpcp[is.infinite(gpcp)] = NA
  for (i in 1:ni) {
    for (j in 1:nj) {
      # OK <- complete.cases(grace[i, j, ], gpcp[i, j, ])
      # x <- grace[i, j, OK]
      # y <- gpcp[i, j, OK]
      # n <- length(x)
      # 
      # 
      # #if (n >= anniok * 12) {
      # if (n >= nt * 0.9) {
      if (length(which(is.na(gpcp[i, j, ])))!=length(anni)) {
        ino=which(is.na(pred[i, j, 1, ]))
        # print(sum(ino))
        if (sum(ino) == 0) {
          pred2=pred[i, j, , ]
        } else {pred2=pred[i, j, ,-ino ]
        }
          
        
        # if (lon[i]==131.25 & lat[j]==-26.25) {
       
        if (lon[i]== -118.75 & lat[j]==41.25) {
          image.plot(lon, lat, gpcp[,,1])
          plot(wrld_simpl, add = TRUE)
          points(lon[i],lat[j],cex = 1.5)
          
          
          
          plot(ts(gpcp[i,j,]),ylim = c(-3, 3), col = "black")
          for (iens in 1:10) {
          lines(pred[i, j, ,iens],col='gray')
          }
          lines(gpcp[i,j,],col='black')
          lines(esa[i,j,],col='red')
          
          cc
          
          obs=as.vector(gpcp[i, j, ])
          ens=pred2
          xmask <- apply(!is.na(ens), 1, any) & !is.na(obs)
          
          spread <- mean(apply(ens[xmask, , drop = F], 1, sd, na.rm = T)^2, 
                         na.rm = T)
          error <- mean((obs - rowMeans(ens))^2, na.rm = T)
          sqrt(spread/error)
          
          
          aux = EnsSprErr(pred2,
                          as.vector(gpcp[i, j, ]))
          
          cc}
        
        
        
        
        aux = EnsSprErr(pred2,
                        as.vector(gpcp[i, j, ]))

                # aux = FairSprErr(pred2,
        #                 as.vector(gpcp[i, j, ]))
        
        
        
        corre[i, j] = aux
        rm(aux)
      }
    }
  }
  
  lat2 = lat[which(lat > -60 & lat < 85)]
  corre2 = corre[, which(lat > -60 & lat < 85)]
  corre2[corre2<0]=0
  corre2[corre2>2]=2
  ## plot corr
  postscript(
    file.path(
      dir_out,
      paste("EnsSprErr_MSWEP_annual_spi", sc, ".eps",
            sep = "")
    ),
    paper = "special",
    width = 11,
    height = 7,
    horizontal = T
  )
  
  layout(matrix(c(1, 2), 1, 2, byrow = T), widths = c(11, 1.5))
  par(oma = c(1, 1, 4, 1))
  tit <-
    paste('')
  
  
  mioplot(
    corre2,
    lon,
    lat2,
    toptitle = '',
    sizetit = 0.8,
    brks = brk_cor,
    cols = col_cor,
    axelab =
      F,
    filled.continents = FALSE,
    drawleg = F
    # dots2 = pvalue2 <= 0.05#,
    #dots2 = pvalue_adj2 <= 0.05
  )
  
  title(tit, line = 0.5, outer = T)
  ce = 1.4
  ColorBar(
    brks = brk_cor,
    cols = col_cor,
    vert = T,
    cex = 1
  )
  dev.off()
  
  save(corre, file = file.path(
    dir_out,
    paste("EnsSprErr_spi_", sc, "_MSWEP.RData", sep = "")
  ))
  
}

