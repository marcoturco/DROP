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

source("/Users/marco/Dropbox/estcena/scripts/BSC/R/common/ColorBarM.R")
source("/Users/marco/Dropbox/estcena/scripts/BSC/R/common/mioplot_global.R")
source("/Users/marco/Dropbox/estcena/scripts/BSC/R/common/CorrMIO.R")


## fixed parameters

time_scale = c(1, 3, 6, 12)
# time_scale = c(12)


dir_oss = '/Users/marco/Documents/dati/obs'
dir_out = '/Users/marco/Documents/output/obs_uncertainties'
noaaout <- "/Users/marco/Documents/dati/obs/GHCN"

#brk_corr <- seq(-1, 1, 0.2)
#col_corr <- (colorRampPalette(brewer.pal(11, "PRGn"))(10))

brk_cor <- seq(-1, 1, 0.2)
col_cor <- (colorRampPalette(brewer.pal(11, "PiYG"))(10))

anni = 1981:2016
#SON 36 ANNI
anniok = 32 # 33/37 ?? circa il 90%

load(file.path(dir_oss, "GPCPv2_3/lon_GPCP_1981_2017.RData"))
load(file.path(dir_oss, "GPCPv2_3/lat_GPCP_1981_2017.RData"))
lonGPCP = lon
latGPCP = lat

mesi = rep(1:12, length(anni))

# datasets = c('ENS','ERA5','CHIRPS','NCEP','MERRA2',
#              'CPC',
#              'PRECL',
#              'CAMS_OPI',
#              'GPCC',
#              'GPCP',
#              'JRA55')


datasets = c('ENS')

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
  
  if (sc==12) {
    mswep = mswep[, , seq(12, dim(mswep)[3], 12)]
  } else if (sc==6) {
    mswep = mswep[, , seq(6, dim(mswep)[3], 6)]
  } else if (sc==3) {
    mswep = mswep[, , seq(3, dim(mswep)[3], 3)]
  }
  mswep[is.infinite(mswep)]=NA
  for (idata in 1:length(datasets)) {
    #for (idata in 1:1) {
    dataset = datasets[idata]
    if (dataset == "ENS") {
      load(file.path(
        dir_out,
        paste("/SPI", sc, "_ENS_1981_2016.RData", sep = "")
      ))
    } else if (dataset == "JRA55") {
      load(file.path(
        paste(
          "/Users/marco/Documents/dati/obs/JRA55/SPI",
          sc,
          "_JRA55_1981_2016.RData",
          sep = ""
        )
      ))
    } else if (dataset == "GPCP") {
      load(file.path(
        dir_oss,
        paste("GPCPv2_3/SPI", sc, "_GPCP_1981_2016.RData", sep = "")
      ))
    } else if (dataset == "GPCC") {
      load(file.path(
        paste(
          "/Users/marco/Documents/dati/obs/GPCCv2018/SPI",
          sc,
          "_GPCC_1981_2016.RData",
          sep = ""
        )
      ))
    } else if (dataset == "GPCC") {
      load(file.path(
        paste(
          "/Users/marco/Documents/dati/obs/GPCCv2018/SPI",
          sc,
          "_GPCC_1981_2016.RData",
          sep = ""
        )
      ))
    } else if (dataset == "CAMS_OPI") {
      load(file.path(
        paste(
          "/Users/marco/Documents/dati/obs/CAMS_OPI/SPI",
          sc,
          "_CAMS_OPI_1981_2016.RData",
          sep = ""
        )
      ))
    } else if (dataset == "PRECL") {
      load(file.path(
        paste(
          "/Users/marco/Documents/dati/obs/PRECL/SPI",
          sc,
          "_PRECL_1981_2016.RData",
          sep = ""
        )
      ))
    } else if (dataset == "CPC") {
      load(file.path(
        paste(
          "/Users/marco/Documents/dati/obs/CPC_GLOBAL_PRECIP/SPI",
          sc,
          "_CPC_1981_2016.RData",
          sep = ""
        )
      ))
    } else if (dataset == "CHIRPS") {
      load(file.path(
        paste(
          "/Users/marco/Documents/dati/SPIF/data/SPI",
          sc,
          "_CHIRPS_1981_2016.RData",
          sep = ""
        )
      ))
    } else if (dataset == "ERA5") {
      load(file.path(
        paste(
          "/Users/marco/Documents/dati/obs/ERA5/SPI",
          sc,
          "_ERA5_1981_2016.RData",
          sep = ""
        )
      ))
    } else if (dataset == "NCEP") {
        load(file.path(
          paste(
            "/Users/marco/Documents/dati/obs/NCEP/SPI",
            sc,
            "_NCEP_1981_2016.RData",
            sep = ""
          )
        ))
    } else if (dataset == "MERRA2") {
      load(file.path(
        paste(
          "/Users/marco/Documents/dati/MERRA2/SPI",
          sc,
          "_MERRA2_1981_2016.RData",
          sep = ""
        )
      ))
    } else {
      print('dataset not known')
    }
    
    if (dataset == "ENS") {
      gpcp = spi
      rm(spi)
      
    } else {
      gpcp = get(nam)
    }
    
    
    if (sc==12) {
      gpcp = gpcp[, , seq(12, dim(gpcp)[3], 12)]
      
    } else if (sc==6) {
      gpcp = gpcp[, , seq(6, dim(gpcp)[3], 6)]
      
    } else if (sc==3) {
      gpcp = gpcp[, , seq(3, dim(gpcp)[3], 3)]
      
    }
    
    
    ni = dim(gpcp)[1]
    nj = dim(gpcp)[2]
    nt = dim(gpcp)[3]
    
    corre <- matrix(data = NA,
                    nrow = ni,
                    ncol = nj)
    pvalue <- matrix(data = NA,
                     nrow = ni,
                     ncol = nj)
    
    gpcp[is.infinite(gpcp)]=NA
    for (i in 1:ni) {
      for (j in 1:nj) {
        
        OK <- complete.cases(mswep[i, j,], gpcp[i, j,])
        x <- mswep[i, j, OK]
        y <- gpcp[i, j, OK]
        n <- length(x)
        #if (n >= anniok * 12) {
        if (n >= nt*0.9) {
          
          
          dum = CorrMIO((x), (y), method = 'pearson', pval = TRUE)
          corre[i, j] = as.numeric(dum)[1]
          pvalue[i, j] <- as.numeric(dum)[4]
          rm(dum)
        }
        rm(OK)
        rm(n)
        rm(x)
        rm(y)
      }
    }
    
    # pvalue_adj = p.adjust(pvalue, method = "fdr", n = length(pvalue[!is.na(pvalue)]))
    # pvalue_adj = matrix(pvalue_adj, nrow = ni, ncol = nj)
    # 
    lat2 = lat[which(lat > -60 & lat < 85)]
    corre2 = corre[, which(lat > -60 & lat < 85)]
    pvalue2 = pvalue[, which(lat > -60 & lat < 85)]
    # pvalue_adj2 = pvalue_adj[, which(lat > -60 & lat < 85)]
    
    ## plot corr
    postscript(
      file.path(
        dir_out,
        paste("cor_MSWEP_", dataset, "_annual_spi", sc, ".eps",
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
      drawleg = F,
      dots2 = pvalue2 <= 0.05#,
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
      paste("corre_spi_", sc, "_", dataset, "_MSWEP.RData", sep = "")
    ))
    
  }
}
