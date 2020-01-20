rm(list = ls())
graphics.off()
gc()

library(sf)
library(sp)
library(maptools) # loads sp library too
library(RColorBrewer) # creates nice color schemes
library(classInt) # finds class intervals for continuous variables
library(fields)
library(s2dverification)
library(maps)
library(rworldmap)

source("/Users/marco/Dropbox/estcena/scripts/BSC/R/common/ColorBarM.R")
source("/Users/marco/Dropbox/estcena/scripts/BSC/R/common/mioplot_global.R")
source("/Users/marco/Dropbox/estcena/scripts/BSC/R/common/CorrMIO.R")


## fixed parameters


dir_oss = '/Users/marco/Documents/dati/obs'
dir_out = '/Users/marco/Documents/output/obs_uncertainties'
noaaout <- "/Users/marco/Documents/dati/obs/GHCN"

# time_scale = c(1, 3, 6, 12)
time_scale = c(12)

# datasets = c('ENS','ERA5','CHIRPS','NCEP',
#              'CPC',
#              'PRECL',
#              'CAMS_OPI',
#              'GPCC',
#              'GPCP',
#              'JRA55',
#              'MERRA2',
#              'MSWEP'
# )
 
datasets = c('ENS')



############

brk_cor <- seq(-1, 1, 0.2)
col_cor <- (colorRampPalette(brewer.pal(11, "PiYG"))(10))

anni = 1981:2016
#SON 37 ANNI
anniok = 20 # 33/37 ?? circa il 90%

load(file.path(dir_oss, "GPCPv2_3/lon_GPCP_1981_2017.RData"))
load(file.path(dir_oss, "GPCPv2_3/lat_GPCP_1981_2017.RData"))
lonGPCP = lon
latGPCP = lat

data(wrld_simpl)
points <- expand.grid(lonGPCP, latGPCP)
pts = SpatialPoints(points, proj4string = CRS(proj4string(wrld_simpl)))


rootPath = "/Users/marco/Documents/dati/GSIM"

gsim = read.csv(file.path(rootPath, "GLOBAL_MEAN_1981_2016.csv"))
lon = gsim$longitude
lat = gsim$latitude

# mesi = rep(1:12, length(anni))
# anno_2000=which(anni==2011)
# mese_aug=which(mesi==9)
# mese_aug2000=mese_aug[anno_2000]

# #image.plot(lon,lat,spi12_ghcn[,,mese_aug2000])
# #plot(wrld_simpl,add=TRUE)
#plotvar <- spi12_ghcn[mese_aug2000,]
#plotvar <- spi12_ghcn[12,]
# plotvar = apply(spi12_ghcn, 2, sd, na.rm = TRUE)
# nclr <- 6
# plotclr <- brewer.pal(nclr, "BrBG")
# class <- classIntervals(plotvar,
#                         nclr,
#                         style = "fixed",
#                         fixedBreaks = c(-2, -0.5, -0.15, 0.15, 0.5, 2))
# colcode <- findColours(class, plotclr)
#
# newmap <- getMap(resolution = "low")
# plot(newmap, xlim = c(-180, 180), ylim = c(-60, 85))
# points(lon,
#        lat,
#        pch = 16,
#        col = colcode,
#        cex = 0.8)
# legend(
#   -160,
#   0,
#   legend = names(attr(colcode, "table")),
#   fill = attr(colcode, "palette"),
#   cex = 0.6,
#   bty = "n"
# )


#




for (isc in 1:length(time_scale)) {
  sc = time_scale[isc]
  
  nam <- paste("spi", sc, sep = "")
  print(nam)
  
  
  
  
  
  for (idata in 1:length(datasets)) {
    #for (idata in 1:1) {
    dataset = datasets[idata]
    if (dataset == "ENS") {
      load(file.path(
        dir_out,
        paste("/SPI", sc, "_ENS_1981_2016.RData", sep = "")
      ))
    } else if (dataset == "MSWEP") {
      load(file.path(
        dir_oss,
        paste("MSWEP/SPI", sc, "_MSWEP_1981_2016.RData", sep = "")
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
    } else {
      print('dataset not known')
    }
    
    #gpcp = spi12[, , seq(12, dim(spi12)[3], 12)]
    
    
    if (dataset == "ENS") {
      gpcp = spi
      rm(spi)
      
    } else {
      gpcp = get(nam)
    }
    
    
    if (sc == 12) {
      gpcp = gpcp[, , seq(12, dim(gpcp)[3], 12)]
      #ghcn = ghcn[, , seq(12, dim(ghcn)[3], 12)]
    } else if (sc == 6) {
      gpcp = gpcp[, , seq(6, dim(gpcp)[3], 6)]
      #ghcn = ghcn[, , seq(6, dim(ghcn)[3], 6)]
    } else if (sc == 3) {
      gpcp = gpcp[, , seq(3, dim(gpcp)[3], 3)]
      #ghcn = ghcn[, , seq(3, dim(ghcn)[3], 3)]
    }
    nt = dim(gpcp)[3]
    
    ## loop
    ni = dim(gpcp)[1]
    nj = dim(gpcp)[2]
    
    
    
    corre <- rep(NA, dim(gsim)[1])
    pvalue <- rep(NA, dim(gsim)[1])
    #                 nrow = ni,
    #                 ncol = nj)
    # pvalue <- matrix(data = NA,
    #                  nrow = ni,
    #                  ncol = nj)
    #
    #
    
    #ghcn = gpcp * NA
    k=0
    for (ibasin in 1:dim(gsim)[1]) {
      fileid = paste0(rootPath,
                      '/GSIM_metadata/GSIM_catchments/',
                      as.character(gsim$ID[ibasin]),
                      '.shp')
      
      # if(!file.exists(fileid)){
      #   next()
      #   }
      #   
      #   
      # if (is.na(gsim$ID[ibasin])) {
      #   next
      # }
      shp <- st_read(fileid)
      shp = shp[3]
      # plot(shp)
      aux1 = st_coordinates(shp)
      # points(points)
      # aux1=shp@polygons[[1]]@Polygons[[1]]@coords
      inout = point.in.polygon(points[, 1], points[, 2], aux1[, 1], aux1[, 2])
      dim(inout) <- c(nrow(gpcp), ncol(gpcp))
      inout[inout == 0] = NA
      
      if (sum(inout, na.rm = TRUE) > 0) {
        
        image.plot(lonGPCP, latGPCP, inout)
        plot(shp, add = TRUE,col="red")
        plot(wrld_simpl, add = TRUE)
        
        gpcp2 = gpcp
        gpcp_reg = vector()
        for (i in 1:dim(gpcp)[3]) {
          gpcp2[, , i] = gpcp[, , i] * inout
          gpcp_reg[i] = mean(gpcp2[, , i], na.rm = TRUE)
        }
        gpcp_reg = scale(gpcp_reg)
        
        ghcn = as.numeric(as.character(gsim[ibasin, 6:41]))
        
        OK <- complete.cases(gpcp_reg, ghcn)
        x <- gpcp_reg[OK]
        y <- ghcn[OK]
        n <- length(x)
        # if (n >= nt * 0.9) {
        if (n >= 32) {
          
          k=k+1
          print(k)
          
          dum = CorrMIO((x), (y), method = 'pearson', pval = TRUE)
          corre[ibasin] = as.numeric(dum)[1]
          pvalue[ibasin] <- as.numeric(dum)[4]
          rm(dum)
        }
      }
    }
    
    # pvalue_adj = p.adjust(pvalue, method = "fdr", n = length(pvalue[!is.na(pvalue)]))
    # pvalue_adj = matrix(pvalue_adj, nrow = ni, ncol = nj)
    #
    # image.plot(lon,lat,corre)
    # plot(wrld_simpl,add=TRUE)
    #
    
    
    # lat2 = latGPCP[which(latGPCP > -60 & latGPCP < 85)]
    # corre2 = corre[, which(latGPCP > -60 & latGPCP < 85)]
    # pvalue2 = pvalue[, which(latGPCP > -60 & latGPCP < 85)]
    # pvalue_adj2 = pvalue_adj[, which(latGPCP > -60 & latGPCP < 85)]
    
    #image.plot(lon,lat,spi12_ghcn[,,mese_aug2000])
    #plot(wrld_simpl,add=TRUE)
    corre2 = corre
    # corre2[pvalue_adj > 0.05] = NA
    corre2[pvalue > 0.05] = NA
    
    
    
    ilat = which(lat > -60 & lat < 85)
    corre2 = corre2[ilat]
    lat2 = lat[ilat]
    lon2 = lon[ilat]
    plotvar <- corre2
    #nclr <- 6
    #plotclr <- brewer.pal(nclr, "BrBG")
    library(RColorBrewer) # creates nice color schemes
    brk_cor <- seq(1, -1, -0.2)
    col_cor <- (colorRampPalette(brewer.pal(11, "PiYG"))(10))
    
    
    class <- classIntervals(plotvar,
                            #nclr,
                            length(brk_cor),
                            style = "fixed",
                            #fixedBreaks = c(-1, -0.5, -0.15, 0.15, 0.5, 1))
                            fixedBreaks = brk_cor)
    #colcode <- findColours(class, plotclr)
    colcode <- findColours(class, col_cor)
    newmap <- getMap(resolution = "low")
    
    setEPS()
    postscript(file.path(
      dir_out,
      paste(
        "cor_",
        dataset,
        "_gsim_annual_spi",
        sc,
        "_station.eps",
        sep = ""
      )
    ),
    width = 6,
    height = 4)
    par(mar = c(4, 4, 3, 1))
    
    
    plot(
      newmap,
      xlim = c(-180, 180),
      ylim = c(-60, 85),
      asp = 1
    )
    points(lon2,
           lat2,
           pch = 16,
           col = colcode,
           cex = 0.4)
    legend(
      -170,
      30,
      legend = names(attr(colcode, "table")),
      fill = attr(colcode, "palette"),
      cex = 0.6,
      bty = "n"
    )
    dev.off()
    
    save(corre, file = file.path(
      dir_out,
      paste("corre_spi_", sc, "_", dataset, "_gsim_station.RData", sep = "")
    ))
  }
}