rm(list = ls())
graphics.off()
gc()


library(sp)
library(maptools) # loads sp library too
library(RColorBrewer) # creates nice color schemes
library(classInt) # finds class intervals for continuous variables
library(fields)
library(maps)
library(s2dverification)
source("/Users/marco/Dropbox/estcena/scripts/BSC/R/common/ColorBarM.R")


## fix parameters
dir_oss = '/Users/marco/Documents/dati/obs'
dir_out = '/Users/marco/Documents/output/obs_uncertainties'


datasets = c('GPCP',
             'CAMS_OPI',
             'CHIRPS',
             'CPC',
             'GPCC',
             'JRA55',
             'PRECL',
             'ERA5',
             'NCEP',
             'MERRA2')


# casi = c('ASIA',
#          'Australia',
#          'USA',
#          'AMAZONIA',
#          'AFRICA',
#          'EUROPE')

casi = c('AFRICA','USA')

for (icaso in 1:length(casi)) {
  caso = casi[icaso]
  
  
  if (caso == "Australia") {
    # 2006-12 Australia
    #http://www.abs.gov.au/ausstats/abs@.nsf/a9ca4374ed453c6bca2570dd007ce0a4/ccc8ead2792bc3c7ca2573d200106bde!OpenDocument
    lon1 = 110
    lon2 = 155
    lat1 = -45
    lat2 = -10
    anno_case = 2006
  } else if (caso == "USA") {
    lon1 = -170
    lon2 = -45
    lat1 = 5
    lat2 = 85
    anno_case = 2012
    lon_pt=-101.25
    lat_pt=41.25
    
  } else if (caso == "AMAZONIA") {
    lon1 = -90
    lon2 = -30
    lat1 = -60
    lat2 = 20
    anno_case = 2015
  } else if (caso == "AFRICA") {
    lon1 = -20
    lon2 = 55
    lat1 = -40
    lat2 = 40
    anno_case = 1984
    lon_pt=26.25
    lat_pt=13.75
  } else if (caso == "EUROPE") {
    lon1 = -15
    lon2 = 60
    lat1 = 30
    lat2 = 75
    anno_case = 2003
  } else if (caso == "ASIA") {
    lon1 = 35
    lon2 = 145
    lat1 = 0
    lat2 = 80
    anno_case = 2011
    
  } else {
    print('dataset not known')
  }
  
  
  
  data(wrld_simpl)
  
  anni = 1981:2015
  mesi = rep(1:12, length(anni))
  
  
  ## load dati
  load(file.path(
    dir_oss,
    #paste("GPCPv2_3/SPI12_GPCP_1981_2017.RData", sep = "")
    paste("MSWEP/SPI12_MSWEP_1981_2016.RData", sep = "")
  ))
  #dum = get(tolower(extreme_index))
  #erai = dum[, , seq(mese, length(anni_rean) * 12, 12)]
  #rm(dum)
  obs = spi12[, , seq(12, dim(spi12)[3], 12)]
  
  load(file.path(dir_oss, "GPCPv2_3/lon_GPCP_1981_2017.RData"))
  load(file.path(dir_oss, "GPCPv2_3/lat_GPCP_1981_2017.RData"))
  
  ## load data
  pred = array(data = NA, dim = c(dim(spi12)[1], dim(spi12)[2], dim(obs)[3], 10))
  load(file.path(
    paste(dir_oss, "/GPCPv2_3/SPI12_GPCP_1981_2016.RData", sep = "")
  ))
  pred[, , , 1] = spi12[, , seq(12, dim(spi12)[3], 12)]
  
  load(file.path(
    paste(dir_oss, "/CAMS_OPI/SPI12_CAMS_OPI_1981_2016.RData", sep = "")
  ))
  pred[, , , 2] = spi12[, , seq(12, dim(spi12)[3], 12)]
  
  load(file.path(
    paste(
      "/Users/marco/Documents/dati/SPIF/data/SPI12_CHIRPS_1981_2016.RData",
      sep = ""
    )
  ))
  pred[, , , 3] = spi12[, , seq(12, dim(spi12)[3], 12)]
  
  load(file.path(
    paste(
      dir_oss,
      "/CPC_GLOBAL_PRECIP/SPI12_CPC_1981_2016.RData",
      sep = ""
    )
  ))
  pred[, , , 4] = spi12[, , seq(12, dim(spi12)[3], 12)]
  
  load(file.path(
    paste(dir_oss, "/GPCCv2018/SPI12_GPCC_1981_2016.RData", sep = "")
  ))
  pred[, , , 5] = spi12[, , seq(12, dim(spi12)[3], 12)]
  
  load(file.path(
    paste(dir_oss, "/JRA55/SPI12_JRA55_1981_2016.RData", sep = "")
  ))
  pred[, , , 6] = spi12[, , seq(12, dim(spi12)[3], 12)]
  
  load(file.path(
    paste(dir_oss, "/PRECL/SPI12_PRECL_1981_2016.RData", sep = "")
  ))
  pred[, , , 7] = spi12[, , seq(12, dim(spi12)[3], 12)]
  
  load(file.path(paste(
    dir_oss, "/ERA5/SPI12_ERA5_1981_2016.RData", sep = ""
  )))
  pred[, , , 8] = spi12[, , seq(12, dim(spi12)[3], 12)]
  
  load(file.path(paste(
    dir_oss, "/NCEP/SPI12_NCEP_1981_2016.RData", sep = ""
  )))
  pred[, , , 9] = spi12[, , seq(12, dim(spi12)[3], 12)]
  
  load(file.path(
    paste(
      "/Users/marco/Documents/dati/MERRA2/SPI12_MERRA2_1981_2016.RData",
      sep = ""
    )))
  pred[, , , 10] = spi12[, , seq(12, dim(spi12)[3], 12)]
  
  
  load(file.path(
    dir_out,
    paste("/SPI12_ENS_1981_2016.RData", sep = "")
  ))
  
  
  ni = length(lon)
  nj = length(lat)
  
  
  
  ## select case study
  ilon = which(lon > lon1 & lon < lon2)
  ilat = which(lat > lat1 & lat < lat2)
  
  
  anno_for = which(anni == anno_case)
  
  image.plot(lon[ilon], lat[ilat], obs[ilon, ilat , anno_for])
  plot(wrld_simpl, add = TRUE)
  
  
  
  
  sid = obs[ilon, ilat , anno_for]
  lonsid = lon[ilon]
  latsid = lat[ilat]
  idx = which(sid[, ] == min(sid, na.rm = TRUE), arr.ind = T)
  lonsid[idx[1, 1]]
  latsid[idx[1, 2]]
  sid[idx[1, 1], idx[1, 2]] = -5
  image.plot(lon[ilon], lat[ilat], sid)
  plot(wrld_simpl, add = TRUE)
  
  
  
  spei6obs = obs[ilon, ilat, anno_for]
  obs_drought = spei6obs*NA
  obs_drought[(spei6obs > -0.5)] = 0
  obs_drought[(spei6obs <= -0.5) & (spei6obs > -0.8)] = 1
  obs_drought[(spei6obs <= -0.8) & (spei6obs > -1.3)] = 2
  obs_drought[(spei6obs <= -1.3) & (spei6obs > -1.6)] = 3
  obs_drought[(spei6obs <= -1.6) & (spei6obs > -2)] = 4
  obs_drought[(spei6obs <= -2)] = 5
  
  brk <- seq(-1, 5, length.out = 7)
  pal.1 = colorRampPalette(c("yellow", "red", "black"), space = "rgb")
  col = pal.1(length(brk) - 1)
  col[1] = "#C0C0C0"
  

  
  image.plot(lon[ilon], lat[ilat], obs_drought,zlim=c(-1,5))
  plot(wrld_simpl, add = TRUE)
  
  ## plot obs level
  postscript(
    file.path(
      dir_out,
      paste("SPI12_obs_", anno_case, "_level_", caso, ".eps", sep = "")
    ),
    paper = "special",
    width = 11,
    height = 7,
    horizontal = T
  )
  layout(matrix(c(1, 2), 1, 2, byrow = T), widths = c(11, 1.5))
  par(oma = c(1, 1, 4, 1))
  tit <- ('')
  PlotEquiMap(
    obs_drought,
    lon[ilon],
    lat[ilat],
    toptitle = '',
    sizetit = 0.8,
    brks = brk,
    cols = col,
    axelab =
      F,
    filled.continents = FALSE,
    drawleg = F,
    colNA = "white"
  )
  title(tit, line = 0.5, outer = T)
  ce = 1.4
  ColorBar(
    brks = brk,
    cols = col,
    vert = T,
    cex = 1
  )
  dev.off()
  
  
  ## plot obs spi
  brk_new = c(-2,-1.6,-0.8,-0.5, 0, 0.5, 0.8, 1.3, 2)
  brk2 = union(-1e+05, brk_new)
  brk2 = union(brk2, 1e+05)
  col <- (colorRampPalette(brewer.pal(11, "BrBG"))(10))
  
  image.plot(lon[ilon], lat[ilat], spei6obs)
  plot(wrld_simpl, add = TRUE)
  
  postscript(
    file.path(
      dir_out,
      paste("spi12_obs_", anno_case, "_", caso, ".eps", sep = "")
    ),
    paper = "special",
    width = 11,
    height = 7,
    horizontal = T
  )
  layout(matrix(c(1, 2), 1, 2, byrow = T), widths = c(11, 1.5))
  par(oma = c(1, 1, 4, 1))
  tit <- ('')
  PlotEquiMap(
    spei6obs,
    lon[ilon],
    lat[ilat],
    toptitle = '',
    sizetit = 0.8,
    brks = brk2,
    cols = col,
    axelab =
      F,
    filled.continents = FALSE,
    drawleg = F,
    colNA = "white"
  )
  title(tit, line = 0.5, outer = T)
  ce = 1.4
  ColorBarM(
    brks = brk2,
    cols = col,
    vert = T,
    cex = 1,
    labs = seq(2, length(brk2) - 1, 1)
  )
  dev.off()
  
  
  ## Plot Probability Moderate Drought
  spei6espfor = pred[ilon, ilat, anno_for,]
  for_drought_prob = array(data = NA, dim = c(dim(spei6espfor)[1], dim(spei6espfor)[2]))
  for (i in 1:dim(spei6espfor)[1]) {
    for (j in 1:dim(spei6espfor)[2]) {
      aux = spei6espfor[i, j,]
      for_drought_prob[i, j] = sum(aux[!is.na(aux)] <= -0.8) / length(!is.na(aux))
      
    }
  }
  
  points <- expand.grid(lon[ilon], lat[ilat])
  data(wrld_simpl)
  pts = SpatialPoints(points, proj4string = CRS(proj4string(wrld_simpl)))
  ii <- !is.na(over(pts, wrld_simpl))
  inout = ii[, 1]
  dim(inout) <- c(nrow(obs[ilon, ilat,]), ncol(obs[ilon, ilat,]))
  inout[inout == 0] = NA
  for_drought_prob = inout * for_drought_prob
  
  image.plot(lon[ilon], lat[ilat], for_drought_prob)
  plot(wrld_simpl, add = TRUE)
  
  
  brk_prob <- seq(0, 1, length.out = 6)
  #pal.1=colorRampPalette(c("yellow","red","black"), space="rgb")
  #col_prob=pal.1(length(brk_prob)-1)
  col_prob <-
    (colorRampPalette(brewer.pal(length(brk_prob), "YlOrBr"))(length(brk_prob) -
                                                                1))
  col_prob[1] = "#C0C0C0"
  
  for_drought_prob[for_drought_prob == 0] = 0.1
  
  postscript(
    file.path(
      dir_out,
      paste("SPI12_prob_", anno_case, "_", caso, ".eps", sep = "")
    ),
    paper = "special",
    width = 11,
    height = 7,
    horizontal = T
  )
  layout(matrix(c(1, 2), 1, 2, byrow = T), widths = c(11, 1.5))
  par(oma = c(1, 1, 4, 1))
  tit <- ('')
  PlotEquiMap(
    for_drought_prob,
    lon[ilon],
    lat[ilat],
    toptitle = '',
    sizetit = 0.8,
    brks = brk_prob,
    cols = col_prob,
    axelab =
      F,
    colNA = "white",
    filled.continents = FALSE,
    drawleg = F
  )
  title(tit, line = 0.5, outer = T)
  ce = 1.4
  ColorBar(
    brks = brk_prob,
    cols = col_prob,
    vert = T,
    cex = 1
  )
  dev.off()
  
  ## Plot ensemble mean
  aux = spi[ilon, ilat, seq(12, dim(spi12)[3], 12)]
  mean_pred=aux[,,anno_for]
  # mean_pred = spi(spei6espfor, c(1, 2), mean, na.rm = TRUE))

  pvalue=mean_pred*NA
  pvalue[lon_pt==lon[ilon],lat_pt==lat[ilat]]=0
  brk_new = c(-2,-1.6,-0.8,-0.5, 0, 0.5, 0.8, 1.3, 2)
  brk2 = union(-1e+05, brk_new)
  brk2 = union(brk2, 1e+05)
  col <- (colorRampPalette(brewer.pal(11, "BrBG"))(10))
  
  image.plot(lon[ilon], lat[ilat], spei6obs)
  plot(wrld_simpl, add = TRUE)
  
  postscript(
    file.path(
      dir_out,
      paste("spi12_ens_mean_", anno_case, "_", caso, ".eps", sep = "")
    ),
    paper = "special",
    width = 11,
    height = 7,
    horizontal = T
  )
  layout(matrix(c(1, 2), 1, 2, byrow = T), widths = c(11, 1.5))
  par(oma = c(1, 1, 4, 1))
  tit <- ('')
  PlotEquiMap(
    mean_pred,
    lon[ilon],
    lat[ilat],
    toptitle = '',
    sizetit = 0.8,
    brks = brk2,
    cols = col,
    axelab =
      F,
    labW = F,
    filled.continents = FALSE,
    drawleg = F,
    colNA = "white",
    dots = pvalue <= 0.05
  )
  title(tit, line = 0.5, outer = T)
  ce = 1.4
  ColorBarM(
    brks = brk2,
    cols = col,
    vert = T,
    cex = 1,
    labs = seq(2, length(brk2) - 1, 1)
  )
  
  dev.off()
  
  
  ## Plot spread
  sd_pred = apply(spei6espfor, c(1, 2), sd, na.rm = TRUE)
  sd_pred[sd_pred>=2]=2
 
  cols_sd <- brewer.pal(5, "BuPu")
  brk_sd <- seq(0, 2, length.out = 6)
  
  
  image.plot(lon[ilon], lat[ilat], sd_pred)
  plot(wrld_simpl, add = TRUE)
  
  
  postscript(
    file.path(
      dir_out,
      paste("spi12_ens_spread_", anno_case, "_", caso, ".eps", sep = "")
    ),
    paper = "special",
    width = 11,
    height = 7,
    horizontal = T
  )
  layout(matrix(c(1, 2), 1, 2, byrow = T), widths = c(11, 1.5))
  par(oma = c(1, 1, 4, 1))
  tit <- ('')
  PlotEquiMap(
    sd_pred,
    lon[ilon],
    lat[ilat],
    toptitle = '',
    sizetit = 0.8,
    brks = brk_sd,
    cols = cols_sd,
    axelab =
      F,
    labW = F,
    filled.continents = FALSE,
    drawleg = F,
    colNA = "white"
  )
  title(tit, line = 0.5, outer = T)
  ce = 1.4
  ColorBarM(
    brks = brk_sd,
    cols = cols_sd,
    vert = T,
    cex = 1,
    labs = seq(2, length(brk2) - 1, 1)
  )
  dev.off()
  
  
  ## warning level
  load(paste0(dir_out, "/SPI_TRAF_LIG_12_DROP_1981_2016.RData"))
  aux = spi_tl[ilon, ilat, seq(12, dim(spi12)[3], 12)]
  aux=aux[,,anno_for]
  image.plot(lon[ilon], lat[ilat], aux)
  plot(wrld_simpl, add = TRUE)
  cols_tl=c('#FFFF00','#FFA500','#FF0000')
  brk_tl <- seq(1, 4, length.out = 4)

  postscript(
    file.path(
      dir_out,
      paste("SPI12_warning_level_", anno_case, "_", caso, ".eps", sep = "")
    ),
    paper = "special",
    width = 11,
    height = 7,
    horizontal = T
  )
  layout(matrix(c(1, 2), 1, 2, byrow = T), widths = c(11, 1.5))
  par(oma = c(1, 1, 4, 1))
  tit <- ('')
  PlotEquiMap(
    aux,
    lon[ilon],
    lat[ilat],
    toptitle = '',
    sizetit = 0.8,
    brks = brk_tl,
    cols = cols_tl,
    axelab =
      F,
    colNA = "white",
    labW = F,
    filled.continents = FALSE,
    drawleg = F
  )
  title(tit, line = 0.5, outer = T)
  ce = 1.4
  ColorBar(
    brks = brk_tl,
    cols = cols_tl,
    vert = T,
    cex = 1
  )
  dev.off()
  
  
  
  
  
  
  for (idata in 1:length(datasets)) {
    #for (idata in 1:1) {
    dataset = datasets[idata]
    
    
    #spei6espfor = pred[ilon, ilat, anno_for, ]
    mean_pred = pred[ilon, ilat, anno_for, idata]
    
    
    image.plot(lon[ilon], lat[ilat], mean_pred)
    plot(wrld_simpl, add = TRUE)
    
    postscript(
      file.path(
        dir_out,
        paste(
          "spi12_ens_mean_",
          anno_case,
          "_",
          caso,
          "_",
          dataset,
          ".eps",
          sep = ""
        )
      ),
      paper = "special",
      width = 11,
      height = 7,
      horizontal = T
    )
    layout(matrix(c(1, 2), 1, 2, byrow = T), widths = c(11, 1.5))
    par(oma = c(1, 1, 4, 1))
    tit <- ('')
    PlotEquiMap(
      mean_pred,
      lon[ilon],
      lat[ilat],
      toptitle = '',
      sizetit = 0.8,
      brks = brk2,
      cols = col,
      axelab =
        F,
      labW = F,
      filled.continents = FALSE,
      drawleg = F,
      colNA = "white"
    )
    title(tit, line = 0.5, outer = T)
    ce = 1.4
    ColorBarM(
      brks = brk2,
      cols = col,
      vert = T,
      cex = 1,
      labs = seq(2, length(brk2) - 1, 1)
    )
    dev.off()
    
  }
  
  
  
}
