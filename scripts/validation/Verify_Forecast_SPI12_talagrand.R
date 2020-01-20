rm(list = ls())
graphics.off()
gc()

library(SpecsVerification)

##

time_scale = c(12)
## loop soglie, start dates, zone



#time_scale = c(1, 3, 6, 12)



## fix parameters
dir_oss = '/Users/marco/Documents/dati/obs'
dir_out = "/Users/marco/Documents/output/obs_uncertainties"


load(file.path(dir_oss, "GPCPv2_3/lon_GPCP_1981_2017.RData"))
load(file.path(dir_oss, "GPCPv2_3/lat_GPCP_1981_2017.RData"))
ni = length(lon)
nj = length(lat)

## zones
regions = c(
  'Australia' ,
  'Amazon Basin' ,
  'Southern South America' ,
  'Central America' ,
  'Western North America' ,
  'Central North America',
  'Eastern North America' ,
  'Alaska' ,
  'Greenland' ,
  'Mediterranean Basin' ,
  'Northern Europe' ,
  'Western Africa' ,
  'Eastern Africa',
  'Southern Africa' ,
  'Sahara' ,
  'Southeast Asia' ,
  'East Asia' ,
  'South Asia' ,
  'Central Asia' ,
  'Tibet' ,
  'North Asia'
)

reg = c(
  'AUS' ,
  'AMZ' ,
  'SS' ,
  'CAM' ,
  'WNA' ,
  'CNA' ,
  'ENA' ,
  'ALA' ,
  'GRL' ,
  'MED' ,
  'NEU' ,
  'WAF' ,
  'EAF' ,
  'SAF' ,
  'SAH' ,
  'SE' ,
  'EAS' ,
  'SAS' ,
  'CAS' ,
  'TIB' ,
  'NAS'
)

coordreg = matrix(
  c(
    -45,
    -11,
    110,
    155,-20,
    12,
    -82,
    -34,-56,
    -20,
    -76,
    -40,
    10,
    30,
    -116,
    -83,
    30,
    60,
    -130,
    -103,
    30,
    50,
    -103,
    -85,
    25,
    50,
    -85,
    -60,
    60,
    72,
    -170,
    -103,
    50,
    85,
    -103,
    -10,
    30,
    48,
    -10,
    40,
    48,
    75,
    -10,
    40,-12,
    18,
    -20,
    22,-12,
    18,
    22,
    52,-35,
    -12,
    -10,
    52,
    18,
    30,
    -20,
    65,-11,
    20,
    95,
    155,
    20,
    50,
    100,
    145,
    5,
    30,
    65,
    100,
    30,
    50,
    40,
    75,
    30,
    50,
    75,
    100,
    50,
    70,
    40,
    180
  ),
  nrow = length(reg),
  ncol = 4,
  byrow = TRUE
)






for (isc in 1:length(time_scale)) {
  sc = time_scale[isc]
  
  nam <- paste("spi", sc, sep = "")
  print(nam)
  
  load(file.path(
    dir_oss,
    paste("MSWEP/SPI", sc, "_MSWEP_1981_2016.RData", sep = "")
  ))
  obs = get(nam)
  
  obs[is.infinite(obs)] = NA
  
  ## load data
  pred = array(data = NA, dim = c(dim(obs)[1], dim(obs)[2], dim(obs)[3], 10))
  
  load(file.path(
    paste(dir_oss, "/GPCPv2_3/SPI", sc, "_GPCP_1981_2016.RData", sep = "")
  ))
  aux = get(nam)
  pred[, , , 1] = aux[, , ]
  
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
  pred[, , , 2] = aux[, , ]
  
  load(file.path(
    paste(
      "/Users/marco/Documents/dati/SPIF/data/SPI",
      sc,
      "_CHIRPS_1981_2016.RData",
      sep = ""
    )
  ))
  aux = get(nam)
  pred[, , , 3] = aux[, , ]
  
  
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
  pred[, , , 4] = aux[, , ]
  
  load(file.path(
    paste(dir_oss, "/GPCCv2018/SPI", sc, "_GPCC_1981_2016.RData", sep = "")
  ))
  aux = get(nam)
  pred[, , , 5] = aux[, , ]
  
  load(file.path(
    paste(dir_oss, "/JRA55/SPI", sc, "_JRA55_1981_2016.RData", sep = "")
  ))
  aux = get(nam)
  pred[, , , 6] = aux[, , ]
  
  load(file.path(
    paste(dir_oss, "/PRECL/SPI", sc, "_PRECL_1981_2016.RData", sep = "")
  ))
  aux = get(nam)
  pred[, , , 7] = aux[, , ]
  
  load(file.path(
    paste(dir_oss, "/ERA5/SPI", sc, "_ERA5_1981_2016.RData", sep = "")
  ))
  aux = get(nam)
  pred[, , , 8] = aux[, , ]
  
  load(file.path(
    paste(dir_oss, "/NCEP/SPI", sc, "_NCEP_1981_2016.RData", sep = "")
  ))
  aux = get(nam)
  pred[, , , 9] = aux[, , ]
  
  load(file.path(
    paste(
      "/Users/marco/Documents/dati/MERRA2/SPI",
      sc,
      "_MERRA2_1981_2016.RData",
      sep = ""
    )
  ))
  aux = get(nam)
  pred[, , , 10] = aux[, , ]
  
  pred[is.infinite(pred)] = NA
  
  
  
  if (sc == 12) {
    obs = obs[, , seq(12, dim(obs)[3], 12)]
    pred = pred[, , seq(12, dim(pred)[3], 12),]
  } else if (sc == 6) {
    obs = obs[, , seq(6, dim(obs)[3], 6)]
    pred = pred[, , seq(6, dim(pred)[3], 6),]
  } else if (sc == 3) {
    obs = obs[, , seq(3, dim(obs)[3], 3)]
    pred = pred[, , seq(3, dim(pred)[3], 3),]
  }
  
  
  
  
  
  
  
  for (izone in 1:dim(coordreg)[1]) {
    #for (izone in 1:length(reg)) {
    #for (izone in 11:length(reg)) {
    #for (izone in 2:10) {
    zone = reg[izone]
    print(zone)
    
    
    idxlon = which(coordreg[izone, 3] <= lon &
                     lon <= coordreg[izone, 4])
    idxlat = which(coordreg[izone, 1] <= lat &
                     lat <= coordreg[izone, 2])
    
    
    
    
    
    obs2 = obs[idxlon, idxlat,]
    pred2 = pred[idxlon, idxlat, ,]
    
    dim(pred2) = c(length(idxlon) * length(idxlat) * dim(obs)[3], dim(pred)[4])
    dim(obs2) = c(length(idxlon) * length(idxlat) * dim(obs)[3])
    aux = Rankhist(pred2,
                   obs2,
                   reduce.bins = 1,
                   handle.na = "use.complete")
    talagr = TestRankhist(aux)
    
    # round(sid2$pearson.chi2[2], digits = 2)
    
    if (talagr$pearson.chi2[2] <= 0.05) {
      testchi2 = '<0.05'
    } else {
      testchi2 = as.character(round(talagr$pearson.chi2[2], digits = 2))
    }
    
    if (talagr$jp.lin[2] <= 0.05) {
      testlin = '<0.05'
    } else {
      testlin = as.character(round(talagr$jp.lin[2], digits = 2))
    }
    
    if (talagr$jp.sq[2] <= 0.05) {
      testsq = '<0.05'
    } else {
      testsq = as.character(round(talagr$jp.sq[2], digits = 2))
    }
    
    ##PLOT
    
    nomeout = file.path(dir_out,
                        paste("rankhist_", zone, "_spi", sc, ".eps", sep = ""))
    
    setEPS()
    postscript(
      nomeout,
      horiz = FALSE,
      onefile = FALSE,
      width = 8.5,
      height = 5.5
    )
    
    old.par <- par(no.readonly = TRUE)
    on.exit(par(old.par))
    
    PlotRankhist(aux, mode = "raw")
    mtext(
      paste0(
        'Chi-Square: ',
        testchi2,
        ', Slope: ',
        testlin,
        ', Convexity: ',
        testsq
      ),
      side = 3
    )
    
    
    dev.off()
    
    save(talagr, file = file.path(
      dir_out,
      paste("rankhist_", zone, "_spi", sc, ".RData", sep = "")
    ))
    
  }
}


