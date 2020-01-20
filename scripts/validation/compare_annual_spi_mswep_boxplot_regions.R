rm(list = ls())
graphics.off()
gc()

# library(StatDA)

source("/Users/marco/Dropbox/estcena/scripts/BSC/R/common/my_boxplot_stat.R")
source("/Users/marco/Dropbox/estcena/scripts/BSC/R/common/my_boxplot.R")


## fixed parameters
time_scale = c(1, 3, 6, 12)

dir_oss = '/Users/marco/Documents/dati/obs'
dir_out = '/Users/marco/Documents/output/obs_uncertainties'



load(file.path(dir_oss, "GPCPv2_3/lon_GPCP_1981_2017.RData"))
load(file.path(dir_oss, "GPCPv2_3/lat_GPCP_1981_2017.RData"))
lonGPCP = lon
latGPCP = lat


lat2 = latGPCP[which(latGPCP > -60 & latGPCP < 85)]
ni = length(lon)
nj = length(lat2)




datasets = c(
  'CAMS_OPI',
  'CHIRPS',
  'CPC',
  'ERA5',
  'GPCC',
  'GPCP',
  'JRA55',
  'MERRA2',
  'NCEP',
  'PRECL',
  'ENS'
)


## regions

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
    -45,-11,
    110,
    155,
    -20,
    12,-82,-34,
    -56,-20,-76,-40,
    10,
    30,-116,-83,
    30,
    60,-130,-103,
    30,
    50,-103,-85,
    25,
    50,-85,-60,
    60,
    72,-170,-103,
    50,
    85,-103,-10,
    30,
    48,-10,
    40,
    48,
    75,-10,
    40,
    -12,
    18,-20,
    22,
    -12,
    18,
    22,
    52,
    -35,-12,-10,
    52,
    18,
    30,-20,
    65,
    -11,
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



##



for (isc in 1:length(time_scale)) {
  sc = time_scale[isc]
  
  nam <- paste("spi", sc, sep = "")
  print(nam)
  
  load(paste(dir_out,
             "/corre_spi_",
             sc,
             "_ENS_MSWEP.RData",
             sep = ""))
  
  
  
  corre_box <-
    matrix(data = NA,
           nrow = ni * nj,
           ncol = length(reg))
  
  
  for (izone in 1:dim(coordreg)[1]) {
    #for (izone in 1:1) {
    zona = regions[izone]
    print(zona)
    
    idxlon = which(coordreg[izone, 3] <= lon &
                     lon <= coordreg[izone, 4])
    idxlat = which(coordreg[izone, 1] <= lat &
                     lat <= coordreg[izone, 2])
    
    corre2 = corre[idxlon, idxlat]
    corre_box[1:length(as.vector(corre2)), izone] = as.vector(corre2)
    
  }
  
  plot_data = data.frame(corre_box)
  
  
  
  
  setEPS()
  postscript(
    file.path(
      dir_out,
      paste("boxplot_corre_spi_", sc, "_mswep_regions.eps", sep = "")
    ),
    horiz = FALSE,
    onefile = FALSE,
    width = 8.5,
    height = 5.5
  )
  par(mar = c(15, 6, 4, 1) + .1)
  my_boxplot(
    (plot_data),
    quant = c(0.025, 0.975),
    outline = FALSE,
    las = 2,
    ylim = c(0.2, 1.1),
    names = regions
  )
  grid (NULL, NULL, lty = 6, col = "cornsilk2")
  dev.off()
  
}



