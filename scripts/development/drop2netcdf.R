rm(list = ls())
graphics.off()
gc()

library(ncdf4)

time_scale = c(1, 3, 6, 12)
# time_scale = c(1)

dirin = '/home/mt/Dropbox/estcena/scripts/obs_uncertainty/app_drop/data/'
dirout = '/home/mt/Dropbox/estcena/scripts/obs_uncertainty/gitlab/drop/data'

# setwd(dirin)
# files = list.files(pattern = "^SPI(.*)RData$")


load(file.path(dirin, "/lon_GPCP_1981_2017.RData"))
load(file.path(dirin, "/lat_GPCP_1981_2017.RData"))
lat = lat[which(lat > -60 & lat < 85)]

# define dimensions
londim <- ncdim_def("lon", "degrees_east", as.double(lon))
latdim <- ncdim_def("lat", "degrees_north", as.double(lat))


fechas<-seq(as.Date("1981/1/1"), as.Date("2019/12/31"), by = "month")
anni=1981:2019
time=seq(0,length(fechas)-1)
tunits <- "months since 1981-01-01 00:00:00.0 -0:00"
timedim <- ncdim_def("time",tunits,as.double(time))


for (isc in 1:length(time_scale)) {
  sc = time_scale[isc]
  
  load(file.path(dirin, paste0("/SPI",sc,"_ENS_1981_2019.RData")))
  spi[is.infinite(spi)]=NA
  spi=spi[,which(lat > -60 & lat < 85),]
  load(file.path(dirin, paste0("/SPI",sc,"_ENS_SPREAD_1981_2019.RData")))
  spi_sd=spi_sd[,which(lat > -60 & lat < 85),]
  load(file.path(dirin, paste0("/SPI_TRAF_LIG_",sc,"_DROP_1981_2019.RData")))
  spi_tl=spi_tl[,which(lat > -60 & lat < 85),]
  load(file.path(dirin, paste0("/SPI_PROB",sc,"_DROP_1981_2019.RData")))
  spi_prob=spi_prob[,which(lat > -60 & lat < 85),]
  
  
  
  
  # define variables
  fillvalue <- 1e32
  dlname <- "mean"
  mean_def <-
    ncvar_def("Ensemble mean",
              "",
              list(londim,latdim,timedim),
              fillvalue,
              dlname,
              prec = "double")
  dlname <- "spread"
  spread_def <-
    ncvar_def("Ensemble spread",
              "",
              list(londim,latdim,timedim),
              fillvalue,
              dlname,
              prec = "double")
  dlname <- "warning"
  warning_def <-
    ncvar_def("Warning levels",
              "",
              list(londim,latdim,timedim),
              fillvalue,
              dlname,
              prec = "double")
  dlname <- "probability"
  probability_def <-
    ncvar_def("Probability for moderate drought",
              "",
              list(londim,latdim,timedim),
              fillvalue,
              dlname,
              prec = "double")
  
  
  
  # create netCDF file and put arrays
  
  
  ncfname <- paste0(dirout,"/DROP_",sc,"_1981_2019.nc")
  ncout <- nc_create(ncfname, list(mean_def, spread_def,warning_def,probability_def))
  
  # put variables
  ncvar_put(ncout, mean_def, spi)
  ncvar_put(ncout, spread_def, spi_sd)
  ncvar_put(ncout, warning_def, spi_tl)
  ncvar_put(ncout, probability_def, spi_prob)
  
  # put additional attributes into dimension and data variables
  ncatt_put(ncout, "lon", "axis", "X") #,verbose=FALSE) #,definemode=FALSE)
  ncatt_put(ncout, "lat", "axis", "Y")
  ncatt_put(ncout,"time","axis","T")
  
  # add global attributes
  ncatt_put(ncout,0,"Title",'DROP is global land gridded dataset to monitoring DROught from Probabilistic approach')
  ncatt_put(ncout,0,"Version",'1.0')
  ncatt_put(ncout,0,"Institution","University of Murcia (Spain)")
  ncatt_put(ncout,0,"Url","https://drop.shinyapps.io/DROP/")
  ncatt_put(ncout,0,"Creators","Marco Turco <turco.mrc@gmail.com")
  ncatt_put(ncout,0,"Software","Create in R \n\t  https://earth.bsc.es/gitlab/mturco/drop")
  ncatt_put(ncout,0,"Date",date())
  ncatt_put(ncout,0,"reference","Marco Turco, Sonia Jerez, Markus Donat, Andrea Toreti, Sergio M. Vicente-Serrano and Francisco J. Doblas-Reyes (2019). An operational global probabilistic dataset for monitoring meteorological droughts. BAMS. Under review")
  

  # close the file, writing data to disk
  nc_close(ncout)
  
}



