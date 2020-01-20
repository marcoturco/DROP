##############################################
# LOAD GSIM STATIONS FOR THE U.S.
# HONG DO - University of Michigan SEAS
# NOV 12, 2019
##############################################
# LOAD REQUIRED PACKAGES
require(parallel)
require(zoo)
require(lubridate)
library(rworldmap)
library(sp)
###############################################
# CORES AND PATH SET UP
maxCores = 4
rootPath = "/Users/marco/Documents/dati/GSIM"
gsimIndicesPath = file.path(rootPath,"GSIM_indices/TIMESERIES/yearly")

#script source
scriptPath = file.path(rootPath,"/")
source(file.path(scriptPath,"01_gsim_file_format.r"))

#################################################
#IDENTIFY QUALIFIED GSIM STATIONS
# usFiles = list.files(gsimIndicesPath,pattern="US_")  # list all US files
allFiles = list.files(gsimIndicesPath,pattern=".year")  # list all US files

# using parallel code to:
# 1/ read all US stations - get a subset of data only from 1993 to 2018
# 2/ assess the number of available data points
#    + for a specific year: more than 350 data points -> mark as good quality (code = TRUE) 
#                                     else mark as bad quality (code = FALSE)
# 3/ count the number of year with "good" quality 
# 4/ station with at least 22 "good years" are coded as 1 (to be kept) - otherwise 0 (i.e. to be removed)

sink = mclapply(1:length(allFiles),function(i)
#for (i in 1:100)
{
	if (i==1| i%%500==0) cat(base::date(),"Checking station number",i,"\n")
	pp = file.path(gsimIndicesPath,allFiles[i])
	exampleYearly = read.gsim(pp)$time.series
	yearids = year(index(exampleYearly))
	ydSubset = which(yearids%in%1981:2016)
	if (length(ydSubset) > 0) {
		exampleYearly = exampleYearly[ydSubset,]
		yearEnoughObs = exampleYearly[,"n.available"]>= 329 #350
		countObs = sum(yearEnoughObs)
		if (countObs >= 32) op=1 else op=0
	} else op = 0
	op
},mc.cores=maxCores)
sink = unlist(sink)

# GET SUBSET OF QUALIFIED STATIONS
filesQualified = allFiles[sink==1]
idsQualified = substr(filesQualified,1,10) # store GSIM id for "good stations"

# EXTRACT A SPECIFIC INDEX OVER 1981:2016 PERIOD
indName = "MEAN"
refPeriod = 1981:2016

# # STORE MEAN INDICES FOR EACH STATION IN ONE COLUMN, EACH ROW WILL BE A SPECIFIC YEAR OVER THE REFERENCE PERIOD
datExtracted = mclapply(1:length(filesQualified),function(i)
{
	if (i==1| i%%500==0) cat(base::date(),"Checking station number",i,"\n")   # SHOWING DATA LOADING PROGRESS
	pp = file.path(gsimIndicesPath,filesQualified[i])
	tmpYearly = read.gsim(pp)$time.series                                # GET ONLY TIME SERIES
	yearids = year(index(tmpYearly))
	idMatch = match(refPeriod,yearids)        # identify ids of the time series to be extracted
	holderVector = tmpYearly[idMatch,indName] # extract data for only 1981:2016 - assign NA where not available
	holderVector = coredata(holderVector)     # only get data - not the attribute
	return(holderVector)
},mc.cores=maxCores)
datExtracted = do.call(cbind,datExtracted)
# 
# write.csv(datExtracted,paste0(rootPath,"/GLOBAL_MEAN_1981_2016.csv"),row.names=F)
aux=t(as.data.frame(datExtracted))

## extract metadata
ID=vector(mode="character",length = length(filesQualified))
longitude=vector(mode="double",length = length(filesQualified))
latitude=vector(mode="double",length = length(filesQualified))
area=vector(mode="double",length = length(filesQualified))
                 


for (i in 1:length(filesQualified)) {
  pp = file.path(gsimIndicesPath,filesQualified[i])
  ID[i]=read.gsim(pp)$meta.data$data$gsim.no
  longitude[i]=read.gsim(pp)$meta.data$data$longitude
  latitude[i]=read.gsim(pp)$meta.data$data$latitude
  area[i]=read.gsim(pp)$meta.data$data$area
}


aux2=as.data.frame(cbind(ID,longitude,latitude,area,aux))


# xy=aux2[,c(2,3)]
# aux3=SpatialPointsDataFrame(coords=xy,data=aux2,proj4string = CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))


newmap <- getMap(resolution = "low")
plot(
  newmap,
  xlim = c(-180, 180),
  ylim = c(-60, 85),
  asp = 1
)
points(as.numeric(as.character(aux2$longitude)),as.numeric(as.character(aux2$latitude)),pch = 16,col = "blue",cex = 0.4)


# gsim=aux2[as.numeric(as.character(aux2$area))>10000,]
gsim=aux2
gsim=gsim[!is.na(gsim$ID),]


newmap <- getMap(resolution = "low")
plot(
  newmap,
  xlim = c(-180, 180),
  ylim = c(-60, 85),
  asp = 1
)
points(as.numeric(as.character(gsim$longitude)),as.numeric(as.character(gsim$latitude)),pch = 16,col = "blue",cex = 0.4)


for (ibasin in 1:dim(gsim)[1]) {
  fileid = paste0(rootPath,
                  '/GSIM_metadata/GSIM_catchments/',
                  as.character(gsim$ID[ibasin]),
                  '.shp')
  
  if(!file.exists(fileid)){
    
    gsim[ibasin,]=NA
    next
  }
}  

gsim=gsim[!is.na(gsim$ID),]


newmap <- getMap(resolution = "low")
plot(
  newmap,
  xlim = c(-180, 180),
  ylim = c(-60, 85),
  asp = 1
)
points(as.numeric(as.character(gsim$longitude)),as.numeric(as.character(gsim$latitude)),pch = 16,col = "blue",cex = 0.4)


write.csv(gsim,paste0(rootPath,"/GLOBAL_MEAN_1981_2016.csv"))