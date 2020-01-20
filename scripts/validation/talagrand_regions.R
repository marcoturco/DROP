rm(list = ls())
graphics.off()
gc()



## fixed parameters
# time_scale = c(1, 3, 6, 12)
time_scale = c(12)

dir_oss = '/Users/marco/Documents/dati/obs'
dir_out = '/Users/marco/Documents/output/obs_uncertainties'




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
  
  
  # save(talagr, file = file.path(
  #   dir_out,
  #   paste("rankhist_", zone, "_", (th), "_spi", sc, ".RData", sep = "")
  # ))
  
  talagr_all <-
    matrix(data = NA,
           nrow = 3,
           ncol = length(reg))
  
  
  for (izone in 1:dim(coordreg)[1]) {
    #for (izone in 1:1) {
    zone = reg[izone]
    print(zone)
    
    
    load(file.path(
        dir_out,
        paste("rankhist_", zone, "_spi", sc, ".RData", sep = "")
      ))
    
    talagr_all[1,izone]=talagr$pearson.chi2[2]
    talagr_all[2,izone]=talagr$jp.lin[2]
    talagr_all[3,izone]=talagr$jp.sq[2]
    
  }
  
  
  # plot(1:length(reg),talagr_all[1,])
  setEPS()
  postscript(
    file.path(
      dir_out,
      paste("rankhist_", sc, "_mswep_regions.eps", sep = "")
    ),
    horiz = FALSE,
    onefile = FALSE,
    width = 8.5,
    height = 5.5
  )
  par(mar = c(15, 6, 4, 1) + .1)
  
  plot(1:length(reg),talagr_all[2,],ylim=c(0,1),xlab="Regions", ylab="P-values",xaxt="n")
  points((1:length(reg))-0.1,talagr_all[1,],col='blue')
  points((1:length(reg))+0.1,talagr_all[3,],col='red')
  abline(v=(1:length(reg))+0.5,col='gray')
  abline(h=0.05,col='black',lty=2, lwd=2)
  
  xtick<-seq(1, 21, by=1)
  axis(side=1, at=xtick, labels = FALSE)
  text(x=xtick,  par("usr")[3], 
       labels = regions, srt = 90, pos = 2, xpd = TRUE)
  
  dev.off()
  
}



