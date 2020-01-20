rm(list = ls())
graphics.off()
gc()

## fixed parameters
library(StatDA)

#time_scale = c(1,3,6,12)
time_scale = c(1,3,6,12)
thresholds = c(-0.8,-1.3) 
cth = c('moderate_drought','severe_drought')


dir_oss = '/Users/marco/Documents/dati/obs'
dir_out = '/Users/marco/Documents/output/obs_uncertainties'



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




nb = 1000



for (isc in 1:length(time_scale)) {
  sc = time_scale[isc]
  for (ith in 1:length(thresholds)) {
    #for (ith in 1:1) {
    th = thresholds[ith]
    print(th)
    
    roc_box <-
      matrix(data = NA,
             nrow = nb,
             ncol = length(reg))
    
    
    for (idata in 1:length(reg)) {
      region = reg[idata]
      print(region)
      
      
      load(paste(dir_out, "/reliability_", region, "_", (th), "_spi", sc, ".RData", sep = ""))
      
      # save(rel, file = file.path(
      #   dir_out,
      #   paste("reliability_", zone, "_", (th), "_spi", sc, ".RData", sep = "")
      # ))
      
      roc_box[, idata] = (rel$slope1)
      
    }
    
    
    
    
    plot_data <-
      data.frame(
        roc_box[, 1],
        roc_box[, 2],
        roc_box[, 3],
        roc_box[, 4],
        roc_box[, 5],
        roc_box[, 6],
        roc_box[, 7],
        roc_box[, 8],
        roc_box[, 9],
        roc_box[, 10],
        roc_box[, 11],
        roc_box[, 12],
        roc_box[, 13],
        roc_box[, 14],
        roc_box[, 15],
        roc_box[, 16],
        roc_box[, 17],
        roc_box[, 18],
        roc_box[, 19],
        roc_box[, 20],
        roc_box[, 21]
      )
    
    setEPS()
    #png(filename="figure.png", width=900, bg="white")
    
    #barplot(c(1.1, 0.8, 0.7), horiz=TRUE, border="blue", axes=FALSE, col="darkblue")
    #axis(2, at=1:3, lab=c("elephant", "hippo", "snorkel"), las=1, cex.axis=1.3)
    #dev.off()
    
    #postscript(file.path(dir_out, paste("boxplot_roc.eps",sep = "")),horiz=FALSE,onefile=FALSE,width=8.5,height=5.5)
    postscript(file.path(dir_out, paste("boxplot_reliability_", (cth[ith]), "_spi", sc, ".eps", sep = "")),
               horiz = FALSE,
               onefile = FALSE)
    par(mar = c(15, 6, 4, 1) + .1)
    boxplotperc(
      na.omit(plot_data),
      quant = c(0.025, 0.975),
      outline = FALSE,
      las = 2,
      ylim = c(0.6, 1.5),
      names = regions
    )
    grid (NULL, NULL, lty = 6, col = "cornsilk2")
    dev.off()
    
  }
}
