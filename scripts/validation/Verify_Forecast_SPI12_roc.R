rm(list = ls())
graphics.off()
gc()

library(fields)
library(sp)
library(maptools) # loads sp library too
#library(verification)
#library(pROC)
#library(StatDA)
source("/Users/marco/Dropbox/estcena/scripts/BSC/R/common/myroc.R")

##################################################


nb = 1000
#time_scale = c(1, 3, 6, 12)
time_scale = c(1)
## loop soglie, start dates, zone
#thresholds = c(-0.8, -1.3)
thresholds = c(-0.8)



#########################################################33

data(wrld_simpl)

identity.col = "darkgrey"
identity.lty = 1
identity.lwd = 1


pstep = 0.2
prob1 = seq(0, 1 - pstep, pstep)
prob1[1] = -prob1[length(prob1)]
prob2 = seq(0 + pstep, 1, pstep)


## fix parameters
dir_oss = '/Users/marco/Documents/dati/obs'
#dir_out = '/Users/marco/Dropbox/estcena/scripts/obs_uncertainty'
dir_out="/Users/marco/Documents/output/obs_uncertainties"

anni = 1981:2016
mesi = rep(1:12, length(anni))
mesi_8 = which(mesi == 12)

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





#zone = c("SW", "SE", "NE", "NW")

#start_dates = c(04)
#thresholds = c(-0.8)
#zone = c("SW")

# lat2 = 50
# lon2 = 40

#spei < -0.8 Moderate drought
#spei < -1.3 Severe drought
#spei < -1.6 Extreme drought
#spei < -2.0 Exceptional drought (if we have enough statistical samples)

#th=-0.8
#start_date=05
#idate = 1
#ith = 1
#izone = 1
#for (ith in 1:length(thresholds)) {

for (isc in 1:length(time_scale)) {
  sc = time_scale[isc]
  
  nam <- paste("spi", sc, sep = "")
  print(nam)
  
  load(file.path(
    dir_oss,
    paste("MSWEP/SPI", sc, "_MSWEP_1981_2016.RData", sep = "")
  ))
  obs = get(nam)
  
  
  
  ## load data
  pred = array(data = NA, dim = c(dim(obs)[1], dim(obs)[2], dim(obs)[3], 10))
  
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
    paste("/Users/marco/Documents/dati/SPIF/data/SPI",
      #dir_oss,
      #"/CHIRPS/SPI",
      sc,
      "_CHIRPS_1981_2016.RData",
      sep = "")
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
  pred[, , , 10] = aux[, , ]
  
  
  # 2003
  anno_for = which(anni == 2003)
  image.plot(lon, lat, obs[, , anno_for])
  plot(wrld_simpl, add = TRUE)
  #
  inout = obs[, , anno_for] * 0
  image.plot(lon, lat, inout)
  plot(wrld_simpl, add = TRUE)
  
  
  # if (sc > 1) {
  #   obs = obs[, , -(1:sc - 1)]
  #   pred = pred[, , -(1:sc - 1), ]
  # }
  if (sc==12) {
    obs = obs[, , seq(12, dim(obs)[3], 12)]
    pred = pred[, , seq(12, dim(pred)[3], 12),]
  } else if (sc==6) {
    obs = obs[, , seq(6, dim(obs)[3], 6)]
    pred = pred[, , seq(6, dim(pred)[3], 6),]
  } else if (sc==3) {
    obs = obs[, , seq(3, dim(obs)[3], 3)]
    pred = pred[, , seq(3, dim(pred)[3], 3),]  
  }
  
  for (ith in 1:length(thresholds)) {
    th = thresholds[ith]
    print(th)
    
    
    
    
    # for (izone in 1:dim(coordreg)[1]) {
      for (izone in 10:10) {
      zona = regions[izone]
      print(zona)
      
      
      
      inout1 = inout
      idxlon = which(coordreg[izone, 3] <= lon &
                       lon <= coordreg[izone, 4])
      idxlat = which(coordreg[izone, 1] <= lat &
                       lat <= coordreg[izone, 2])
      inout1[idxlon, idxlat] = 1
      
      inout1 = inout1 + inout
      inout1[inout1 != 1] = NaN
      image.plot(lon, lat, inout1)
      plot(wrld_simpl, add = TRUE)
      
      ###############
      ## roc
      ###############
      
      roc_1 <- myroc(obs, pred, th, pstep, lat, inout1)
      
      
      #roc_1 <- myroc(obs[idxlon,idxlat,], pred[idxlon,idxlat,,], th, pstep, lat[idxlat], inout1)
      #roc_1 <- myroc(obs, pred, th, pstep, lat, inout)
      
      hr_1 = as.numeric(unlist(roc_1[1]))
      far_1 = as.numeric(unlist(roc_1[2]))
      hr_1[length(hr_1) + 1] = 0
      far_1[length(far_1) + 1] = 0
      
      plot(far_1, hr_1, xlim = c(0, 1), ylim = c(0, 1))
      
      ##boot
      
      far_b_1 = matrix(NA, nb, length(hr_1) - 1)
      hr_b_1 = matrix(NA, nb, length(hr_1) - 1)
      auc_b_1 = matrix(NA, nb, 1)
      
      for (ib in 1:nb) {
        cat('Processing ', ib, 'of', nb, 'boostraps', '\n')
        
        ind <-
          sample(1:dim(obs)[3],
                 size = dim(obs)[3],
                 replace = TRUE)
        
        obs_b = obs * NA
        for (i in 1:ni) {
          for (j in 1:nj) {
            obs_b[i, j, ] = obs[i, j, ind]
          }
        }
        
        pred_b = pred * NA
        for (i in 1:ni) {
          for (j in 1:nj) {
            for (k in 1:dim(pred)[4]) {
              pred_b[i, j, , k] = pred[i, j, ind, k]
            }
          }
        }
        
        
        
        
        roc_b <- myroc(obs_b, pred_b, th, pstep, lat, inout1)
        hr_b_1[ib,] = as.numeric(unlist(roc_b[1]))
        far_b_1[ib,] = as.numeric(unlist(roc_b[2]))
        auc_b_1[ib] = as.numeric(unlist(roc_b[3]))
        rm(roc_b)
        
        #lines(as.numeric(unlist(roc_b[2])), as.numeric(unlist(roc_b[1])))
        
      }
      
      #s4_aug_b[i,j,]=obs[i,j,ind]
      
      hr_ci_1 = matrix(0, dim(hr_b_1)[2] + 1, 2)
      far_ci_1 = matrix(0, dim(hr_b_1)[2] + 1, 2)
      for (i in 1:dim(hr_b_1)[2]) {
        hr_ci_1[i,] = as.numeric(quantile(hr_b_1[, i], c(0.025, 0.975), na.rm = TRUE))
        far_ci_1[i,] = as.numeric(quantile(far_b_1[, i], c(0.025, 0.975), na.rm = TRUE))
      }
      
      postscript(
        file.path(dir_out, paste("roc_", (th), "_", reg[izone],  "_spi", sc, ".eps", sep = "")),
        horiz = FALSE,
        onefile = FALSE,
        width = 8.5,
        height = 5.5
      )
      
      # png(file.path(dir_out, paste("roc_",zona,"_",(th),"_",sprintf("%02d", start_date),".png",sep = "")),
      #     width = 480, height = 480, units = "px", pointsize = 12,
      #     bg = "white",  res = NA,
      #     type = c("cairo"))
      #
      par(pty = "s")
      plot(
        far_1,
        hr_1,
        xlim = c(0, 1),
        ylim = c(0, 1),
        col = "white",
        xlab = "False Alarm Rate",
        ylab = "Hit Rate"
      )
      polygon(c(far_1, rev(far_1)),
              c(hr_ci_1[, 1], rev(hr_ci_1[, 2])),
              col = rgb(0, 1, 0),
              border = NA)
      #polygon(c(far_ci_1[,1],rev(far_ci_1[,2])),c(hr_1,rev(hr_1)),col=rgb(0, 1, 0,0.6),border=NA)
      polygon(c(far_ci_1[, 1], rev(far_ci_1[, 2])),
              c(hr_1, rev(hr_1)),
              col = rgb(0, 1, 0),
              border = NA)
      
      lines(
        x = c(0, 1),
        y = c(0, 1),
        col = identity.col,
        lwd = identity.lwd,
        lty = identity.lty
      )
      dev.off()
      
      roc_1 = auc_b_1
      # save(roc_1, file = file.path(
      #   dir_out, paste(
      #     "rocarea_1_",zona,"_",(th),"_",sprintf("%02d", start_date),".RData",sep = ""
      #   )
      # ))
      save(roc_1, file = file.path(
        dir_out,
        paste("rocarea_1_",
              (th), "_",
              reg[izone],
              "_spi", sc, 
              ".RData",
              sep = "")
      ))
      
      # plot_data <-
      #   data.frame(roc_1[!is.na(roc_1)])
      # 
      # boxplotperc(
      #   plot_data,
      #   quant = c(0.025, 0.975),
      #   outline = FALSE,
      #   las = 2,
      #   ylim = c(0, 1),
      #   col = c("green"),
      #   at = c(1),
      #   names = c("pred")
      # )
    }
  }
}
