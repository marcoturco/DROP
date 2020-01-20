rm(list = ls())
graphics.off()
gc()

library(sp)
library(maptools) # loads sp library too
library(RColorBrewer) # creates nice color schemes
library(classInt) # finds class intervals for continuous variables
library(fields)
library(maps)
#source("/Users/marco/Dropbox/estcena/scripts/BSC/R/common/ColorBarM.R")
#source("/Users/marco/Dropbox/estcena/scripts/BSC/R/INDICATORS/drought/reliability_wp_mio.R")
source("/Users/marco/Dropbox/estcena/scripts/BSC/R/common/ReliabilityDiagram_MIO2.R")
source("/Users/marco/Dropbox/estcena/scripts/BSC/R/common/myreliability.R")

##
nb = 1000

time_scale = c(1)
## loop soglie, start dates, zone
#thresholds = c(-0.8,-1.3,-1.6,-2.0)
thresholds = c(-1.6)


pstep = 0.2
prob1 = seq(0, 1 - pstep, pstep)
prob1[1] = -prob1[length(prob1)]
prob2 = seq(0 + pstep, 1, pstep)


#time_scale = c(1, 3, 6, 12)

data(wrld_simpl)


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




#start_dates = c(04)
#thresholds = c(-0.8)
#zone = c("SW")


#spei < -0.8 Moderate drought
#spei < -1.3 Severe drought
#spei < -1.6 Extreme drought
#spei < -2.0 Exceptional drought (if we have enough statistical samples)

#th=-0.8
#start_date=05

# idate = 1
# ith = 1
# izone = 1

#for (idate in 1:length(start_dates)) {



for (isc in 1:length(time_scale)) {
  sc = time_scale[isc]
  
  nam <- paste("spi", sc, sep = "")
  print(nam)
  
  load(file.path(
    dir_oss,
    paste("MSWEP/SPI", sc, "_MSWEP_1981_2016.RData", sep = "")
  ))
  obs = get(nam)
  
  obs[is.infinite(obs)]=NA
  
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
  
  pred[is.infinite(pred)]=NA
  
  # 2003
  anno_for = which(anni == 2003)
  image.plot(lon, lat, obs[, , anno_for])
  plot(wrld_simpl, add = TRUE)
  #
  inout = obs[, , anno_for] * 0
  image.plot(lon, lat, inout)
  plot(wrld_simpl, add = TRUE)
  
  
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
  #for (ith in 1:1) {
    th = thresholds[ith]
    print(th)
    
    for (izone in 1:dim(coordreg)[1]) {
    #for (izone in 1:length(reg)) {
    #for (izone in 11:length(reg)) {
      #for (izone in 2:10) {
      zone = reg[izone]
      print(zone)
      
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
      ## reliability
      ###############
      #pred = s4[, , mesi_8, ]
      #esp_aug = esp[, , mesi_8, ]
      #spei_obs = obs[, , mesi_8]
      
      rel_s4 <- myreliability(obs, pred, th, pstep, lat, inout1)
      freq_s4 = as.numeric(unlist(rel_s4[1]))
      h_s4 = as.numeric(unlist(rel_s4[2]))
      g_s4 = as.numeric(unlist(rel_s4[3]))
      slope_s4 = as.numeric(unlist(rel_s4[4]))
      obs.clim = sum(g_s4) / sum(h_s4)
      
      
      
      ##boot
      
      
      slope_s4_b = matrix(NA, nb, 1)
      
      
      if (!is.na(slope_s4)) {
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
          
          
          
          rel_b <-
            myreliability(obs_b, pred_b, th, pstep, lat, inout1)
          slope_s4_b[ib] = as.numeric(unlist(rel_b[4]))
          rm(rel_b)
          
          
          #lines(as.numeric(unlist(roc_b[2])), as.numeric(unlist(roc_b[1])))
          
        }
        
        #inter1.stat1<-obs.clim*(1-ws_slope1)
        
        
        ##PLOT
        
        nomeout = file.path(dir_out,
                            paste("reliability_", zone, "_", (th), "_spi", sc, ".eps", sep = ""))
        
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
        
        slope1.stat <-
          quantile(slope_s4_b,
                   probs = c(.025, 0.5, .975),
                   na.rm = TRUE)
        inter1.stat1 <- obs.clim * (1 - slope1.stat[1])
        inter1.stat2 <- obs.clim * (1 - slope1.stat[2])
        inter1.stat3 <- obs.clim * (1 - slope1.stat[3])
        
        
        
        plot(
          NULL,
          xlim = c(0, 1),
          ylim = c(0, 1),
          xlab = "Forecast probability",
          ylab = "Observed relative frequency"
        )
        a <- (1 - obs.clim) / 2 + obs.clim
        b <- obs.clim / 2
        x.p <- c(obs.clim, obs.clim, 1, 1, 0, 0)
        y.p <- c(0, 1, 1, a, b, 0)
        polygon(x.p, y.p, col = "#e6e6e6")
        abline(h = obs.clim, lty = 2)
        text(0.9, obs.clim, "No resolution", pos = 3)
        text(
          0.9,
          obs.clim + (a - b) * (0.9 - obs.clim),
          "No skill",
          pos = 1,
          srt = atan(a - b) / (2 * pi) *
            360
        )
        
        pointxlow1 = c(max(0,-inter1.stat1 / slope1.stat[1]),
                       min(1, (1 - inter1.stat1) / slope1.stat[1]))
        pointxhigh1 = c(max(0,-inter1.stat3 / slope1.stat[3]),
                        min(1, (1 - inter1.stat3) / slope1.stat[3]))
        
        y1 = 1
        y3 = 1
        if (slope1.stat[1] <= 1) {
          y1 = inter1.stat1 + slope1.stat[1]
        }
        if (slope1.stat[3] <= 1) {
          y3 = inter1.stat3 + slope1.stat[3]
        }
        pointylow1 = c(max(0, inter1.stat1), y1)
        pointyhigh1 = c(max(0, inter1.stat3), y3)
        
        polygon(
          c(pointxhigh1, rev(pointxlow1)),
          c(pointyhigh1, rev(pointylow1)),
          col = "green",
          border = NA
        )
        
        
        
        
        #ws_slope1 <- summary(weiss1.rel)$coefficients[1]
        #inter2.stat1<-obs.clim*(1-slope1.stat[2])
        #abline(inter2.stat1, slope1.stat[2], lty = 1, col = "red")
        
        #inter2.stat1<-obs.clim*(1-slope2.stat[2])
        #abline(inter2.stat1, slope2.stat[2], lty = 1, col = "red")
        
        points(
          prob2 - (pstep / 2),
          freq_s4,
          col = "black",
          pch = 21,
          bg = "green",
          lwd = 2,
          type = "p"
        ) #type = "b")
        
        
        
        lines(c(0, 1), c(0, 1), lty = 1)
        
        pp <- par("plt")
        par(plt = c(pp[2] - 0.2, pp[2], pp[3], pp[3] + 0.2))
        par(new = TRUE)
        barplot(
          rbind(h_s4),
          beside = TRUE,
          axes = FALSE,
          axisnames = FALSE,
          col = c("green")
        )
        axis(4)
        box()
        
        
        dev.off()
        
        
        
        # rel = rel_wp(
        #   spei_aug_prob_s4,spei_aug_prob_esp, spei_aug_obs_prob,nomeout, plot = TRUE,nboot =
        #     1000, attributes = TRUE
        # )
        
      }
      
      rel <- list()
      rel$slope1 <- slope_s4_b
      
      
      
      
      save(rel, file = file.path(
        dir_out,
        paste("reliability_", zone, "_", (th), "_spi", sc, ".RData", sep = "")
      ))
      
      
      
    }
  }
  
}
