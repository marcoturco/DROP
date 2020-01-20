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
library(ggplot2)
library(reshape)

source("/Users/marco/Dropbox/estcena/scripts/BSC/R/common/ColorBarM.R")


data(wrld_simpl)

anni = 1981:2016
mesi = rep(1:12, length(anni))


## fix parameters
dir_oss = '/Users/marco/Documents/dati/obs'
dir_out = '/Users/marco/Documents/output/obs_uncertainties'

casi = c('USA',
         'AFRICA')

# casi = c('USA')

for (icaso in 1:length(casi)) {
#for (icaso in 1:1) {
  caso = casi[icaso]
  
  
 if (caso == "USA") {
   #35N-45N, 110W-90W
    # lon1 = -115
    # lon2 = -90
    # lat1 = 30
    # lat2 = 50
    lon1=-101.25
    lat1=41.25
    anno_case = 2012
  } else if (caso == "AFRICA") {
    # lon1 = -20
    # lon2 = 55
    # lat1 = -40
    # lat2 = 40
    lon1=26.25
    lat1=13.75
    anno_case = 1984
  } else {
    print('case study not known')
  }
  
  
  
  
  
  ## load dati
  load(file.path(
    dir_oss,
    #paste("GPCPv2_3/SPI12_GPCP_1981_2017.RData", sep = "")
    paste("MSWEP/SPI12_MSWEP_1981_2016.RData", sep = "")
  ))
  #dum = get(tolower(extreme_index))
  #erai = dum[, , seq(mese, length(anni_rean) * 12, 12)]
  #rm(dum)
  obs = spi12
  
  load(file.path(dir_oss, "GPCPv2_3/lon_GPCP_1981_2017.RData"))
  load(file.path(dir_oss, "GPCPv2_3/lat_GPCP_1981_2017.RData"))
  
  ## load data
  pred = array(data = NA, dim = c(dim(spi12)[1], dim(spi12)[2], dim(obs)[3], 10))
  load(file.path(
    paste(dir_oss, "/GPCPv2_3/SPI12_GPCP_1981_2016.RData", sep = "")
  ))
  pred[, , , 1] = spi12
  
  load(file.path(
    paste(dir_oss, "/CAMS_OPI/SPI12_CAMS_OPI_1981_2016.RData", sep = "")
  ))
  pred[, , , 2] = spi12
  
  load(file.path(
    paste(
      "/Users/marco/Documents/dati/SPIF/data/SPI12_CHIRPS_1981_2016.RData",
      sep = ""
    )
  ))
  pred[, , , 3] = spi12
  
  load(file.path(
    paste(
      dir_oss,
      "/CPC_GLOBAL_PRECIP/SPI12_CPC_1981_2016.RData",
      sep = ""
    )
  ))
  pred[, , , 4] = spi12
  
  load(file.path(
    paste(dir_oss, "/GPCCv2018/SPI12_GPCC_1981_2016.RData", sep = "")
  ))
  pred[, , , 5] = spi12
  
  load(file.path(
    paste(dir_oss, "/JRA55/SPI12_JRA55_1981_2016.RData", sep = "")
  ))
  pred[, , , 6] = spi12
  
  load(file.path(
    paste(dir_oss, "/PRECL/SPI12_PRECL_1981_2016.RData", sep = "")
  ))
  pred[, , , 7] = spi12
  
  load(file.path(
    paste(dir_oss, "/ERA5/SPI12_ERA5_1981_2016.RData", sep = "")
  ))
  pred[, , , 8] = spi12
  
  load(file.path(
    paste(dir_oss, "/NCEP/SPI12_NCEP_1981_2016.RData", sep = "")
  ))
  pred[, , , 9] = spi12
  
  load(file.path(
    paste(
      "/Users/marco/Documents/dati/MERRA2/SPI12_MERRA2_1981_2016.RData",
      sep = ""
    )))
  pred[, , , 10] = spi12
  
  
  
  
  
  ni = length(lon)
  nj = length(lat)
  
  
  
  ## select case study
  # ilon = which(lon > lon1 & lon < lon2)
  # ilat = which(lat > lat1 & lat < lat2)
  ilon = which(lon == lon1)
  ilat = which(lat == lat1)
  

  
  
  anno_for = which(anni == anno_case)
  mese_dec=which(mesi==12)
  mese_dec_for=mese_dec[anno_for]
  
  # image.plot(lon[ilon], lat[ilat], obs[ilon, ilat , mese_dec_for])
  # plot(wrld_simpl, add = TRUE)
  # 
  
  
  
  #plot_data <-
  #  data.frame(anni_rean,GHCND,MERRA_masked,ERAI_masked,JMA_masked,MERRA,ERAI,JMA)
  #test_data_long <-
  #  melt(plot_data, id = "anni_rean")  # convert to long format
  #plot(apply(obs[ilon, ilat , ],c(3),mean,na.rm = TRUE))
  #lines(apply(obs[ilon, ilat , ],c(3),mean,na.rm = TRUE))
  
  
  #month <- paste0(anno_2000)
  #as.Date(paste(month,"-01",sep=""))
  
  
  months=(mese_dec_for - 20):(mese_dec_for + 20)
  #months=12:420
  years=rep(anni,each=12)
  fecha=paste0(as.character(years[months]),"-",formatC(mesi[months], width=2, flag="0"))

  #mswep=apply(obs[ilon, ilat , months], c(3), mean, na.rm = TRUE)
  mswep=obs[ilon, ilat , months]
  #ens=apply(pred[ilon, ilat , months,8], c(3), mean, na.rm = TRUE)
  #ens=pred[ilon, ilat , months,9]
  
  # aux=apply(pred[ilon, ilat , months,], c(3,4), mean, na.rm = TRUE)
  # ens_min=apply(aux, c(1), min, na.rm = TRUE)
  # ens_max=apply(aux, c(1), max, na.rm = TRUE)
  # 
  aux=pred[ilon, ilat , months,1:10]
  # ens_mean=apply(aux, c(1), mean, na.rm = TRUE)
  
  load(file.path(
    paste(dir_out, "/SPI12_ENS_1981_2016.RData", sep = "")
  ))
  ens_mean=spi[ilon, ilat , months]
  
  ens_min=apply(aux, c(1), min, na.rm = TRUE)
  ens_max=apply(aux, c(1), max, na.rm = TRUE)
  
  
  
  ens_sd=apply(aux, c(1), sd, na.rm = TRUE)
  
  scrutiny_data <-
    data.frame(fecha,mswep,ens_mean,ens_sd)
  
  
  #plot_data <-
  #  data.frame(fecha,mswep,ens,ens_min,ens_max)
  # plot_data <-
  #   data.frame(fecha,ens_mean,ens_min,ens_max)
  GPCP=aux[,1]
  CAMS_OPI=aux[,2]
  CHIRPS=aux[,3]
  CPC=aux[,4]
  GPCC=aux[,5]
  JRA55=aux[,6]
  PRECL=aux[,7]
  ERA5=aux[,8]
  NCEP=aux[,9]
  MERRA2=aux[,10]  
  
  plot_data <-
    data.frame(fecha,GPCP,CAMS_OPI,CHIRPS,CPC,GPCC,JRA55,PRECL,ERA5,NCEP,MERRA2,mswep,ens_mean)
  
  
  test_data_long <-
    melt(plot_data, id = "fecha")  # convert to long format
  # test_data_long$id=1
  # test_data_long$id[test_data_long$value==mswep]=2
  # test_data_long$id[test_data_long$value==ens_mean]=3
  #cutoff <- data.frame( x = c(-Inf, Inf), y = -0.8, cutoff = factor(-0.8) )
  
  sp <-
    ggplot(data = test_data_long, aes(x = fecha, y = value, group = variable)) +
    geom_line(aes(
      linetype = variable,
      color = variable#,
      #size = variable
    )) +
    #values=c("#999999", "#E69F00", "#56B4E9")
    scale_linetype_manual(values = c( "dotted","dotted","dotted","dotted","dotted", "dotted","dotted","dotted","dotted","dotted","solid", "solid")) +
    scale_color_manual(values = c( "#484848","#484848",'#484848','#484848','#484848','#484848','#484848','#484848','#484848','#484848','orange', '#008000')) +
    # scale_size_manual(values = c(1, 1, 1)) +
    theme(legend.position = "top") +
    xlab("Months") + ylab(paste("SPI12", sep = "")) + # Set axis labels
    ggtitle("") +     # Set title
    theme_bw() +
    #scale_x_continuous(breaks = as.numeric((months))[c(TRUE, rep(FALSE, 19))])
    theme(axis.text.x  = element_text(
      angle = 90,
      vjust = 0.5,
      size = 20
    )) 
  
  # cutoff <- data.frame(yintercept=-0.8, cutoff=factor(-0.8))
  # 
  # sp  + 
  #   geom_hline(aes(yintercept=yintercept, linetype="dashed"), data=cutoff) 
  
  # Add horizontal line at y = O
  sp + geom_hline(yintercept = -0.8, linetype = "dashed")
  
  aspect_ratio <- 2
  height <- 7
  
  #ggsave(file.path(dir_out, paste("spi12_time_series", caso, ".eps", sep = "")),width=8.5,height=5.5)
  ggsave(paste(dir_out,"/spi12_time_series", caso, ".eps", sep = ""),height = 7 , width = 7 * aspect_ratio, device="eps")
  
} 
  
#  aes(colour = ifelse(value <
 #                       0, "blue", "red")))

# 
#   
#   sp <-ggplot(data = test_data_long, aes(x = fecha, y = value, group = variable)) +
#       geom_line(aes(
#         linetype = variable, color = variable, size = variable
#       )) +
#       #geom_point(size=5)+
#       #geom_point(shape=1,size=3) +    # Use hollow circles
#       #scale_linetype_manual(values = c(
#       #  "solid","solid","solid","solid","dashed","dashed","dashed"
#       #)) +
#       scale_linetype_manual(values = c(
#         "solid","dotted","dotted")) +
#       scale_color_manual(values = c('black','gray','gray')) +
#       scale_size_manual(values = c(1,1,1)) +
#       theme(legend.position="top") +
#       xlab("Months") + ylab(paste("SPI12",sep="")) + # Set axis labels
#       ggtitle("") +     # Set title
#       theme_bw() +
#       #scale_x_continuous(breaks = as.numeric((months))[c(TRUE, rep(FALSE, 19))])
#       theme(axis.text.x  = element_text(angle=90, vjust=0.5, size=10))
#     
#     
#     # Add horizontal line at y = O
#     sp + geom_hline(yintercept=0,linetype="dashed") 
#       
#     
#     ggsave(file.path(dir_out, paste("spi12_time_series",caso,".eps",sep = ""))) #,width = 20, height = 20, units = "cm")
#     
#     
#   # plot_data <-
#   #   data.frame(fecha,ens)
#   # test_data_long <-
#   #   melt(plot_data, id = "fecha")  # convert to long format
#   #   
#   #   ggplot(test_data_long, aes(x= fecha, y = value, group = variable)) + 
#   #     geom_bar(stat = 'identity', aes(fill = value>0), position = 'dodge', col = 'transparent') + 
#   #     theme_bw() + scale_fill_discrete(guide = 'none') + 
#   #     labs(x = '', y = 'SPI')
#     
#     
#       #file.path(dir_out, paste("global_trend_",extreme_index,".eps",sep = ""))
#     #ggsave(file.path(dir_out, paste("global_trend_",extreme_index,".eps",sep = ""))) #,width = 20, height = 20, units = "cm")
# 

  


  
  
  
  
  

