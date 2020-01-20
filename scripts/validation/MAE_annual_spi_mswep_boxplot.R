rm(list = ls())
graphics.off()
gc()

library(StatDA)



## fixed parameters
time_scale = c(1, 3, 6, 12)
# time_scale = c(12)

dir_oss = '/Users/marco/Documents/dati/obs'
dir_out = '/Users/marco/Documents/output/obs_uncertainties'
noaaout <- "/Users/marco/Documents/dati/obs/GHCN"


anni = 1981:2016
#SON 37 ANNI
anniok = 32 # 33/37 ?? circa il 90%
mesi = rep(1:12, length(anni))

load(file.path(dir_oss, "GPCPv2_3/lon_GPCP_1981_2017.RData"))
load(file.path(dir_oss, "GPCPv2_3/lat_GPCP_1981_2017.RData"))
lonGPCP = lon
latGPCP = lat


lat2 = latGPCP[which(latGPCP > -60 & latGPCP < 85)]
ni = length(lon)
nj = length(lat2)




datasets = c('CAMS_OPI',
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




for (isc in 1:length(time_scale)) {
  sc = time_scale[isc]
  
  nam <- paste("spi", sc, sep = "")
  print(nam)
  
  
  corre_box <-
    matrix(data = NA,
           nrow = ni * nj,
           ncol = length(datasets))
  
  
  for (idata in 1:length(datasets)) {
    dataset = datasets[idata]
    print(dataset)
    
    #load(paste(dir_out,"/corre_", dataset, "_MSWEP.RData", sep = ""))
    load(paste(
      dir_out,
      "/mae_spi_",
      sc,
      "_",
      dataset,
      "_MSWEP.RData",
      sep = ""
    ))
    
    corre2 = corre[, which(latGPCP > -60 & latGPCP < 85)]
    corre_box[, idata] = as.vector(corre2)
    
  }
  
  plot_data=data.frame(corre_box)
  # plot_data <-
  #   data.frame(
  #     corre_box[, 1],
  #     corre_box[, 2],
  #     corre_box[, 3],
  #     corre_box[, 4],
  #     corre_box[, 5],
  #     corre_box[, 6],
  #     corre_box[, 7],
  #     corre_box[, 8],
  #     corre_box[, 9],
  #     corre_box[, 10]
  #   )
  
  setEPS()
  postscript(
    file.path(
      dir_out,
      paste("boxplot_mae_spi_", sc, "_mswep.eps", sep = "")
    ),
    horiz = FALSE,
    onefile = FALSE,
    width = 8.5,
    height = 5.5
  )
  boxplotperc(
    na.omit(plot_data),
    quant = c(0.025, 0.975),
    outline = FALSE,
    las = 2,
    ylim = c(0, 1.3),
    names = datasets
  )
  grid (NULL, NULL, lty = 6, col = "cornsilk2")
  dev.off()
  
}


