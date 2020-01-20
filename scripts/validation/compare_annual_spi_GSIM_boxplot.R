rm(list = ls())
graphics.off()
gc()

library(StatDA)


source("/Users/marco/Dropbox/estcena/scripts/BSC/R/common/ColorBarM.R")
source("/Users/marco/Dropbox/estcena/scripts/BSC/R/common/mioplot_global.R")
source("/Users/marco/Dropbox/estcena/scripts/BSC/R/common/CorrMIO.R")


time_scale = c(12)


## fixed parameters


dir_oss = '/Users/marco/Documents/dati/obs'
dir_out = '/Users/marco/Documents/output/obs_uncertainties'







datasets = c('CAMS_OPI',
             'CHIRPS',
             'CPC',
             'ERA5',
             'GPCC',
             'GPCP',
             'JRA55',
             'MERRA2',
             'MSWEP',
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
           # nrow = ni * nj,
           nrow= 4953,
           ncol = length(datasets))
  
  
  for (idata in 1:length(datasets)) {
    dataset = datasets[idata]
    print(dataset)
    load(paste(
      dir_out,
      "/corre_spi_",
      sc,
      "_",
      dataset,
      "_gsim_station.RData",
      sep = ""
    ))
    
    # corre2 = corre[, which(latGPCP > -60 & latGPCP < 85)]
    # corre_box[, idata] = as.vector(corre2)
    corre_box[, idata] = as.vector(corre)
    
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
    file.path(dir_out, paste("boxplot_corre_spi_", sc, "_GSIM.eps", sep = "")),
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
    ylim = c(-0.33, 1.1),
    names = datasets
  )
  grid (NULL, NULL, lty = 6, col = "cornsilk2")
  dev.off()
  
}

