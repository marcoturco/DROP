#####################
cd /home/mt/Dropbox/estcena/scripts/obs_uncertainty/gitlab/drop/scripts/development
./get_data.sh
./process_datasets.sh
Rscript loop_load_SPI_RT.R
Rscript load_SPI_ENS_RT.R
Rscript drop2netcdf.R

#####################
update gitlab

#####################
update appp DROP and deploy it

