#dir_wrk=/Users/marco/Dropbox/estcena/scripts/obs_uncertainty

##previous month
pm=$(date -d "$(date +%Y-%m-1) -1 month" +%-m)
echo $pm
pm0=$(printf "%02d" $pm)
py=$(date -d "$(date +%Y-%m-1) -1 month" +%-Y)

## CAMS_OPI
#version=v0208
#dir_obs="/data/disk1/CAMS_OPI/cams_opi_"$version
#cd $dir_obs
#cdo settaxis,1979-01-15,00:00,1month camsopi.nc camsopi_timecorrect.nc 
#cdo -f nc remapcon,/data/disk1/GPCPv2_3/griddes.txt $dir_obs/camsopi_timecorrect.nc $dir_obs/camsopi_timecorrect-2.5.nc 
#cdo selyear,1981/$py $dir_obs/camsopi_timecorrect-2.5.nc $dir_obs/camsopi_timecorrect-2.5-1981-$py.nc 

## CHIRPS
# dir_obs="/data/disk1/CHIRPS"
# cdo -f nc remapcon,/data/disk1/GPCPv2_3/griddes.txt $dir_obs/chirps-v2.0.monthly.nc $dir_obs/chirps-v2.0.monthly-2.5.nc 
# cdo -f nc remapcon,/data/disk1/GPCPv2_3/griddes.txt $dir_obs/chirps-v2.0.$py.$pm0.nc $dir_obs/chirps-v2.0.$py.$pm0-2.5.nc
#cdo cat $dir_obs/chirps-v2.0.monthly-2.5.nc $dir_obs/chirps-v2.0.$py.$pm0-2.5.nc $dir_obs/ofile.nc
#mv $dir_obs/ofile.nc $dir_obs/chirps-v2.0.monthly-2.5.nc

## cpc_global_precip
#dir_obs=/data/disk1/CPC_GLOBAL_PRECIP
#cd $dir_obs
##rm precip_1981_2019_monthly.nc
##cdo cat *.nc ofile.nc
##cdo monsum ofile.nc ofile_monthly.nc
#var='precip' 
#nomefile=$var"."$py".nc"
#cdo monsum $nomefile ofile_monthly.nc
#cdo -f nc remapcon,/data/disk1/GPCPv2_3/griddes.txt ofile_monthly.nc ofile_monthly_25.nc
#cdo cat precip_1981_2019_monthly.nc ofile_monthly_25.nc ofile.nc
#mv ofile.nc precip_1981_"$py"_monthly.nc
#rm ofile*.nc


### ERA5
#INDIR="/data/disk1/ERA5/"
#OUTDIR="/data/disk1/ERA5/"
#cd $INDIR
#rm ERA5-drop.nc
#for iyear in {1981..2019}
#do
#for imonth in {01..12}
#do
#  f="prec-era5-"$imonth"-"$iyear".nc"
#  echo "Processing $f file..."
#  name=$(basename $f .nc)
#  #cdo -f nc remapcon,/data/disk1/GPCPv2_3/griddes.txt $f $name-drop.nc
#  cdo cat $name-drop.nc ERA5-drop.nc
#done
#done
#cdo -f nc remapcon,/data/disk1/GPCPv2_3/griddes.txt prec-era5-$pm-$py.nc prec-era5-$pm0-$py-drop.nc
#cdo cat prec-era5-$pm0-$py-drop.nc ERA5-drop.nc

## GPCC
#  dir_obs=/data/disk1/GPCCv2018
#  cd $dir_obs
#cdo -f nc remapcon,/data/disk1/GPCPv2_3/griddes.txt full_data_monthly_v2018_10.nc full_data_monthly_v2018_25.nc
#cdo selyear,1981/2016 full_data_monthly_v2018_25.nc ofile1.nc
#  cdo cat first_guess_monthly_2017_* first_guess_monthly_2018_* first_guess_monthly_2019_* first_guess_monthly_2020_* ofile2.nc 
#  cdo chname,p,precip ofile2.nc ofile22.nc
#  ncks -x -v s ofile22.nc ofile222.nc
#  ncks -x -v lon_bnds,lat_bnds,numgauge,infilled_numgauges,interpolation_error,interpolation_error_infilled,diff_new_old_method ofile1.nc ofile11.nc
#  cdo -f nc remapcon,/data/disk1/GPCPv2_3/griddes.txt ofile222.nc ofile2222.nc
#  cdo cat ofile11.nc ofile2222.nc prec_1981_$py.nc
#  rm ofile*

## GPCP
# dir_obs=/data/disk1/GPCPv2_3
# cd $dir_obs
#rm gpcp_cdr_v23rB1_1981_2019.nc
#rm gpcp_cdr_v23rB1_1981_$py.nc
# cdo cat gpcp_cdr_v23rB1_*.nc gpcp_icdr_v23rB1_*.nc ofile1.nc
# cdo selyear,1981/$py ofile1.nc ofile2.nc
# cdo muldpm ofile2.nc gpcp_cdr_v23rB1_1981_$py.nc
# rm ofile*

## JRA55
#cd /data/disk1/JRA55
#rm JRA55_1981_2019_25_monthly.nc
#cdo cat *.nc ofile.nc
#cdo -f nc remapcon,/data/disk1/GPCPv2_3/griddes.txt ofile.nc ofile2.nc
#cdo muldpm ofile2.nc "JRA55_1981_"$py"_25_monthly.nc"
#rm ofile*

## MERRA-2
# dir_obs=/data/disk1/MERRA2
# cd $dir_obs
# rm MERRA2_1981_2019_25_monthly.nc
# cdo cat *.nc ofile.nc
# cdo selyear,1981/$py ofile.nc ofile2.nc
# cdo -f nc remapcon,/data/disk1/GPCPv2_3/griddes.txt ofile2.nc ofile3.nc
# cdo monsum ofile3.nc ofile4.nc
# cdo muldpm ofile4.nc ofile5.nc
# cdo mulc,86400 ofile5.nc "MERRA2_1981_"$py"_25_monthly.nc"
# rm ofile*

#cdo -f nc remapcon,/data/disk1/GPCPv2_3/griddes.txt MERRA2_400.tavgU_2d_lnd_Nx.201912.nc4.nc ofile3.nc
#cdo monsum ofile3.nc ofile4.nc
#cdo muldpm ofile4.nc ofile5.nc
#cdo cat ofile5.nc MERRA2_1981_2019_25_monthly.nc


## NCEP
# cd /data/disk1/NCEP
# cdo selyear,1981/$py prate.sfc.mon.mean.nc "prate_1981_"$py".nc"
# cdo -f nc remapcon,/data/disk1/GPCPv2_3/griddes.txt "prate_1981_"$py".nc" "prate_1981_"$py"_monthly.nc"


## PREC/L
# dir_obs=/data/disk1/PRECL
# cd $dir_obs
# cdo selyear,1981/$py precip.mon.mean.2.5x2.5.nc ofile1.nc
# cdo muldpm ofile1.nc ofile2.nc
# cdo -f nc remapcon,/data/disk1/GPCPv2_3/griddes.txt ofile2.nc "precip.mon.mean.2.5x2.5.1981_"$py".nc"
# rm ofile*


#################### 


## MSWEP
# dir_obs=/Users/marco/Documents/dati/obs/MSWEP
# cd $dir_obs
# #cdo selyear,1981/2015 $dir_obs/global_monthly_050deg.nc.nc4 ofile.nc
# cdo -f nc remapcon,/Users/marco/Documents/dati/obs/GPCPv2_3/griddes.txt $dir_obs/global_monthly_050deg.nc.nc4 $dir_obs/global_monthly_2.50deg.nc.nc4
# rm $dir_obs/ofile*
# 
# cd $dir_wrk



## GRACE
# dir_obs=/Users/marco/Documents/dati/GRACE
# cdo -f nc remapcon,/Users/marco/Documents/dati/obs/GPCPv2_3/griddes.txt $dir_obs/GRACE_DSI_JPLMasconV2_2002_2017.nc $dir_obs/GRACE_DSI_JPLMasconV2_2002_2017_25.nc

## ESACCI SM
# dir_obs=/Users/marco/Documents/dati/esacci_sm_v045/ESACCI-SOILMOISTURE-L3S-SSMV-COMBINED_1978-2018-v04.5

# for year in $(seq 1981 2016)
# do
#  cd $dir_obs/$year
#  cdo cat *.nc ofile1.nc
#  cdo yearmean ofile1.nc ofile2.nc
#  cdo -f nc remapcon,/Users/marco/Documents/dati/obs/GPCPv2_3/griddes.txt ofile2.nc $dir_obs/"ESACCI-SM-"$year".nc"
#  rm ofile*
# done

# for year in $(seq 1981 2016)
# do
#  cd $dir_obs/$year
#  cdo cat *.nc ofile1.nc
#  cdo monmean ofile1.nc ofile2.nc
#  cdo -f nc remapcon,/Users/marco/Documents/dati/obs/GPCPv2_3/griddes.txt ofile2.nc $dir_obs/"ESACCI-SM-"$year"_monthly.nc"
#  rm ofile*
# done

