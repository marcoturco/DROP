#!/bin/bash

##previous month
# pm=$(date -d "$(date +%Y-%m-1) -1 month" +%-m)
# echo $pm
pm=4
# pm0=$(printf "%02d" $pm)
pm0=04
# py=$(date -d "$(date +%Y-%m-1) -1 month" +%-Y)
py=2020

# CAMSOPI
# version=v0208
# root=ftp://ftp.cpc.ncep.noaa.gov/precip/data-req/cams_opi_$version
# dir_obs="/data/disk1/CAMS_OPI/cams_opi_"$version
# dir_obs="/Users/marco/Documents/dati/obs/CAMS_OPI/cams_opi_"$version
# mkdir -p $dir_obs
# cd $dir_obs
#wget -q -N $root/cams_opi_merged.$py$pm0
#  wget -q -N $root/\*
#   wget -N $root/\*
# for file in cams_opi_merged.??????.gz; do
#    f=${file%.gz}
#    if [ ! -s $f -o $f -ot $file ]; then
#        gunzip -c $file > $f
#    fi
# done
## BE CAREFULL, 198603 is missing
# n=`ls cams_opi_merged.?????? | wc -l`
# n=$(( n + 1 ))
# echo "n=$n"
# grads -b -l <<EOF
# open camsopi.ctl
# set x 1 144
# set t 1 $n
# define prcp=comb
# define perc=gam
# set sdfwrite camsopi.nc
# sdfwrite prcp
# clear sdfwrite
# set sdfwrite camsopi_perc.nc
# sdfwrite perc
# clear sdfwrite
# quit
# EOF

## CHIRPS 
#dirwrk="/data/disk1/CHIRPS"
# dirwrk="/Users/marco/Documents/dati/obs/CHIRPS"
# cd $dirwrk
# rm chirps-v2.0.monthly.nc
# wget ftp://ftp.chg.ucsb.edu/pub/org/chg/products/CHIRPS-2.0/global_monthly/netcdf/chirps-v2.0.monthly.nc
# wget ftp://ftp.chg.ucsb.edu/pub/org/chg/products/CHIRPS-2.0/prelim/global_monthly/netcdf/chirps-v2.0.$py.$pm0.nc
##wget -c ftp://ftp.chg.ucsb.edu/pub/org/chg/products/CHIRPS-2.0/global_monthly/netcdf/chirps-v2.0.monthly.nc

## cpc_global_precip
# var='precip' 
# dirfile="ftp://ftp.cdc.noaa.gov/Datasets/cpc_global_precip/"
# # dirwrk="/data/disk1/CPC_GLOBAL_PRECIP"
# dirwrk="/Users/marco/Documents/dati/obs/CPC_GLOBAL_PRECIP"
# cd $dirwrk
# nomefile=$var"."$py".nc"
# echo $nomefile
# rm $nomefile
# wget $dirfile$nomefile  
# for year in $(seq 1998 2019)
# do
#     nomefile=$var"."$year".nc"
# 	rm $nomefile
#     echo $nomefile
# 	wget $dirfile$nomefile    
# done
 

## ERA5 
# y1=1984
# y2=1984
# vars='prec'
# dirout=/data/disk1/ERA5
# dirout=/Users/marco/Documents/dati/obs/ERA5
# mkdir -p $dirout
# cd $dirout
# for variable in $vars;do
# case $variable in
# 'prec') varlongname='total_precipitation' ;;
# esac
# for year in $(seq $y1 $y2);do
# for month in $(seq 6 6);do
# if [ $month -lt 10 ];then
# month=0$month
# fi
# year=$py
# month=$pm
# cat>kk4sed<<EOF
# s#SSSVARNAME#${varlongname}#g
# s#SSSYEAR#${year}#g
# s#SSSMONTH#${month}#g
# EOF
# sed -f kk4sed download-cds-era5-4sed.py > download-cds-era5.py
# rm kk4sed
# chmod u+x download-cds-era5.py
# python download-cds-era5.py
# mv download.nc $dirout/$variable-era5-$month-$year.nc
# done
# done
# done

## GPCCv2018
# dirfile="https://opendata.dwd.de/climate_environment/GPCC/first_guess"
# # dirwrk="/data/disk1/GPCCv2018"
# dirwrk="/Users/marco/Documents/dati/obs/GPCCv2018"
# cd $dirwrk
# #first_guess_monthly_2019_06.nc.gz 
# #ftp://ftp.dwd.de/pub/data/gpcc/first_guess/2013/first_guess_monthly_2013_01.nc.gz
# for year in $(seq $py $py)
# do
# 	for month in $(seq $pm $pm)
# 	do
# 	
# 	
# 	if [ $month -lt 10 ];         # If $i is smaller than 10
#     then
#             nomefile="$dirfile"/"$year"/first_guess_monthly_"$year"_0"$month".nc.gz	   			
#     else
#             nomefile="$dirfile"/"$year"/first_guess_monthly_"$year"_"$month".nc.gz
# 	fi
#         
#     echo $nomefile
# 	wget $nomefile    
# 	
# 	done
# done
# gunzip -f *.gz


## GPCPv2_3
# dirfile="http://eagle1.umd.edu/GPCP_CDR/Monthly_Data"
# # dirwrk="/data/disk1/GPCPv2_3"
# dirwrk="/Users/marco/Documents/dati/obs/GPCPv2_3"
# cd $dirwrk
# for year in $(seq 2019 2019)
# do
# 	for month in $(seq 9 12)
# 	do
# 	if [ $month -lt 10 ];         # If $i is smaller than 10
#     then
#             nomefile="$dirfile"/gpcp_cdr_v23rB1_y"$year"_m0"$month".nc	   			
#     else
#             nomefile="$dirfile"/gpcp_cdr_v23rB1_y"$year"_m"$month".nc	   			
# 	fi
#     echo $nomefile
# 	wget $nomefile    
# 	done
# done
# 
# wget http://eagle1.umd.edu/GPCP_ICDR/Data/gpcp_icdr_v23rB1_y2020_m01.nc
# wget http://eagle1.umd.edu/GPCP_ICDR/Data/gpcp_icdr_v23rB1_y2020_m02.nc
# wget http://eagle1.umd.edu/GPCP_ICDR/Data/gpcp_icdr_v23rB1_y2020_m03.nc
# wget "http://eagle1.umd.edu/GPCP_ICDR/Data/gpcp_icdr_v23rB1_y"$py"_m"$pm0".nc"

## JRA55 
#set -vx
#gribvar=tpratsfc
#var=prlr
#cd /data/disk1/JRA55
#for iyear in {2020..2020}
#do
#for imonth in {01..01}
#do
#file=fcst_phy2m125.$iyear$imonth
#ftp -n -v ds.data.jma.go.jp <<END_SCRIPT
#quote USER jra05856
#quote PASS Juan2020!!
#bin
#cd /data20/JRA-55/Hist/Monthly/fcst_phy2m125
#get $file
#quit
#END_SCRIPT
#date=`echo $file | rev | cut -b -6 | rev`
#cdo selvar,${gribvar} $file ${var}_${file} 
#cdo -f nc copy ${var}_${file} ${var}_${date}.nc 
#rm ${var}_${file} 
#ncrename -h -v $gribvar,$var ${var}_${date}.nc
#ncrename -h -v lon,longitude -v lat,latitude -d lon,longitude -d lat,latitude ${var}_${date}.nc
#done
#done

## MERRA2 TODO
## 1. If you have not already done so, please register!
## - Create an Earthdata account
## - Link GES DISC with your account
## - Verify by downloading this example data file URL
## 2. Download the list of links
## 3. Follow the instructions for wget or curl (https://disc.gsfc.nasa.gov/data-access#mac_linux_wget)
## 
## in linux this works:
#dir_obs=/data/disk1/MERRA2
# cd $dir_obs
# wget https://goldsmr4.gesdisc.eosdis.nasa.gov:443/opendap/MERRA2_DIURNAL/M2TUNXLND.5.12.4/2019/MERRA2_400.tavgU_2d_lnd_Nx.201912.nc4.nc?PRECTOTLAND,time,lat,lon
#mv MERRA2_400.tavgU_2d_lnd_Nx.201912.nc4.nc?PRECTOTLAND,time,lat,lon MERRA2_400.tavgU_2d_lnd_Nx.201912.nc4.nc


## NCEP
## cd /data/disk1/NCEP
# cd /Users/marco/Documents/dati/obs/NCEP
# rm prate.sfc.mon.mean.nc
# wget ftp://ftp.cdc.noaa.gov/Datasets/ncep.reanalysis2.derived/gaussian_grid/prate.sfc.mon.mean.nc

## PREC/L
 #dir_wrk="/data/disk1/PRECL"
#  dir_wrk="/Users/marco/Documents/dati/obs/PRECL"
#  cd $dir_wrk
#  rm precip.mon.mean.2.5x2.5.nc  
#  wget ftp://ftp.cdc.noaa.gov/Datasets/precl/2.5deg/precip.mon.mean.2.5x2.5.nc

