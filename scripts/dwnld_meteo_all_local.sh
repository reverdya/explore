#!/bin/bash

# Download all Explore2 climate projections
# A. Reverdy (14/02/2023)
# Launch by doing time bash dwnld_meteo_all.sh 2>&1 | tee info.log (do not add & or it will bug read), allows time measurement and output to info.log (overwrites) AND console
# First plug hard drive then open WSL

base_url_1=https://climatedata.umr-cnrm.fr/public/dcsc/projects/DRIAS/DRIAS2020_NEW/
base_url_2=https://climatedata.umr-cnrm.fr/public/dcsc/projects/DRIAS/EXPLORE2-Atmos_NEW/

cd /mnt/c/Users/reverdya/Documents/Docs/2_Data/raw/
#mkdir meteo/
cd meteo/
mkdir raw/
mkdir indic/
cd raw/

var=( tasAdjust prtotAdjust prsnAdjust evspsblpotAdjust )

gcm=( CNRM-CM5-LR EC-EARTH HadGEM2-ES IPSL-CM5A-MR MPI-ESM-LR NorESM1-M )
rcm=( ALADIN63 RACMO22E RCA4 CCLM4-8-17 RegCM4-6 WRF381P HIRHAM5 REMO2009 REMO2015 HadREM3-GA7-05 )
gcm2=( CNRM-CERFACS-CNRM-CM5 ICHEC-EC-EARTH MOHC-HadGEM2-ES IPSL-IPSL-CM5A-MR MPI-M-MPI-ESM-LR NCC-NorESM1-M )
rcm2=( CNRM-ALADIN63 KNMI-RACMO22E SMHI-RCA4 CLMcom-CCLM4-8-17 ICTP-RegCM4-6 IPSL-WRF381P DMI-HIRHAM5 MPI-CSC-REMO2009 GERICS-REMO2015 MOHC-HadREM3-GA7-05 )
rcp=( historical rcp26 rcp45 rcp85 )



###################################################
## Scrap url 1 and 2 and download everything

wget --recursive --no-parent --random-wait --wait 0.05 --no-http-keep-alive --level=inf --accept 'tasAdjust*nc','prtotAdjust*nc','prsnAdjust*nc','evspsblpotAdjust*nc' --reject '*FAO.nc' --no-directories --spider --no-verbose -e robots=off $base_url_1 2>&1 | sort | uniq | grep -oe 'https[^ ]*nc' > list_url_1.txt
wget --recursive --no-parent --random-wait --wait 0.05 --no-http-keep-alive --level=inf --accept 'tasAdjust*nc','prtotAdjust*nc','prsnAdjust*nc','evspsblpotAdjust*nc' --reject '*FAO.nc' --no-directories --spider --no-verbose -e robots=off $base_url_2 2>&1 | sort | uniq | grep -oe 'https[^ ]*nc' > list_url_2.txt
cat *.txt > list_url.txt
rm list_url_1.txt
rm list_url_2.txt

cat list_url.txt | xargs -n10 -P4 wget #wget called with 10 urls at a time and run 4 processes in parallel
#wget -i list_url.txt #not parallel
rm *EC-EARTH*HIRHAM5*

###################################################
## Reorganize and rename everything


for file in *.nc; do
mv "$file" "${file/France/FR}"
done
for file in *CDFt*.nc; do
mv "$file" "${file/LSCE-IPSL-CDFt-L-1V-0L-1976-2005/CDFt}"
done
for file in *ADAMONT*.nc; do
mv "$file" "${file/MF-ADAMONT-SAFRAN-1980-2011/ADAMONT}"
done

for G in "${!gcm[@]}"; do
for file in *${gcm2[$G]}*.nc; do
mv "$file" "${file/${gcm2[$G]}/${gcm[$G]}}"
done
done
for R in "${!rcm[@]}"; do
for file in *${rcm2[$R]}*.nc; do
mv "$file" "${file/${rcm2[$R]}/${rcm[$R]}}"
done
done

for file in *.nc; do
mv "$file" "${file/_r[0-9]?([0-9])i[0-9]p[0-9]_/_}"
done
for file in *.nc; do
mv "$file" "${file/_v[0-9]_/_}"
done

for file in *.nc; do
mv "$file" "${file/[0-9][0-9][0-9][0-9]-/-}"
done
for file in evspsblpotAdjust*.nc; do
mv "$file" "${file/[0-9][0-9][0-9][0-9]_/_}"
done
for file in tasAdjust*.nc; do
mv "$file" "${file/[0-9][0-9][0-9][0-9].nc/.nc}"
done
for file in prsnAdjust*.nc; do
mv "$file" "${file/[0-9][0-9][0-9][0-9].nc/.nc}"
done
for file in prtotAdjust*.nc; do
mv "$file" "${file/[0-9][0-9][0-9][0-9].nc/.nc}"
done


mkdir tmp
for P in "${!rcp[@]}"; do
for file in *${rcp[$P]}*; do
mv "$file" ./tmp/"${file/_${rcp[$P]}_/_}"
done
cd tmp
for file in *nc; do
mv "$file" ../"${file/_FR_/_FR_${rcp[$P]}_}"
done
cd ..
done
rm -r tmp

for V in "${!var[@]}"; do

mkdir ${var[$V]}
mv ${var[$V]}*nc ${var[$V]}/

done

unset base_url_1 #removing variable
unset base_url_2
unset full_path
unset file_name
unset gcm
unset rcm
unset rcp
unset var
unset gcm2
unset rcm2
unset G
unset R
unset P
unset V


