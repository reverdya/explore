#!/bin/bash

# Calculate indicators
# A. Reverdy (20/02/2023)
# Launch by doing time bash calc_indic_meteo_local.sh 2>&1 | tee info.log (do not add & or it will bug read), allows time measurement and output to info.log (overwrites) AND console
# First plug hard drive then open WSL
# This is for a grouping of old scripts: some bugs could remain. Please check that all files got processed

cd /mnt/c/Users/reverdya/Documents/Docs/2_Data/raw/meteo/indic/

var=( tasAdjust prtotAdjust prsnAdjust evspsblpotAdjust )

rcp3=( rcp26 rcp45 rcp85 )

###################################################
## Altitude mask 1000m
mkdir masks
cdo -griddes -selgrid,1 ../raw/tasAdjust/tasAdjust_FR_rcp26_CNRM-CM5-LR_ALADIN63_ADAMONT_day_2006-2100.nc > ./masks/mygrid

cdo gec,1000 ../height_SAFRAN-France.nc tmp.nc #0 if False, 1 if True
cdo setctomiss,0 tmp.nc ./masks/mask_alti1000.nc
rm tmp.nc


###################################################
## Indicators

for V in "${!var[@]}"
do
mkdir ${var[$V]}
cd ${var[$V]}

FILES="/mnt/c/Users/reverdya/Documents/Docs/2_Data/raw/meteo/raw/${var[$V]}/*historical*"
FILES="/mnt/c/Users/reverdya/Documents/Docs/2_Data/raw/meteo/raw/${var[$V]}/*historical*CDFt*"
for f in $FILES
do

for P in "${!rcp3[@]}"
do

f_rcp="${f/historical/${rcp3[$P]}}"
f_rcp="${f_rcp%_day_*}" 
if [ "${var[$V]}" == evspsblpotAdjust"" ]; then
if ([[ "$f_rcp" == *"HadGEM2-ES"* ]])||([[ "$f_rcp" == *"EC-EARTH"* ]] && [[ "$f_rcp" == *"HadREM3-GA7-05"* ]]&& [[ "$f_rcp" == *"rcp26"* ]]);then
f_rcp=${f_rcp}_day_2006-2099_Hg0175.nc
else
f_rcp=${f_rcp}_day_2006-2100_Hg0175.nc
fi
else
if ([[ "$f_rcp" == *"HadGEM2-ES"* ]])||([[ "$f_rcp" == *"EC-EARTH"* ]] && [[ "$f_rcp" == *"HadREM3-GA7-05"* ]]&& [[ "$f_rcp" == *"rcp26"* ]]);then
f_rcp=${f_rcp}_day_2006-2099.nc
else
f_rcp=${f_rcp}_day_2006-2100.nc
fi
fi

test if combination exists
if test -f "$f_rcp"; then

cdo mergetime "$f" "$f_rcp" tmp.nc
f_rcp=${f_rcp##*/}



if [ "${var[$V]}" == "tasAdjust" ]; then

cdo yearmean tmp.nc tmp2.nc
cdo seasmean tmp.nc tmp3.nc
cdo monmean tmp.nc tmp6.nc
last=$(cdo -ntime tmp3.nc)
cdo delete,timestep=1,$last tmp3.nc tmp4.nc
if ([[ "$f_rcp" == *"CNRM-CM5-LR"* ]] && [[ "$f_rcp" == *"HadREM3-GA7-05"* ]])||([[ "$f_rcp" == *"HadGEM2-ES"* ]] && [[ "$f_rcp" == *"CCLM4-8-17"* ]]&& [[ "$f_rcp" == *"rcp45"* ]])||([[ "$f_rcp" == *"HadGEM2-ES"* ]] && [[ "$f_rcp" == *"HadREM3-GA7-05"* ]])||([[ "$f_rcp" == *"MPI-ESM-LR"* ]] && [[ "$f_rcp" == *"RegCM4-6"* ]]);then
last=$(cdo -ntime tmp2.nc)
cdo delete,timestep=$last tmp2.nc tmp5.nc
mv tmp5.nc tmp2.nc
last=$(cdo -ntime tmp6.nc)
cdo delete,timestep=$last tmp6.nc tmp7.nc
mv tmp7.nc tmp6.nc
fi
rm tmp.nc
rm tmp3.nc

if [[ "$f_rcp" == *"HadGEM2-ES"* ]] && [[ "$f_rcp" == *"RegCM4-6"* ]];then
mv tmp2.nc "${f_rcp/%_day_2006-2099.nc/_yearmean_1971-2099.nc}"
mv tmp4.nc "${f_rcp/%_day_2006-2099.nc/_seasmean_1971-2099.nc}"
mv tmp6.nc "${f_rcp/%_day_2006-2099.nc/_monmean_1971-2099.nc}"
cdo splitseas "${f_rcp/%_day_2006-2099.nc/_seasmean_1971-2099.nc}" "${f_rcp/%_day_2006-2099.nc/_seasmean_1971-2099_}"
rm "${f_rcp/%_day_2006-2099.nc/_seasmean_1971-2099.nc}"
cdo splitmon "${f_rcp/%_day_2006-2099.nc/_monmean_1971-2099.nc}" "${f_rcp/%_day_2006-2099.nc/_monmean_1971-2099_}"
rm "${f_rcp/%_day_2006-2099.nc/_monmean_1971-2099.nc}"
elif ([[ "$f_rcp" == *"HadGEM2-ES"* ]] && [[ "$f_rcp" == *"CCLM4-8-17"* ]]) || ([[ "$f_rcp" == *"HadGEM2-ES"* ]] && [[ "$f_rcp" == *"ALADIN63"* ]]);then
mv tmp2.nc "${f_rcp/%_day_2006-2099.nc/_yearmean_1950-2099.nc}"
mv tmp4.nc "${f_rcp/%_day_2006-2099.nc/_seasmean_1950-2099.nc}"
mv tmp6.nc "${f_rcp/%_day_2006-2099.nc/_monmean_1950-2099.nc}"
cdo splitseas "${f_rcp/%_day_2006-2099.nc/_seasmean_1950-2099.nc}" "${f_rcp/%_day_2006-2099.nc/_seasmean_1950-2099_}"
rm "${f_rcp/%_day_2006-2099.nc/_seasmean_1950-2099.nc}"
cdo splitmon "${f_rcp/%_day_2006-2099.nc/_monmean_1950-2099.nc}" "${f_rcp/%_day_2006-2099.nc/_monmean_1950-2099_}"
rm "${f_rcp/%_day_2006-2099.nc/_monmean_1950-2099.nc}"
elif ([[ "$f_rcp" == *"RCA4"* ]]) || ([[ "$f_rcp" == *"MPI-ESM-LR"* ]] && [[ "$f_rcp" == *"RegCM4-6"* ]]);then
mv tmp2.nc "${f_rcp/%_day_2006-2100.nc/_yearmean_1970-2100.nc}"
mv tmp4.nc "${f_rcp/%_day_2006-2100.nc/_seasmean_1970-2100.nc}"
mv tmp6.nc "${f_rcp/%_day_2006-2100.nc/_monmean_1970-2100.nc}"
cdo splitseas "${f_rcp/%_day_2006-2100.nc/_seasmean_1970-2100.nc}" "${f_rcp/%_day_2006-2100.nc/_seasmean_1970-2100_}"
rm "${f_rcp/%_day_2006-2100.nc/_seasmean_1970-2100.nc}"
cdo splitmon "${f_rcp/%_day_2006-2100.nc/_monmean_1970-2100.nc}" "${f_rcp/%_day_2006-2100.nc/_monmean_1970-2100_}"
rm "${f_rcp/%_day_2006-2100.nc/_monmean_1970-2100.nc}"
elif ([[ "$f_rcp" == *"CNRM-CM5-LR"* ]] && [[ "$f_rcp" == *"ALADIN63"* ]]) || ([[ "$f_rcp" == *"IPSL-CM5A-MR"* ]] && [[ "$f_rcp" == *"WRF381P"* ]]) || ([[ "$f_rcp" == *"NorESM1-M"* ]] && [[ "$f_rcp" == *"HIRHAM5"* ]]) || ([[ "$f_rcp" == *"NorESM1-M"* ]] && [[ "$f_rcp" == *"WRF381P"* ]]) || ([[ "$f_rcp" == *"IPSL-CM5A-MR"* ]] && [[ "$f_rcp" == *"HIRHAM5"* ]]) || ([[ "$f_rcp" == *"EC-EARTH"* ]] && [[ "$f_rcp" == *"HIRHAM5"* ]]);then
mv tmp2.nc "${f_rcp/%_day_2006-2100.nc/_yearmean_1951-2100.nc}"
mv tmp4.nc "${f_rcp/%_day_2006-2100.nc/_seasmean_1951-2100.nc}"
mv tmp6.nc "${f_rcp/%_day_2006-2100.nc/_monmean_1951-2100.nc}"
cdo splitseas "${f_rcp/%_day_2006-2100.nc/_seasmean_1951-2100.nc}" "${f_rcp/%_day_2006-2100.nc/_seasmean_1951-2100_}"
rm "${f_rcp/%_day_2006-2100.nc/_seasmean_1951-2100.nc}"
cdo splitmon "${f_rcp/%_day_2006-2100.nc/_monmean_1951-2100.nc}" "${f_rcp/%_day_2006-2100.nc/_monmean_1951-2100_}"
rm "${f_rcp/%_day_2006-2100.nc/_monmean_1951-2100.nc}"
elif ([[ "$f_rcp" == *"CNRM-CM5-LR"* ]] && [[ "$f_rcp" == *"HadREM3-GA7-05"* ]]) || ([[ "$f_rcp" == *"EC-EARTH"* ]] && [[ "$f_rcp" == *"HadREM3-GA7-05"* ]] && [[ "$f_rcp" == *"rcp85"* ]]);then
mv tmp2.nc "${f_rcp/%_day_2006-2100.nc/_yearmean_1952-2100.nc}"
mv tmp4.nc "${f_rcp/%_day_2006-2100.nc/_seasmean_1952-2100.nc}"
mv tmp6.nc "${f_rcp/%_day_2006-2100.nc/_monmean_1952-2100.nc}"
cdo splitseas "${f_rcp/%_day_2006-2100.nc/_seasmean_1952-2100.nc}" "${f_rcp/%_day_2006-2100.nc/_seasmean_1952-2100_}"
rm "${f_rcp/%_day_2006-2100.nc/_seasmean_1952-2100.nc}"
cdo splitmon "${f_rcp/%_day_2006-2100.nc/_monmean_1952-2100.nc}" "${f_rcp/%_day_2006-2100.nc/_monmean_1952-2100_}"
rm "${f_rcp/%_day_2006-2100.nc/_monmean_1952-2100.nc}"
elif ([[ "$f_rcp" == *"HadGEM2-ES"* ]] && [[ "$f_rcp" == *"HadREM3-GA7-05"* ]])||([[ "$f_rcp" == *"EC-EARTH"* ]] && [[ "$f_rcp" == *"HadREM3-GA7-05"* ]] && [[ "$f_rcp" == *"rcp26"* ]]);then
mv tmp2.nc "${f_rcp/%_day_2006-2099.nc/_yearmean_1952-2099.nc}"
mv tmp4.nc "${f_rcp/%_day_2006-2099.nc/_seasmean_1952-2099.nc}"
mv tmp6.nc "${f_rcp/%_day_2006-2099.nc/_monmean_1952-2099.nc}"
cdo splitseas "${f_rcp/%_day_2006-2099.nc/_seasmean_1952-2099.nc}" "${f_rcp/%_day_2006-2099.nc/_seasmean_1952-2099_}"
rm "${f_rcp/%_day_2006-2099.nc/_seasmean_1952-2099.nc}"
cdo splitmon "${f_rcp/%_day_2006-2099.nc/_monmean_1952-2099.nc}" "${f_rcp/%_day_2006-2099.nc/_monmean_1952-2099_}"
rm "${f_rcp/%_day_2006-2099.nc/_monmean_1952-2099.nc}"
else
mv tmp2.nc "${f_rcp/%_day_2006-2100.nc/_yearmean_1950-2100.nc}"
mv tmp4.nc "${f_rcp/%_day_2006-2100.nc/_seasmean_1950-2100.nc}"
mv tmp6.nc "${f_rcp/%_day_2006-2100.nc/_monmean_1950-2100.nc}"
cdo splitseas "${f_rcp/%_day_2006-2100.nc/_seasmean_1950-2100.nc}" "${f_rcp/%_day_2006-2100.nc/_seasmean_1950-2100_}"
rm "${f_rcp/%_day_2006-2100.nc/_seasmean_1950-2100.nc}"
cdo splitmon "${f_rcp/%_day_2006-2100.nc/_monmean_1950-2100.nc}" "${f_rcp/%_day_2006-2100.nc/_monmean_1950-2100_}"
rm "${f_rcp/%_day_2006-2100.nc/_monmean_1950-2100.nc}"
fi
fi


if ([[ "${var[$V]}" == "prtotAdjust" ]]);then

cdo mulc,86400 tmp.nc tmp1.nc
rm tmp.nc
cdo yearsum tmp1.nc tmp2.nc
cdo seassum tmp1.nc tmp3.nc
cdo monsum tmp1.nc tmp6.nc
last=$(cdo -ntime tmp3.nc)
cdo delete,timestep=1,$last tmp3.nc tmp4.nc
if ([[ "$f_rcp" == *"CNRM-CM5-LR"* ]] && [[ "$f_rcp" == *"HadREM3-GA7-05"* ]])||([[ "$f_rcp" == *"HadGEM2-ES"* ]] && [[ "$f_rcp" == *"CCLM4-8-17"* ]]&& [[ "$f_rcp" == *"rcp45"* ]])||([[ "$f_rcp" == *"HadGEM2-ES"* ]] && [[ "$f_rcp" == *"HadREM3-GA7-05"* ]])||([[ "$f_rcp" == *"MPI-ESM-LR"* ]] && [[ "$f_rcp" == *"RegCM4-6"* ]]);then
last=$(cdo -ntime tmp2.nc)
cdo delete,timestep=$last tmp2.nc tmp5.nc
mv tmp5.nc tmp2.nc
last=$(cdo -ntime tmp6.nc)
cdo delete,timestep=$last tmp6.nc tmp7.nc
mv tmp7.nc tmp6.nc
fi
rm tmp1.nc
rm tmp3.nc


if [[ "$f_rcp" == *"HadGEM2-ES"* ]] && [[ "$f_rcp" == *"RegCM4-6"* ]];then
mv tmp2.nc "${f_rcp/%_day_2006-2099.nc/_yearsum_1971-2099.nc}"
mv tmp4.nc "${f_rcp/%_day_2006-2099.nc/_seassum_1971-2099.nc}"
mv tmp6.nc "${f_rcp/%_day_2006-2099.nc/_monsum_1971-2099.nc}"
cdo splitseas "${f_rcp/%_day_2006-2099.nc/_seassum_1971-2099.nc}" "${f_rcp/%_day_2006-2099.nc/_seassum_1971-2099_}"
rm "${f_rcp/%_day_2006-2099.nc/_seassum_1971-2099.nc}"
cdo splitmon "${f_rcp/%_day_2006-2099.nc/_monsum_1971-2099.nc}" "${f_rcp/%_day_2006-2099.nc/_monsum_1971-2099_}"
rm "${f_rcp/%_day_2006-2099.nc/_monsum_1971-2099.nc}"
elif ([[ "$f_rcp" == *"HadGEM2-ES"* ]] && [[ "$f_rcp" == *"CCLM4-8-17"* ]]) || ([[ "$f_rcp" == *"HadGEM2-ES"* ]] && [[ "$f_rcp" == *"ALADIN63"* ]]);then
mv tmp2.nc "${f_rcp/%_day_2006-2099.nc/_yearsum_1950-2099.nc}"
mv tmp4.nc "${f_rcp/%_day_2006-2099.nc/_seassum_1950-2099.nc}"
mv tmp6.nc "${f_rcp/%_day_2006-2099.nc/_monsum_1950-2099.nc}"
cdo splitseas "${f_rcp/%_day_2006-2099.nc/_seassum_1950-2099.nc}" "${f_rcp/%_day_2006-2099.nc/_seassum_1950-2099_}"
rm "${f_rcp/%_day_2006-2099.nc/_seassum_1950-2099.nc}"
cdo splitmon "${f_rcp/%_day_2006-2099.nc/_monsum_1950-2099.nc}" "${f_rcp/%_day_2006-2099.nc/_monsum_1950-2099_}"
rm "${f_rcp/%_day_2006-2099.nc/_monsum_1950-2099.nc}"
elif ([[ "$f_rcp" == *"RCA4"* ]]) || ([[ "$f_rcp" == *"MPI-ESM-LR"* ]] && [[ "$f_rcp" == *"RegCM4-6"* ]]);then
mv tmp2.nc "${f_rcp/%_day_2006-2100.nc/_yearsum_1970-2100.nc}"
mv tmp4.nc "${f_rcp/%_day_2006-2100.nc/_seassum_1970-2100.nc}"
mv tmp6.nc "${f_rcp/%_day_2006-2100.nc/_monsum_1970-2100.nc}"
cdo splitseas "${f_rcp/%_day_2006-2100.nc/_seassum_1970-2100.nc}" "${f_rcp/%_day_2006-2100.nc/_seassum_1970-2100_}"
rm "${f_rcp/%_day_2006-2100.nc/_seassum_1970-2100.nc}"
cdo splitmon "${f_rcp/%_day_2006-2100.nc/_monsum_1970-2100.nc}" "${f_rcp/%_day_2006-2100.nc/_monsum_1970-2100_}"
rm "${f_rcp/%_day_2006-2100.nc/_monsum_1970-2100.nc}"
elif ([[ "$f_rcp" == *"CNRM-CM5-LR"* ]] && [[ "$f_rcp" == *"ALADIN63"* ]]) || ([[ "$f_rcp" == *"IPSL-CM5A-MR"* ]] && [[ "$f_rcp" == *"WRF381P"* ]]) || ([[ "$f_rcp" == *"NorESM1-M"* ]] && [[ "$f_rcp" == *"HIRHAM5"* ]]) || ([[ "$f_rcp" == *"NorESM1-M"* ]] && [[ "$f_rcp" == *"WRF381P"* ]]) || ([[ "$f_rcp" == *"IPSL-CM5A-MR"* ]] && [[ "$f_rcp" == *"HIRHAM5"* ]]) || ([[ "$f_rcp" == *"EC-EARTH"* ]] && [[ "$f_rcp" == *"HIRHAM5"* ]]);then
mv tmp2.nc "${f_rcp/%_day_2006-2100.nc/_yearsum_1951-2100.nc}"
mv tmp4.nc "${f_rcp/%_day_2006-2100.nc/_seassum_1951-2100.nc}"
mv tmp6.nc "${f_rcp/%_day_2006-2100.nc/_monsum_1951-2100.nc}"
cdo splitseas "${f_rcp/%_day_2006-2100.nc/_seassum_1951-2100.nc}" "${f_rcp/%_day_2006-2100.nc/_seassum_1951-2100_}"
rm "${f_rcp/%_day_2006-2100.nc/_seassum_1951-2100.nc}"
cdo splitmon "${f_rcp/%_day_2006-2100.nc/_monsum_1951-2100.nc}" "${f_rcp/%_day_2006-2100.nc/_monsum_1951-2100_}"
rm "${f_rcp/%_day_2006-2100.nc/_monsum_1951-2100.nc}"
elif ([[ "$f_rcp" == *"CNRM-CM5-LR"* ]] && [[ "$f_rcp" == *"HadREM3-GA7-05"* ]]) || ([[ "$f_rcp" == *"EC-EARTH"* ]] && [[ "$f_rcp" == *"HadREM3-GA7-05"* ]] && [[ "$f_rcp" == *"rcp85"* ]]);then
mv tmp2.nc "${f_rcp/%_day_2006-2100.nc/_yearsum_1952-2100.nc}"
mv tmp4.nc "${f_rcp/%_day_2006-2100.nc/_seassum_1952-2100.nc}"
mv tmp6.nc "${f_rcp/%_day_2006-2100.nc/_monsum_1952-2100.nc}"
cdo splitseas "${f_rcp/%_day_2006-2100.nc/_seassum_1952-2100.nc}" "${f_rcp/%_day_2006-2100.nc/_seassum_1952-2100_}"
rm "${f_rcp/%_day_2006-2100.nc/_seassum_1952-2100.nc}"
cdo splitmon "${f_rcp/%_day_2006-2100.nc/_monsum_1952-2100.nc}" "${f_rcp/%_day_2006-2100.nc/_monsum_1952-2100_}"
rm "${f_rcp/%_day_2006-2100.nc/_monsum_1952-2100.nc}"
elif ([[ "$f_rcp" == *"HadGEM2-ES"* ]] && [[ "$f_rcp" == *"HadREM3-GA7-05"* ]])||([[ "$f_rcp" == *"EC-EARTH"* ]] && [[ "$f_rcp" == *"HadREM3-GA7-05"* ]] && [[ "$f_rcp" == *"rcp26"* ]]);then
mv tmp2.nc "${f_rcp/%_day_2006-2099.nc/_yearsum_1952-2099.nc}"
mv tmp4.nc "${f_rcp/%_day_2006-2099.nc/_seassum_1952-2099.nc}"
mv tmp6.nc "${f_rcp/%_day_2006-2099.nc/_monsum_1952-2099.nc}"
cdo splitseas "${f_rcp/%_day_2006-2099.nc/_seassum_1952-2099.nc}" "${f_rcp/%_day_2006-2099.nc/_seassum_1952-2099_}"
rm "${f_rcp/%_day_2006-2099.nc/_seassum_1952-2099.nc}"
cdo splitmon "${f_rcp/%_day_2006-2099.nc/_monsum_1952-2099.nc}" "${f_rcp/%_day_2006-2099.nc/_monsum_1952-2099_}"
rm "${f_rcp/%_day_2006-2099.nc/_monsum_1952-2099.nc}"
else
mv tmp2.nc "${f_rcp/%_day_2006-2100.nc/_yearsum_1950-2100.nc}"
mv tmp4.nc "${f_rcp/%_day_2006-2100.nc/_seassum_1950-2100.nc}"
mv tmp6.nc "${f_rcp/%_day_2006-2100.nc/_monsum_1950-2100.nc}"
cdo splitseas "${f_rcp/%_day_2006-2100.nc/_seassum_1950-2100.nc}" "${f_rcp/%_day_2006-2100.nc/_seassum_1950-2100_}"
rm "${f_rcp/%_day_2006-2100.nc/_seassum_1950-2100.nc}"
cdo splitmon "${f_rcp/%_day_2006-2100.nc/_monsum_1950-2100.nc}" "${f_rcp/%_day_2006-2100.nc/_monsum_1950-2100_}"
rm "${f_rcp/%_day_2006-2100.nc/_monsum_1950-2100.nc}"
fi
fi



if ([[ "${var[$V]}" == "evspsblpotAdjust" ]]);then

cdo mulc,86400 tmp.nc tmp1.nc
rm tmp.nc
cdo yearsum tmp1.nc tmp2.nc
cdo seassum tmp1.nc tmp3.nc
cdo monsum tmp1.nc tmp6.nc
last=$(cdo -ntime tmp3.nc)
cdo delete,timestep=1,$last tmp3.nc tmp4.nc
if ([[ "$f_rcp" == *"CNRM-CM5-LR"* ]] && [[ "$f_rcp" == *"HadREM3-GA7-05"* ]])||([[ "$f_rcp" == *"HadGEM2-ES"* ]] && [[ "$f_rcp" == *"CCLM4-8-17"* ]]&& [[ "$f_rcp" == *"rcp45"* ]])||([[ "$f_rcp" == *"HadGEM2-ES"* ]] && [[ "$f_rcp" == *"HadREM3-GA7-05"* ]])||([[ "$f_rcp" == *"MPI-ESM-LR"* ]] && [[ "$f_rcp" == *"RegCM4-6"* ]]);then
last=$(cdo -ntime tmp2.nc)
cdo delete,timestep=$last tmp2.nc tmp5.nc
mv tmp5.nc tmp2.nc
last=$(cdo -ntime tmp6.nc)
cdo delete,timestep=$last tmp6.nc tmp7.nc
mv tmp7.nc tmp6.nc
fi
rm tmp1.nc
rm tmp3.nc


if [[ "$f_rcp" == *"HadGEM2-ES"* ]] && [[ "$f_rcp" == *"RegCM4-6"* ]];then
mv tmp2.nc "${f_rcp/%_day_2006-2099_Hg0175.nc/_yearsum_1971-2099.nc}"
mv tmp4.nc "${f_rcp/%_day_2006-2099_Hg0175.nc/_seassum_1971-2099.nc}"
mv tmp6.nc "${f_rcp/%_day_2006-2099_Hg0175.nc/_monsum_1971-2099.nc}"
cdo splitseas "${f_rcp/%_day_2006-2099_Hg0175.nc/_seassum_1971-2099.nc}" "${f_rcp/%_day_2006-2099_Hg0175.nc/_seassum_1971-2099_}"
rm "${f_rcp/%_day_2006-2099_Hg0175.nc/_seassum_1971-2099.nc}"
cdo splitmon "${f_rcp/%_day_2006-2099_Hg0175.nc/_monsum_1971-2099.nc}" "${f_rcp/%_day_2006-2099_Hg0175.nc/_monsum_1971-2099_}"
rm "${f_rcp/%_day_2006-2099_Hg0175.nc/_monsum_1971-2099.nc}"
elif ([[ "$f_rcp" == *"HadGEM2-ES"* ]] && [[ "$f_rcp" == *"CCLM4-8-17"* ]]) || ([[ "$f_rcp" == *"HadGEM2-ES"* ]] && [[ "$f_rcp" == *"ALADIN63"* ]]);then
mv tmp2.nc "${f_rcp/%_day_2006-2099_Hg0175.nc/_yearsum_1950-2099.nc}"
mv tmp4.nc "${f_rcp/%_day_2006-2099_Hg0175.nc/_seassum_1950-2099.nc}"
mv tmp6.nc "${f_rcp/%_day_2006-2099_Hg0175.nc/_monsum_1950-2099.nc}"
cdo splitseas "${f_rcp/%_day_2006-2099_Hg0175.nc/_seassum_1950-2099.nc}" "${f_rcp/%_day_2006-2099_Hg0175.nc/_seassum_1950-2099_}"
rm "${f_rcp/%_day_2006-2099_Hg0175.nc/_seassum_1950-2099.nc}"
cdo splitmon "${f_rcp/%_day_2006-2099_Hg0175.nc/_monsum_1950-2099.nc}" "${f_rcp/%_day_2006-2099_Hg0175.nc/_monsum_1950-2099_}"
rm "${f_rcp/%_day_2006-2099_Hg0175.nc/_monsum_1950-2099.nc}"
elif ([[ "$f_rcp" == *"RCA4"* ]]) || ([[ "$f_rcp" == *"MPI-ESM-LR"* ]] && [[ "$f_rcp" == *"RegCM4-6"* ]]);then
mv tmp2.nc "${f_rcp/%_day_2006-2100_Hg0175.nc/_yearsum_1970-2100.nc}"
mv tmp4.nc "${f_rcp/%_day_2006-2100_Hg0175.nc/_seassum_1970-2100.nc}"
mv tmp6.nc "${f_rcp/%_day_2006-2100_Hg0175.nc/_monsum_1970-2100.nc}"
cdo splitseas "${f_rcp/%_day_2006-2100_Hg0175.nc/_seassum_1970-2100.nc}" "${f_rcp/%_day_2006-2100_Hg0175.nc/_seassum_1970-2100_}"
rm "${f_rcp/%_day_2006-2100_Hg0175.nc/_seassum_1970-2100.nc}"
cdo splitmon "${f_rcp/%_day_2006-2100_Hg0175.nc/_monsum_1970-2100.nc}" "${f_rcp/%_day_2006-2100_Hg0175.nc/_monsum_1970-2100_}"
rm "${f_rcp/%_day_2006-2100_Hg0175.nc/_monsum_1970-2100.nc}"
elif ([[ "$f_rcp" == *"CNRM-CM5-LR"* ]] && [[ "$f_rcp" == *"ALADIN63"* ]]) || ([[ "$f_rcp" == *"IPSL-CM5A-MR"* ]] && [[ "$f_rcp" == *"WRF381P"* ]]) || ([[ "$f_rcp" == *"NorESM1-M"* ]] && [[ "$f_rcp" == *"HIRHAM5"* ]]) || ([[ "$f_rcp" == *"NorESM1-M"* ]] && [[ "$f_rcp" == *"WRF381P"* ]]) || ([[ "$f_rcp" == *"IPSL-CM5A-MR"* ]] && [[ "$f_rcp" == *"HIRHAM5"* ]]) || ([[ "$f_rcp" == *"EC-EARTH"* ]] && [[ "$f_rcp" == *"HIRHAM5"* ]]);then
mv tmp2.nc "${f_rcp/%_day_2006-2100_Hg0175.nc/_yearsum_1951-2100.nc}"
mv tmp4.nc "${f_rcp/%_day_2006-2100_Hg0175.nc/_seassum_1951-2100.nc}"
mv tmp6.nc "${f_rcp/%_day_2006-2100_Hg0175.nc/_monsum_1951-2100.nc}"
cdo splitseas "${f_rcp/%_day_2006-2100_Hg0175.nc/_seassum_1951-2100.nc}" "${f_rcp/%_day_2006-2100_Hg0175.nc/_seassum_1951-2100_}"
rm "${f_rcp/%_day_2006-2100_Hg0175.nc/_seassum_1951-2100.nc}"
cdo splitmon "${f_rcp/%_day_2006-2100_Hg0175.nc/_monsum_1951-2100.nc}" "${f_rcp/%_day_2006-2100_Hg0175.nc/_monsum_1951-2100_}"
rm "${f_rcp/%_day_2006-2100_Hg0175.nc/_monsum_1951-2100.nc}"
elif ([[ "$f_rcp" == *"CNRM-CM5-LR"* ]] && [[ "$f_rcp" == *"HadREM3-GA7-05"* ]]) || ([[ "$f_rcp" == *"EC-EARTH"* ]] && [[ "$f_rcp" == *"HadREM3-GA7-05"* ]] && [[ "$f_rcp" == *"rcp85"* ]]);then
mv tmp2.nc "${f_rcp/%_day_2006-2100_Hg0175.nc/_yearsum_1952-2100.nc}"
mv tmp4.nc "${f_rcp/%_day_2006-2100_Hg0175.nc/_seassum_1952-2100.nc}"
mv tmp6.nc "${f_rcp/%_day_2006-2100_Hg0175.nc/_monsum_1952-2100.nc}"
cdo splitseas "${f_rcp/%_day_2006-2100_Hg0175.nc/_seassum_1952-2100.nc}" "${f_rcp/%_day_2006-2100_Hg0175.nc/_seassum_1952-2100_}"
rm "${f_rcp/%_day_2006-2100_Hg0175.nc/_seassum_1952-2100.nc}"
cdo splitmon "${f_rcp/%_day_2006-2100_Hg0175.nc/_monsum_1952-2100.nc}" "${f_rcp/%_day_2006-2100_Hg0175.nc/_monsum_1952-2100_}"
rm "${f_rcp/%_day_2006-2100_Hg0175.nc/_monsum_1952-2100.nc}"
elif ([[ "$f_rcp" == *"HadGEM2-ES"* ]] && [[ "$f_rcp" == *"HadREM3-GA7-05"* ]])||([[ "$f_rcp" == *"EC-EARTH"* ]] && [[ "$f_rcp" == *"HadREM3-GA7-05"* ]] && [[ "$f_rcp" == *"rcp26"* ]]);then
mv tmp2.nc "${f_rcp/%_day_2006-2099_Hg0175.nc/_yearsum_1952-2099.nc}"
mv tmp4.nc "${f_rcp/%_day_2006-2099_Hg0175.nc/_seassum_1952-2099.nc}"
mv tmp6.nc "${f_rcp/%_day_2006-2099_Hg0175.nc/_monsum_1952-2099.nc}"
cdo splitseas "${f_rcp/%_day_2006-2099_Hg0175.nc/_seassum_1952-2099.nc}" "${f_rcp/%_day_2006-2099_Hg0175.nc/_seassum_1952-2099_}"
rm "${f_rcp/%_day_2006-2099_Hg0175.nc/_seassum_1952-2099.nc}"
cdo splitmon "${f_rcp/%_day_2006-2099_Hg0175.nc/_monsum_1952-2099.nc}" "${f_rcp/%_day_2006-2099_Hg0175.nc/_monsum_1952-2099_}"
rm "${f_rcp/%_day_2006-2099_Hg0175.nc/_monsum_1952-2099.nc}"
else
mv tmp2.nc "${f_rcp/%_day_2006-2100_Hg0175.nc/_yearsum_1950-2100.nc}"
mv tmp4.nc "${f_rcp/%_day_2006-2100_Hg0175.nc/_seassum_1950-2100.nc}"
mv tmp6.nc "${f_rcp/%_day_2006-2100_Hg0175.nc/_monsum_1950-2100.nc}"
cdo splitseas "${f_rcp/%_day_2006-2100_Hg0175.nc/_seassum_1950-2100.nc}" "${f_rcp/%_day_2006-2100_Hg0175.nc/_seassum_1950-2100_}"
rm "${f_rcp/%_day_2006-2100_Hg0175.nc/_seassum_1950-2100.nc}"
cdo splitmon "${f_rcp/%_day_2006-2100_Hg0175.nc/_monsum_1950-2100.nc}" "${f_rcp/%_day_2006-2100_Hg0175.nc/_monsum_1950-2100_}"
rm "${f_rcp/%_day_2006-2100_Hg0175.nc/_monsum_1950-2100.nc}"
fi
fi


if ([[ "${var[$V]}" == "prsnAdjust" ]]);then
cdo shifttime,2months tmp.nc tmp1.nc #to start year in november
rm tmp.nc

cdo -yearsum -select,season=JFMAMJ tmp1.nc tmp2.nc
rm tmp1.nc
cdo shifttime,-2months tmp2.nc tmp3.nc
rm tmp2.nc
cdo mulc,86400 tmp3.nc tmp4.nc
rm tmp3.nc
cdo selgrid,1 -setgrid,../masks/mygrid tmp4.nc tmp5.nc
rm tmp4.nc
cdo -mul tmp5.nc ../masks/mask_alti1000.nc tmp6.nc
rm tmp5.nc

last=$(cdo -ntime tmp6.nc)
cdo delete,timestep=1,$last tmp6.nc tmp7.nc
rm tmp6.nc

if [[ "$f_rcp" == *"HadGEM2-ES"* ]] && [[ "$f_rcp" == *"RegCM4-6"* ]];then
mv tmp7.nc "${f_rcp/%_day_2006-2099.nc/_NDJFMAsum_alti1000_1971-2099.nc}"
elif ([[ "$f_rcp" == *"HadGEM2-ES"* ]] && [[ "$f_rcp" == *"CCLM4-8-17"* ]]) || ([[ "$f_rcp" == *"HadGEM2-ES"* ]] && [[ "$f_rcp" == *"ALADIN63"* ]]);then
mv tmp7.nc "${f_rcp/%_day_2006-2099.nc/_NDJFMAsum_alti1000_1950-2099.nc}"
elif ([[ "$f_rcp" == *"RCA4"* ]]) || ([[ "$f_rcp" == *"MPI-ESM-LR"* ]] && [[ "$f_rcp" == *"RegCM4-6"* ]]);then
mv tmp7.nc "${f_rcp/%_day_2006-2100.nc/_NDJFMAsum_alti1000_1970-2100.nc}"
elif ([[ "$f_rcp" == *"CNRM-CM5-LR"* ]] && [[ "$f_rcp" == *"ALADIN63"* ]]) || ([[ "$f_rcp" == *"IPSL-CM5A-MR"* ]] && [[ "$f_rcp" == *"WRF381P"* ]]) || ([[ "$f_rcp" == *"NorESM1-M"* ]] && [[ "$f_rcp" == *"HIRHAM5"* ]]) || ([[ "$f_rcp" == *"NorESM1-M"* ]] && [[ "$f_rcp" == *"WRF381P"* ]]) || ([[ "$f_rcp" == *"IPSL-CM5A-MR"* ]] && [[ "$f_rcp" == *"HIRHAM5"* ]]) || ([[ "$f_rcp" == *"EC-EARTH"* ]] && [[ "$f_rcp" == *"HIRHAM5"* ]]);then
mv tmp7.nc "${f_rcp/%_day_2006-2100.nc/_NDJFMAsum_alti1000_1951-2100.nc}"
elif ([[ "$f_rcp" == *"CNRM-CM5-LR"* ]] && [[ "$f_rcp" == *"HadREM3-GA7-05"* ]]) || ([[ "$f_rcp" == *"EC-EARTH"* ]] && [[ "$f_rcp" == *"HadREM3-GA7-05"* ]] && [[ "$f_rcp" == *"rcp85"* ]]);then
mv tmp7.nc "${f_rcp/%_day_2006-2100.nc/_NDJFMAsum_alti1000_1952-2100.nc}"
elif ([[ "$f_rcp" == *"HadGEM2-ES"* ]] && [[ "$f_rcp" == *"HadREM3-GA7-05"* ]])||([[ "$f_rcp" == *"EC-EARTH"* ]] && [[ "$f_rcp" == *"HadREM3-GA7-05"* ]]&& [[ "$f_rcp" == *"rcp26"* ]]);then
mv tmp7.nc "${f_rcp/%_day_2006-2099.nc/_NDJFMAsum_alti1000_1952-2099.nc}"
else
mv tmp7.nc "${f_rcp/%_day_2006-2100.nc/_NDJFMAsum_alti1000_1950-2100.nc}"
fi
fi





fi
done		
done
cd ..
done



#########################################################
## Make masks zones

cdo selname,Basins_hydro ../SAFRAN_mask_bas_sect-hydro_bv_deptmt.nc ./masks/mask_bas.nc
cdo selname,Hydro_sectors ../SAFRAN_mask_bas_sect-hydro_bv_deptmt.nc ./masks/mask_sect.nc
cdo selname,Departments ../SAFRAN_mask_bas_sect-hydro_bv_deptmt.nc ./masks/mask_deptmt.nc
cdo selname,Watersheds ../SAFRAN_mask_bas_sect-hydro_bv_deptmt.nc ./masks/mask_bv.nc


###################################################################################
## Make indicators by zone

var=( tasAdjust prtotAdjust evspsblpotAdjust )

for V in "${!var[@]}"
do
cd ${var[$V]}

mkdir bas
mkdir sect
mkdir deptmt
mkdir bv

FILES="/mnt/c/Users/reverdya/Documents/Docs/2_Data/raw/meteo/indic/${var[$V]}/${var[$V]}*"
#FILES="/mnt/c/Users/reverdya/Documents/Docs/2_Data/raw/meteo/indic/${var[$V]}/${var[$V]}*CDFt*"
for f in $FILES
do
f_local=${f##*/}


cd bas
cdo -s -mul $f ../../masks/mask_bas.nc tmp_masked.nc #Warnings okay
cdo -s fldmean tmp_masked.nc "${f_local/%.nc/_bas.nc}" #Warnings okay
rm tmp_masked.nc
cd ..


cd sect
cdo -s -mul $f ../../masks/mask_sect.nc tmp_masked.nc #Warnings okay
cdo -s fldmean tmp_masked.nc "${f_local/%.nc/_sect.nc}" #Warnings okay
rm tmp_masked.nc
cd ..


cd deptmt
cdo -s -mul $f ../../masks/mask_deptmt.nc tmp_masked.nc #Warnings okay
cdo -s fldmean tmp_masked.nc "${f_local/%.nc/_deptmt.nc}" #Warnings okay
rm tmp_masked.nc
cd ..


cd bv
cdo -s -mul $f ../../masks/mask_bv.nc tmp_masked.nc #Warnings okay
cdo -s fldmean tmp_masked.nc "${f_local/%.nc/_bv.nc}" #Warnings okay
rm tmp_masked.nc
cd ..

done
cd ..
done

cd ..
mv -r ./indic/ ../../processed/Explore2-meteo/indic/





unset var
unset P
unset rcp3
unset V
unset FILES
unset f
unset f_rcp
unset f2
unset f_locals

