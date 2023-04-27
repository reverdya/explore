#!/bin/bash

# Calculate indicators
# A. Reverdy (27/04/2023)
# Launch by doing time bash calc_indic_safran.sh 2>&1 | tee info.log (do not add & or it will bug read), allows time measurement and output to info.log (overwrites) AND console
# First plug hard drive then open WSL
# This is for a grouping of old scripts: some bugs could remain. Please check that all files got processed

cd /mnt/c/Users/reverdya/Documents/Docs/2_Data/processed/safran/indic/

var=( tasAdjust prtotAdjust prsnAdjust evspsblpotAdjust )
rcp3=( rcp26 rcp45 rcp85 )


###################################################
## Indicators


FILES="/mnt/c/Users/reverdya/Documents/Docs/2_Data/raw/meteo/safran/regridded/*nc"
for f in $FILES
do

## extract tmp from $f

if [ tmp == "Tair" ]; then

cdo yearmean "$f" tmp2.nc
cdo seasmean tmp.nc tmp3.nc
last=$(cdo -ntime tmp3.nc)
cdo delete,timestep=1,$last tmp3.nc tmp4.nc
rm tmp3.nc


mv tmp2.nc "${f_rcp/%_day_2006-2099.nc/_yearmean_1971-2099.nc}"
mv tmp4.nc "${f_rcp/%_day_2006-2099.nc/_seasmean_1971-2099.nc}"
cdo splitseas "${f_rcp/%_day_2006-2099.nc/_seasmean_1971-2099.nc}" "${f_rcp/%_day_2006-2099.nc/_seasmean_1971-2099_}"
rm "${f_rcp/%_day_2006-2099.nc/_seasmean_1971-2099.nc}"
fi


if ([[ "${var[$V]}" == "prtotAdjust" ]]);then
cdo mulc,86400 tmp.nc tmp1.nc
rm tmp.nc
cdo yearsum tmp1.nc tmp2.nc
cdo seassum tmp1.nc tmp3.nc
last=$(cdo -ntime tmp3.nc)
cdo delete,timestep=1,$last tmp3.nc tmp4.nc
if ([[ "$f_rcp" == *"CNRM-CM5-LR"* ]] && [[ "$f_rcp" == *"HadREM3-GA7-05"* ]])||([[ "$f_rcp" == *"HadGEM2-ES"* ]] && [[ "$f_rcp" == *"CCLM4-8-17"* ]]&& [[ "$f_rcp" == *"rcp45"* ]])||([[ "$f_rcp" == *"HadGEM2-ES"* ]] && [[ "$f_rcp" == *"HadREM3-GA7-05"* ]])||([[ "$f_rcp" == *"MPI-ESM-LR"* ]] && [[ "$f_rcp" == *"RegCM4-6"* ]]);then
last=$(cdo -ntime tmp2.nc)
cdo delete,timestep=$last tmp2.nc tmp5.nc
mv tmp5.nc tmp2.nc
fi
rm tmp1.nc
rm tmp3.nc

if [[ "$f_rcp" == *"HadGEM2-ES"* ]] && [[ "$f_rcp" == *"RegCM4-6"* ]];then
mv tmp2.nc "${f_rcp/%_day_2006-2099.nc/_yearsum_1971-2099.nc}"
mv tmp4.nc "${f_rcp/%_day_2006-2099.nc/_seassum_1971-2099.nc}"
cdo splitseas "${f_rcp/%_day_2006-2099.nc/_seassum_1971-2099.nc}" "${f_rcp/%_day_2006-2099.nc/_seassum_1971-2099_}"
rm "${f_rcp/%_day_2006-2099.nc/_seassum_1971-2099.nc}"
elif ([[ "$f_rcp" == *"HadGEM2-ES"* ]] && [[ "$f_rcp" == *"CCLM4-8-17"* ]]) || ([[ "$f_rcp" == *"HadGEM2-ES"* ]] && [[ "$f_rcp" == *"ALADIN63"* ]]);then
mv tmp2.nc "${f_rcp/%_day_2006-2099.nc/_yearsum_1950-2099.nc}"
mv tmp4.nc "${f_rcp/%_day_2006-2099.nc/_seassum_1950-2099.nc}"
cdo splitseas "${f_rcp/%_day_2006-2099.nc/_seassum_1950-2099.nc}" "${f_rcp/%_day_2006-2099.nc/_seassum_1950-2099_}"
rm "${f_rcp/%_day_2006-2099.nc/_seassum_1950-2099.nc}"
elif ([[ "$f_rcp" == *"RCA4"* ]]) || ([[ "$f_rcp" == *"MPI-ESM-LR"* ]] && [[ "$f_rcp" == *"RegCM4-6"* ]]);then
mv tmp2.nc "${f_rcp/%_day_2006-2100.nc/_yearsum_1970-2100.nc}"
mv tmp4.nc "${f_rcp/%_day_2006-2100.nc/_seassum_1970-2100.nc}"
cdo splitseas "${f_rcp/%_day_2006-2100.nc/_seassum_1970-2100.nc}" "${f_rcp/%_day_2006-2100.nc/_seassum_1970-2100_}"
rm "${f_rcp/%_day_2006-2100.nc/_seassum_1970-2100.nc}"
elif ([[ "$f_rcp" == *"CNRM-CM5-LR"* ]] && [[ "$f_rcp" == *"ALADIN63"* ]]) || ([[ "$f_rcp" == *"IPSL-CM5A-MR"* ]] && [[ "$f_rcp" == *"WRF381P"* ]]) || ([[ "$f_rcp" == *"NorESM1-M"* ]] && [[ "$f_rcp" == *"HIRHAM5"* ]]) || ([[ "$f_rcp" == *"NorESM1-M"* ]] && [[ "$f_rcp" == *"WRF381P"* ]]) || ([[ "$f_rcp" == *"IPSL-CM5A-MR"* ]] && [[ "$f_rcp" == *"HIRHAM5"* ]]) || ([[ "$f_rcp" == *"EC-EARTH"* ]] && [[ "$f_rcp" == *"HIRHAM5"* ]]);then
mv tmp2.nc "${f_rcp/%_day_2006-2100.nc/_yearsum_1951-2100.nc}"
mv tmp4.nc "${f_rcp/%_day_2006-2100.nc/_seassum_1951-2100.nc}"
cdo splitseas "${f_rcp/%_day_2006-2100.nc/_seassum_1951-2100.nc}" "${f_rcp/%_day_2006-2100.nc/_seassum_1951-2100_}"
rm "${f_rcp/%_day_2006-2100.nc/_seassum_1951-2100.nc}"
elif ([[ "$f_rcp" == *"CNRM-CM5-LR"* ]] && [[ "$f_rcp" == *"HadREM3-GA7-05"* ]]) || ([[ "$f_rcp" == *"EC-EARTH"* ]] && [[ "$f_rcp" == *"HadREM3-GA7-05"* ]] && [[ "$f_rcp" == *"rcp85"* ]]);then
mv tmp2.nc "${f_rcp/%_day_2006-2100.nc/_yearsum_1952-2100.nc}"
mv tmp4.nc "${f_rcp/%_day_2006-2100.nc/_seassum_1952-2100.nc}"
cdo splitseas "${f_rcp/%_day_2006-2100.nc/_seassum_1952-2100.nc}" "${f_rcp/%_day_2006-2100.nc/_seassum_1952-2100_}"
rm "${f_rcp/%_day_2006-2100.nc/_seassum_1952-2100.nc}"
elif ([[ "$f_rcp" == *"HadGEM2-ES"* ]] && [[ "$f_rcp" == *"HadREM3-GA7-05"* ]])||([[ "$f_rcp" == *"EC-EARTH"* ]] && [[ "$f_rcp" == *"HadREM3-GA7-05"* ]]&& [[ "$f_rcp" == *"rcp26"* ]]);then
mv tmp2.nc "${f_rcp/%_day_2006-2099.nc/_yearsum_1952-2099.nc}"
mv tmp4.nc "${f_rcp/%_day_2006-2099.nc/_seassum_1952-2099.nc}"
cdo splitseas "${f_rcp/%_day_2006-2099.nc/_seassum_1952-2099.nc}" "${f_rcp/%_day_2006-2099.nc/_seassum_1952-2099_}"
rm "${f_rcp/%_day_2006-2099.nc/_seassum_1952-2099.nc}"
else
mv tmp2.nc "${f_rcp/%_day_2006-2100.nc/_yearsum_1950-2100.nc}"
mv tmp4.nc "${f_rcp/%_day_2006-2100.nc/_seassum_1950-2100.nc}"
cdo splitseas "${f_rcp/%_day_2006-2100.nc/_seassum_1950-2100.nc}" "${f_rcp/%_day_2006-2100.nc/_seassum_1950-2100_}"
rm "${f_rcp/%_day_2006-2100.nc/_seassum_1950-2100.nc}"
fi
fi



if ([[ "${var[$V]}" == "evspsblpotAdjust" ]]);then
cdo mulc,86400 tmp.nc tmp1.nc
rm tmp.nc
cdo yearsum tmp1.nc tmp2.nc
cdo seassum tmp1.nc tmp3.nc
last=$(cdo -ntime tmp3.nc)
cdo delete,timestep=1,$last tmp3.nc tmp4.nc
if ([[ "$f_rcp" == *"CNRM-CM5-LR"* ]] && [[ "$f_rcp" == *"HadREM3-GA7-05"* ]])||([[ "$f_rcp" == *"HadGEM2-ES"* ]] && [[ "$f_rcp" == *"CCLM4-8-17"* ]]&& [[ "$f_rcp" == *"rcp45"* ]])||([[ "$f_rcp" == *"HadGEM2-ES"* ]] && [[ "$f_rcp" == *"HadREM3-GA7-05"* ]])||([[ "$f_rcp" == *"MPI-ESM-LR"* ]] && [[ "$f_rcp" == *"RegCM4-6"* ]]);then
last=$(cdo -ntime tmp2.nc)
cdo delete,timestep=$last tmp2.nc tmp5.nc
mv tmp5.nc tmp2.nc
fi
rm tmp1.nc
rm tmp3.nc

if [[ "$f_rcp" == *"HadGEM2-ES"* ]] && [[ "$f_rcp" == *"RegCM4-6"* ]];then
mv tmp2.nc "${f_rcp/%_day_2006-2099_Hg0175.nc/_yearsum_1971-2099.nc}"
mv tmp4.nc "${f_rcp/%_day_2006-2099_Hg0175.nc/_seassum_1971-2099.nc}"
cdo splitseas "${f_rcp/%_day_2006-2099_Hg0175.nc/_seassum_1971-2099.nc}" "${f_rcp/%_day_2006-2099_Hg0175.nc/_seassum_1971-2099_}"
rm "${f_rcp/%_day_2006-2099_Hg0175.nc/_seassum_1971-2099.nc}"
elif ([[ "$f_rcp" == *"HadGEM2-ES"* ]] && [[ "$f_rcp" == *"CCLM4-8-17"* ]]) || ([[ "$f_rcp" == *"HadGEM2-ES"* ]] && [[ "$f_rcp" == *"ALADIN63"* ]]);then
mv tmp2.nc "${f_rcp/%_day_2006-2099_Hg0175.nc/_yearsum_1950-2099.nc}"
mv tmp4.nc "${f_rcp/%_day_2006-2099_Hg0175.nc/_seassum_1950-2099.nc}"
cdo splitseas "${f_rcp/%_day_2006-2099_Hg0175.nc/_seassum_1950-2099.nc}" "${f_rcp/%_day_2006-2099_Hg0175.nc/_seassum_1950-2099_}"
rm "${f_rcp/%_day_2006-2099_Hg0175.nc/_seassum_1950-2099.nc}"
elif ([[ "$f_rcp" == *"RCA4"* ]]) || ([[ "$f_rcp" == *"MPI-ESM-LR"* ]] && [[ "$f_rcp" == *"RegCM4-6"* ]]);then
mv tmp2.nc "${f_rcp/%_day_2006-2100_Hg0175.nc/_yearsum_1970-2100.nc}"
mv tmp4.nc "${f_rcp/%_day_2006-2100_Hg0175.nc/_seassum_1970-2100.nc}"
cdo splitseas "${f_rcp/%_day_2006-2100_Hg0175.nc/_seassum_1970-2100.nc}" "${f_rcp/%_day_2006-2100_Hg0175.nc/_seassum_1970-2100_}"
rm "${f_rcp/%_day_2006-2100_Hg0175.nc/_seassum_1970-2100.nc}"
elif ([[ "$f_rcp" == *"CNRM-CM5-LR"* ]] && [[ "$f_rcp" == *"ALADIN63"* ]]) || ([[ "$f_rcp" == *"IPSL-CM5A-MR"* ]] && [[ "$f_rcp" == *"WRF381P"* ]]) || ([[ "$f_rcp" == *"NorESM1-M"* ]] && [[ "$f_rcp" == *"HIRHAM5"* ]]) || ([[ "$f_rcp" == *"NorESM1-M"* ]] && [[ "$f_rcp" == *"WRF381P"* ]]) || ([[ "$f_rcp" == *"IPSL-CM5A-MR"* ]] && [[ "$f_rcp" == *"HIRHAM5"* ]]) || ([[ "$f_rcp" == *"EC-EARTH"* ]] && [[ "$f_rcp" == *"HIRHAM5"* ]]);then
mv tmp2.nc "${f_rcp/%_day_2006-2100_Hg0175.nc/_yearsum_1951-2100.nc}"
mv tmp4.nc "${f_rcp/%_day_2006-2100_Hg0175.nc/_seassum_1951-2100.nc}"
cdo splitseas "${f_rcp/%_day_2006-2100_Hg0175.nc/_seassum_1951-2100.nc}" "${f_rcp/%_day_2006-2100_Hg0175.nc/_seassum_1951-2100_}"
rm "${f_rcp/%_day_2006-2100_Hg0175.nc/_seassum_1951-2100.nc}"
elif ([[ "$f_rcp" == *"CNRM-CM5-LR"* ]] && [[ "$f_rcp" == *"HadREM3-GA7-05"* ]]) || ([[ "$f_rcp" == *"EC-EARTH"* ]] && [[ "$f_rcp" == *"HadREM3-GA7-05"* ]] && [[ "$f_rcp" == *"rcp85"* ]]);then
mv tmp2.nc "${f_rcp/%_day_2006-2100_Hg0175.nc/_yearsum_1952-2100.nc}"
mv tmp4.nc "${f_rcp/%_day_2006-2100_Hg0175.nc/_seassum_1952-2100.nc}"
cdo splitseas "${f_rcp/%_day_2006-2100_Hg0175.nc/_seassum_1952-2100.nc}" "${f_rcp/%_day_2006-2100_Hg0175.nc/_seassum_1952-2100_}"
rm "${f_rcp/%_day_2006-2100_Hg0175.nc/_seassum_1952-2100.nc}"
elif ([[ "$f_rcp" == *"HadGEM2-ES"* ]] && [[ "$f_rcp" == *"HadREM3-GA7-05"* ]])||([[ "$f_rcp" == *"EC-EARTH"* ]] && [[ "$f_rcp" == *"HadREM3-GA7-05"* ]]&& [[ "$f_rcp" == *"rcp26"* ]]);then
mv tmp2.nc "${f_rcp/%_day_2006-2099_Hg0175.nc/_yearsum_1952-2099.nc}"
mv tmp4.nc "${f_rcp/%_day_2006-2099_Hg0175.nc/_seassum_1952-2099.nc}"
cdo splitseas "${f_rcp/%_day_2006-2099_Hg0175.nc/_seassum_1952-2099.nc}" "${f_rcp/%_day_2006-2099_Hg0175.nc/_seassum_1952-2099_}"
rm "${f_rcp/%_day_2006-2099_Hg0175.nc/_seassum_1952-2099.nc}"
else
mv tmp2.nc "${f_rcp/%_day_2006-2100_Hg0175.nc/_yearsum_1950-2100.nc}"
mv tmp4.nc "${f_rcp/%_day_2006-2100_Hg0175.nc/_seassum_1950-2100.nc}"
cdo splitseas "${f_rcp/%_day_2006-2100_Hg0175.nc/_seassum_1950-2100.nc}" "${f_rcp/%_day_2006-2100_Hg0175.nc/_seassum_1950-2100_}"
rm "${f_rcp/%_day_2006-2100_Hg0175.nc/_seassum_1950-2100.nc}"
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


done		





#########################################################
## Make masks regions

nb_sect=$(cdo -s -nlevel -selname,Hydro_sectors ../SAFRAN_mask_sect-hydro_bv_deptmt.nc)
for i in $(seq 1 $nb_sect)
do
cdo sellevel,$i -selname,Hydro_sectors ../SAFRAN_mask_sect-hydro_bv_deptmt.nc mask_sect.nc
cdo setgrid,./masks/mygrid mask_sect.nc ./masks/mask_sect${i}.nc
rm mask_sect.nc
done

nb_dep=$(cdo -s -nlevel -selname,Departments ../SAFRAN_mask_sect-hydro_bv_deptmt.nc)
for i in $(seq 1 $nb_dep)
do
cdo sellevel,$i -selname,Departments ../SAFRAN_mask_sect-hydro_bv_deptmt.nc mask_dep.nc
cdo setgrid,./masks/mygrid mask_dep.nc ./masks/mask_dep${i}.nc
rm mask_dep.nc
done

nb_bv=$(cdo -s -nlevel -selname,Basins ../SAFRAN_mask_sect-hydro_bv_deptmt.nc)
for i in $(seq 1 $nb_bv)
do
cdo sellevel,$i -selname,Basins ../SAFRAN_mask_sect-hydro_bv_deptmt.nc mask_bv.nc
cdo setgrid,./masks/mygrid mask_bv.nc ./masks/mask_bv${i}.nc
rm mask_bv.nc
done

###################################################################################
## Make indicators by zone

var=( tasAdjust prtotAdjust evspsblpotAdjust )

for V in "${!var[@]}"
do
cd ${var[$V]}

mkdir sect
mkdir dep
mkdir bv
#FILES="/mnt/c/Users/reverdya/Documents/Docs/2_Data/raw/meteo/indic/${var[$V]}/${var[$V]}*"
#FILES="/mnt/c/Users/reverdya/Documents/Docs/2_Data/processed/safran/indic/${var[$V]}/${var[$V]}*"
FILES="/mnt/c/Users/reverdya/Documents/Docs/2_Data/processed/Explore2-meteo/indic/${var[$V]}/${var[$V]}*"
for f in $FILES
do
f_local=${f##*/}


cd sect
for i in $(seq 1 $nb_sect)
do
if [ ${var[$V]} != "evspsblpotAdjust" ]; then
cdo -s selgrid,1 -setgrid,../../masks/mygrid "$f" tmp.nc
else
cdo -s selgridname,curvilinear -setgrid,../../masks/mygrid "$f" tmp.nc
fi
cdo -s -mul tmp.nc ../../masks/mask_sect${i}.nc tmp_masked.nc #Warnings okay
rm tmp.nc
cdo -s fldmean tmp_masked.nc "${f_local/%.nc/_sect${i}.nc}" #Warnings okay
rm tmp_masked.nc
done

f_all="/mnt/c/Users/reverdya/Documents/Docs/2_Data/raw/meteo/indic/${var[$V]}/sect/*_sect*"
#f_all="/mnt/c/Users/reverdya/Documents/Docs/2_Data/processed/safran/indic/${var[$V]}/sect/*_sect*"
f_all=$(ls $f_all -tr --time=ctime) #sort by reverse modification time
for f2 in $f_all
do
if [ ${f2##*/} = "${f_local/%.nc/_sect1.nc}" ]; then
mv $f2 tmp.nc
else
cdo -s merge $(basename $f2) tmp.nc "${f_local/%.nc/_all-sect.nc}" 
rm tmp.nc
cp "${f_local/%.nc/_all-sect.nc}" tmp.nc
rm "${f_local/%.nc/_all-sect.nc}"
fi
done
mv tmp.nc "${f_local/%.nc/_all-sect.nc}"
rm *_sect*
cd ..


cd bv
for i in $(seq 1 $nb_bv)
do
if [ ${var[$V]} != "evspsblpotAdjust" ]; then
cdo -s selgrid,1 -setgrid,../../masks/mygrid "$f" tmp.nc
else
cdo -s selgridname,curvilinear -setgrid,../../masks/mygrid "$f" tmp.nc
fi
cdo -s -mul tmp.nc ../../masks/mask_bv${i}.nc tmp_masked.nc #Warnings okay
rm tmp.nc
cdo -s fldmean tmp_masked.nc "${f_local/%.nc/_bv${i}.nc}" #Warnings okay
rm tmp_masked.nc
done

f_all="/mnt/c/Users/reverdya/Documents/Docs/2_Data/raw/meteo/indic/${var[$V]}/bv/*_bv*"
#f_all="/mnt/c/Users/reverdya/Documents/Docs/2_Data/processed/safran/indic/${var[$V]}/bv/*_bv*"
f_all=$(ls $f_all -tr --time=ctime) #sort by reverse modification time
for f2 in $f_all
do
if [ ${f2##*/} = "${f_local/%.nc/_bv1.nc}" ]; then
mv $f2 tmp.nc
else
cdo -s merge $(basename $f2) tmp.nc "${f_local/%.nc/_all-bv.nc}" 
rm tmp.nc
cp "${f_local/%.nc/_all-bv.nc}" tmp.nc
rm "${f_local/%.nc/_all-bv.nc}"
fi
done
mv tmp.nc "${f_local/%.nc/_all-bv.nc}"
rm *_bv*
cd ..



cd dep
for i in $(seq 1 $nb_dep)
do
if [ ${var[$V]} != "evspsblpotAdjust" ]; then
cdo -s selgrid,1 -setgrid,../../masks/mygrid "$f" tmp.nc
else
cdo -s selgridname,curvilinear -setgrid,../../masks/mygrid "$f" tmp.nc
fi
cdo -s -mul tmp.nc ../../masks/mask_dep${i}.nc tmp_masked.nc #Warnings okay
rm tmp.nc
cdo -s fldmean tmp_masked.nc "${f_local/%.nc/_dep${i}.nc}" #Warnings okay
rm tmp_masked.nc
done

f_all="/mnt/c/Users/reverdya/Documents/Docs/2_Data/raw/meteo/indic/${var[$V]}/dep/*_dep*"
#f_all="/mnt/c/Users/reverdya/Documents/Docs/2_Data/processed/safran/indic/${var[$V]}/dep/*_dep*"
f_all=$(ls $f_all -tr --time=ctime) #sort by reverse modification time
for f2 in $f_all
do
if [ ${f2##*/} = "${f_local/%.nc/_dep1.nc}" ]; then
mv $f2 tmp.nc
else
cdo -s merge $(basename $f2) tmp.nc "${f_local/%.nc/_all-dep.nc}" 
rm tmp.nc
cp "${f_local/%.nc/_all-dep.nc}" tmp.nc
rm "${f_local/%.nc/_all-dep.nc}"
fi
done
mv tmp.nc "${f_local/%.nc/_all-dep.nc}"
rm *_dep*
cd ..

done
cd ..
done









unset rcp3
unset var
unset P
unset V
unset FILES
unset f
unset f_rcp
unset f2
unset f_local
unset f_all
unset nb_dep
unset nb_sect
unset nb_bv

