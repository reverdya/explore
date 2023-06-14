#!/bin/bash

# Calculate indicators
# A. Reverdy (27/04/2023)
# Launch by doing time bash calc_indic_safran.sh 2>&1 | tee info.log (do not add & or it will bug read), allows time measurement and output to info.log (overwrites) AND console
# First plug hard drive then open WSL
# This is for a grouping of old scripts: some bugs could remain. Please check that all files got processed

cd /mnt/c/Users/reverdya/Documents/Docs/2_Data/processed/safran/indic/

var=( tasAdjust prtotAdjust prsnAdjust evspsblpotAdjust )


###################################################
## Indicators


# FILES="/mnt/c/Users/reverdya/Documents/Docs/2_Data/raw/meteo/safran/regridded/*nc"
# for f in $FILES
# do

# tmp=$(basename $f)
# tmp=$(echo "$tmp" | cut -d'_' -f3)

# if [[ "$tmp" == "Tair" ]]; then
# cdo yearmean "$f" tmp2.nc
# cdo seasmean "$f" tmp3.nc
# cdo monmean "$f" tmp6.nc
# last=$(cdo -ntime tmp3.nc)
# cdo delete,timestep=1,$last tmp3.nc tmp4.nc
# rm tmp3.nc

# mv tmp2.nc safran_tasAdjust_yearmean.nc
# mv tmp4.nc safran_tasAdjust_seasmean.nc
# cdo splitseas safran_tasAdjust_seasmean.nc safran_tasAdjust_seasmean_
# rm safran_tasAdjust_seasmean.nc
# mv tmp6.nc safran_tasAdjust_monmean.nc
# cdo splitmon safran_tasAdjust_monmean.nc safran_tasAdjust_monmean_
# rm safran_tasAdjust_monmean.nc
# fi


# if [[ "$tmp" == "Ptot" ]]; then
# cdo yearsum "$f" tmp2.nc
# cdo seassum "$f" tmp3.nc
# cdo monsum "$f" tmp6.nc
# last=$(cdo -ntime tmp3.nc)
# cdo delete,timestep=1,$last tmp3.nc tmp4.nc
# rm tmp3.nc

# mv tmp2.nc safran_prtotAdjust_yearsum.nc
# mv tmp4.nc safran_prtotAdjust_seassum.nc
# cdo splitseas safran_prtotAdjust_seassum.nc safran_prtotAdjust_seassum_
# rm safran_prtotAdjust_seassum.nc
# mv tmp6.nc safran_prtotAdjust_monsum.nc
# cdo splitmon safran_prtotAdjust_monsum.nc safran_prtotAdjust_monsum_
# rm safran_prtotAdjust_monsum.nc
# fi



# if [[ "$tmp" == "ETP" ]]; then
# cdo yearsum "$f" tmp2.nc
# cdo seassum "$f" tmp3.nc
# cdo monsum "$f" tmp6.nc
# last=$(cdo -ntime tmp3.nc)
# cdo delete,timestep=1,$last tmp3.nc tmp4.nc
# rm tmp3.nc

# mv tmp2.nc safran_evspsblpotAdjust_yearsum.nc
# mv tmp4.nc safran_evspsblpotAdjust_seassum.nc
# cdo splitseas safran_evspsblpotAdjust_seassum.nc safran_evspsblpotAdjust_seassum_
# rm safran_evspsblpotAdjust_seassum.nc
# mv tmp6.nc safran_evspsblpotAdjust_monsum.nc
# cdo splitmon safran_evspsblpotAdjust_monsum.nc safran_evspsblpotAdjust_monsum_
# rm safran_evspsblpotAdjust_monsum.nc
# fi


# if [[ "$tmp" == "Snowf" ]];then
# cdo shifttime,2months "$f" tmp1.nc #to start year in november
# cdo -yearsum -select,season=JFMAMJ tmp1.nc tmp2.nc
# rm tmp1.nc
# cdo shifttime,-2months tmp2.nc tmp3.nc
# rm tmp2.nc
# cdo mulc,86400 tmp3.nc tmp4.nc
# rm tmp3.nc
# cdo selgrid,1 -setgrid,../../Explore2-meteo/indic/masks/mygrid tmp4.nc tmp5.nc
# rm tmp4.nc
# cdo -mul tmp5.nc ../../Explore2-meteo/indic/masks/mask_alti1000.nc tmp6.nc
# rm tmp5.nc
# last=$(cdo -ntime tmp6.nc)
# cdo delete,timestep=1,$last tmp6.nc tmp7.nc
# rm tmp6.nc

# mv tmp7.nc safran_prsnAdjust_NDJFMAsum.nc
# fi


# done		


###################################################################################
## Make indicators by zone


var=( tasAdjust prtotAdjust evspsblpotAdjust )



for V in "${!var[@]}"
do

mkdir bas
mkdir sect
mkdir deptmt
mkdir bv

FILES="/mnt/c/Users/reverdya/Documents/Docs/2_Data/processed/safran/indic/*${var[$V]}*"

for f in $FILES
do
f_local=${f##*/}


cd bas
cdo -s -mul $f ../../../Explore2-meteo/indic/masks/mask_bas.nc tmp_masked.nc #Warnings okay
cdo -s fldmean tmp_masked.nc "${f_local/%.nc/_bas.nc}" #Warnings okay
rm tmp_masked.nc
cd ..


cd sect
cdo -s -mul $f ../../../Explore2-meteo/indic/masks/mask_sect.nc tmp_masked.nc #Warnings okay
cdo -s fldmean tmp_masked.nc "${f_local/%.nc/_sect.nc}" #Warnings okay
rm tmp_masked.nc
cd ..


cd deptmt
cdo -s -mul $f ../../../Explore2-meteo/indic/masks/mask_deptmt.nc tmp_masked.nc #Warnings okay
cdo -s fldmean tmp_masked.nc "${f_local/%.nc/_deptmt.nc}" #Warnings okay
rm tmp_masked.nc
cd ..


cd bv
cdo -s -mul $f ../../../Explore2-meteo/indic/masks/mask_bv.nc tmp_masked.nc #Warnings okay
cdo -s fldmean tmp_masked.nc "${f_local/%.nc/_bv.nc}" #Warnings okay
rm tmp_masked.nc
cd ..


done
done









unset var
unset V
unset FILES
unset f
unset f_rcp
unset f2
unset f_local

