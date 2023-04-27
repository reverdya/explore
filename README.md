# explore2
Code Alix Reverdy for Explore 2
first bar is partially written, second bar is definitively written, second bar is ran, third bar is ran on definitive dataset



Ordre climat:
from "C:/Users/reverdya/Documents/Docs/1_code/"

->scripts/dwnld_meteo_all_local.sh			||||
->explore/meteo/preprocess/shp_to_nc.R			||||	manually moved SAFRAN_mask_sect-hydro_bv_deptmt.nv from SIG/processed/ to raw/meteo/ 
->scripts/calc_indic_meteo_local.sh			||||	manually moved calculated indic and masks from raw/ to processed/
->explore/meteo/preprocess/regrid_safran.R		||||
->scripts/calc_indic_safran.sh				|	manually moved and renamed raw/meteo/safran/safran_new_ETP.nc raw/meteo/safran/regridded/safran_new_ETP_grid.nc

->prep_temp_pred.R					|	figures checked? NO / figures final visual? NO

->explore/meteo/check_indic_meteo.R			|	figures checked? NO / figures final visual? YES
->explore/meteo/run_Qualypso.R				|	before that visually determine best spar
->explore/meteo/make_figures_meteo.R			|	figures checked? NO / figures final visual? NO



Ordre hydro:
from "C:/Users/reverdya/Documents/Docs/1_code/"

->explore/hydro/preprocess/clean_files_indic.R		|||

->explore/hydro/check_indic_hydro.R			|||	figures checked? NO / figures final visual? YES
->explore/hydro/run_Qualypso.R				|	before that visually determine best spar
->explore/hydro/make_figures_hydro.R			|	figures checked? NO / figures final visual? NO
