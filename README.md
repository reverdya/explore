# explore2
Code Alix Reverdy for Explore 2
first bar is partially written, second bar is definitively written, second bar is ran, third bar is ran on definitive dataset



Order climat:
from "C:/Users/reverdya/Documents/Docs/1_code/explore/"

->scripts/dwnld_meteo_all_local.sh			||||
->meteo/preprocess/shp_to_nc.R				||||	after that manually moved SAFRAN_mask_sect-hydro_bv_deptmt.nv from SIG/processed/ to raw/meteo/ and processed/Explore2-meteo/indic/masks/					
->scripts/calc_indic_meteo_local.sh			||||	after that manually moved calculated indic and masks from raw/ to processed/ ; version could be still optimized for calculation time							
->meteo/preprocess/regrid_safran.R			||||	after that manually moved and renamed raw/meteo/safran/safran_new_ETP.nc raw/meteo/safran/regridded/safran_new_ETP_grid.nc
->scripts/calc_indic_safran.sh				||||	version could be still optimized for calculation time

->prep_temp/prep_temp_pred.R				||||
->prep_temp/understandXfut_mat.R			|||	figures checked? NO														Waiting on T_global

->meteo/check_indic_meteo.R				|||	figures checked? NO
->meteo/run_Qualypso.R					| |	before that visually determine best spar											new T_FR function, remove bv..., add basins, change Xfut, spar
->meteo/make_figures_meteo.R				| |	figures checked? NO														new T_FR function,  remove bv..., add basins, new mask, change Xfut, log and warning if spline<0, warning if brown-forsythe fails between chains or between periods, visual, safran data, Waiting on T_global



Order hydro:
from "C:/Users/reverdya/Documents/Docs/1_code/explore/"

->hydro/preprocess/clean_files_indic.R			||||																	

->hydro/check_indic_hydro.R				||||	figures checked? NO
->hydro/run_Qualypso.R					| |	before that visually determine best spar											new T_FR function,choice on runs, spar, change Xfut
->hydro/make_figures_hydro.R				| |	figures checked? NO														new T_FR function,choice on runs, change Xfut, visual, mesures hydro, Waiting on T_global, change Xfut, log and warning if spline<0, warning if brown-forsythe fails between chains or between periods, visual, safran data, Waiting on T_global
