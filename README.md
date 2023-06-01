# explore2
Code Alix Reverdy for Explore 2
first bar is partially written, second bar is definitively written, third bar is ran, fourth bar is ran on definitive dataset



Order climat:
from "C:/Users/reverdya/Documents/Docs/1_code/explore/"

->scripts/dwnld_meteo_all_local.sh			||||
->meteo/preprocess/shp_to_nc.R				| |	after that manually moved SAFRAN_mask_sect-hydro_bv_deptmt.nc from SIG/processed/ to raw/meteo/ and processed/Explore2-meteo/indic/masks/		bv...			
->scripts/calc_indic_meteo_local.sh			| |	after that manually moved calculated indic and masks from raw/ to processed/										bv..., version could be still optimized for calculation time				
->meteo/preprocess/regrid_safran.R			||||	after that manually moved and renamed raw/meteo/safran/safran_new_ETP.nc raw/meteo/safran/regridded/safran_new_ETP_grid.nc
->scripts/calc_indic_safran.sh				| |																				bv..., version could be still optimized for calculation time

->prep_temp/prep_temp_pred.R				||||
->prep_temp/understandXfut_mat.R			|||	figures checked? NO																	Waiting on T_global

->meteo/check_indic_meteo.R				| |	figures checked? NO																	bv...
->meteo/run_Qualypso.R					| |	before that visually determine best spar														new T_FR function, bv..., change Xfut, spar
->meteo/make_figures_meteo.R				| |	figures checked? NO																	new T_FR function, bv..., new mask, change Xfut, log and warning if spline<0, warning if brown-forsythe fails between chains or between periods, visual, safran data, Waiting on T_global



Order hydro:
from "C:/Users/reverdya/Documents/Docs/1_code/explore/"

->hydro/preprocess/clean_files_indic.R			||||																	

->hydro/check_indic_hydro.R				||||	figures checked? YES
->hydro/run_Qualypso.R					||||	before that visually determine best spar														
->hydro/make_figures_hydro.R				||||	figures checked? NO																	visual, Waiting on T_global, log and warning if spline<0, warning if brown-forsythe fails between chains or between periods
