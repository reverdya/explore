# explore2
Code Alix Reverdy for Explore 2
first bar is partially written, second bar is definitively written, second bar is ran, third bar is ran on definitive dataset



Ordre climat:
from "C:/Users/reverdya/Documents/Docs/1_code/explore/"

->scripts/dwnld_meteo_all_local.sh			||||
->meteo/preprocess/shp_to_nc.R				||||	manually moved SAFRAN_mask_sect-hydro_bv_deptmt.nv from SIG/processed/ to raw/meteo/ 
->scripts/calc_indic_meteo_local.sh			|||	manually moved calculated indic and masks from raw/ to processed/							RUNNING
->meteo/preprocess/regrid_safran.R			||||
->scripts/calc_indic_safran.sh				||	manually moved and renamed raw/meteo/safran/safran_new_ETP.nc raw/meteo/safran/regridded/safran_new_ETP_grid.nc		waiting on idle cores

->prep_temp/prep_temp_pred.R				||||
->prep_temp/understandXfut_mat.R			|||	figures checked? NO													Waiting on T_global

->meteo/check_indic_meteo.R				| |	figures checked? NO													Optimize memory, new T_FR function, add bv, new mask
->meteo/run_Qualypso.R					| |	before that visually determine best spar										Without IPSL/WRF, new T_FR function, add bv, new mask, change Xfut
->meteo/make_figures_meteo.R				| |	figures checked? NO													Without IPSL/WRF, new T_FR function, add bv, new mask, change Xfut, visual, safran data, Waiting on T_global



Ordre hydro:
from "C:/Users/reverdya/Documents/Docs/1_code/"

->explore/hydro/preprocess/clean_files_indic.R		|||																Waiting for SIM2 and GRSD_final, 2005 bug, code MORDOR-SD

->explore/hydro/check_indic_hydro.R			| |	figures checked? NO													Waiting for SIM2 and GRSD_final, 2005 bug, code MORDOR-SD
->explore/hydro/run_Qualypso.R				| |	before that visually determine best spar										Waiting for SIM2 and GRSD_final, 2005 bug, code MORDOR-SD, choice on runs
->explore/hydro/make_figures_hydro.R			| |	figures checked? NO													Waiting for SIM2 and GRSD_final, 2005 bug, code MORDOR-SD, choice on runs, visual, mesures hydro, Waiting on T_global
