# explore2
Code Alix Reverdy for Explore 2
first bar is partially written, second bar is definitively written, third bar is ran, fourth bar is ran on definitive dataset



Order climat:
from "C:/Users/reverdya/Documents/Docs/1_code/explore/"

->scripts/dwnld_meteo_all_local.sh			||||
->meteo/preprocess/shp_to_nc.R				||||	after that manually moved SAFRAN_mask_bas_sect-hydro_bv_deptmt.nc from SIG/processed/ to raw/meteo/ and processed/Explore2-meteo/indic/masks/				
->scripts/calc_indic_meteo_local.sh			|||	after that manually moved calculated indic and masks from raw/ to processed/														
->meteo/preprocess/regrid_safran.R			||||	after that manually moved and renamed raw/meteo/safran/safran_new_ETP.nc raw/meteo/safran/regridded/safran_new_ETP_grid.nc
->scripts/calc_indic_safran.sh				|||																				

->prep_temp/prep_temp_pred.R				||||
->prep_temp/understandXfut_mat.R			|||	figures checked? NO																	Waiting on T_global

->meteo/check_indic_meteo.R				| |	figures checked? NO																	bv...
->meteo/run_Qualypso.R					| |	before that visually determine best spar														new T_FR function, bv..., change Xfut, spar, without CDFt
->meteo/make_figures_meteo.R				| |	figures checked? NO																	new T_FR function, bv..., new mask, change Xfut, safran data (ETP<2019), Waiting on T_global, new figures (regime, carte erreur, storylines), figuré significatif (2 types hachures)



Order hydro:
from "C:/Users/reverdya/Documents/Docs/1_code/explore/"

->hydro/preprocess/clean_files_indic.R			||||																	

->hydro/check_indic_hydro.R				||||	figures checked? YES
->hydro/run_Qualypso.R					|||	before that visually determine best spar														Waiting on Mordor 2006, waiting on bug february, run FR without SIM2?
->hydro/make_figures_hydro.R				|||	figures checked? NO																	Waiting on T_global, waiting on Mordor 2006, waiting on bug february, run FR without SIM2?
