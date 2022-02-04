plan  <-  drake::drake_plan(
  # Data -------------------------------------------------
  POM_all = read.csv("data/POM_all.csv"),                           # all POM data after selection
  POM_genomes_selected = read.csv("data/POM_genomes_selected.csv"), # POM data paired with genome-based nut limitation
  # Data wrangling for various analyses ------------------
  POM_all_binned = bin_data_1by1(POM_all),                              
  POM_all_gam = clean_data_for_gam(POM_all), 
  POM_genomes_selected_gam = clean_data_for_gam(POM_genomes_selected),
  POM_genomes_selected_gam_w_highlat = combine_data_global_gam(POM_all_gam,POM_genomes_selected_gam),
  POM_highlat_gam = POM_all_gam %>% sep_data_highlat(latitude = 45),
  POM_lowlat_gam = POM_genomes_selected_gam %>% sep_data_lowlat(latitude = 45),
  scaled.POM_highlat_corr = clean_data_for_corr(sep_data_highlat(POM_all, latitude = 45)),
  scaled.POM_lowlat_corr = clean_data_for_corr(sep_data_lowlat(POM_all, latitude = 45)),
  # CESM2-LENS Data--------------------------------------
  cesm_filepath = "data/CESM2_lens",
  sst_surf_historic = read_sst_cesm(cesm_filepath, 'TEMP_regrid_historic.nc'),
  sst_surf_SSP370 = read_sst_cesm(cesm_filepath, 'TEMP_regrid_SSP370.nc'),
  nutcline_historic = read_nutcline_cesm(cesm_filepath, 'Nutcline_regrid_historic.nc',
                                        nutcline_correction = 1.54),
  nutcline_SSP370 = read_nutcline_cesm(cesm_filepath, 'Nutcline_regrid_SSP370.nc',
                                      nutcline_correction = 1.54),
  nitrate_surf_historic = read_nitrate_cesm(cesm_filepath, 'NO3_regrid_historic.nc', threshold = 0.1),
  nitrate_surf_SSP370 = read_nitrate_cesm(cesm_filepath, 'NO3_regrid_SSP370.nc', threshold = 0.1),
  nutlim_historic = read_nutlim_cesm(cesm_filepath,
                                    'sp_Nut_lim_surf_historic.nc',
                                    varid = "sp_Nut_lim_surf_historic"),
  nutlim_SSP370 = read_nutlim_cesm(cesm_filepath,
                                    'sp_Nut_lim_surf_SSP370.nc',
                                    varid = "sp_Nut_lim_surf_SSP370"),
  # Analyses ---------------------------------------------
  CNP_global_mean = calc_cnp_global_mean(tibble(POM_all)),
  CNP_global_mean_binned = calc_cnp_global_mean(tibble(POM_all_binned)),
  M.POM_highlat_corr_selected = M.POM_corr(scaled.POM_highlat_corr,highlat = TRUE, colnames = TRUE),
  M.POM_lowlat_corr_selected = M.POM_corr(scaled.POM_lowlat_corr,highlat = FALSE, colnames = TRUE),
  testRes_selected_highlat = testRes.POM_corr(scaled.POM_highlat_corr,highlat = TRUE),
  testRes_selected_lowlat = testRes.POM_corr(scaled.POM_lowlat_corr, highlat = FALSE),
  CNP_highlat_gam_devexpl = make_CNP_devexpl_highlat(POM_highlat_gam),
  CNP_lowlat_gam_devexpl = make_CNP_devexpl_lowlat(POM_lowlat_gam),
  CNP_highlat_gam_pval =  make_CNP_pval_highlat(POM_highlat_gam),
  CNP_lowlat_gam_pval = make_CNP_pval_lowlat(POM_lowlat_gam),
  mod_CNP_no_Nutlim = make_mod_CNP_no_Nutlim(POM_genomes_selected_gam_w_highlat),
  mod_CNP_Nutcline_Nutlim_modGS = make_mod_CNP_Nutcline_Nutlim_modGS(POM_genomes_selected_gam_w_highlat),
  # Figures ----------------------------------------------
  fig_out_folder = dir.create("output/figures/",
                              recursive = TRUE,
                              showWarnings = FALSE),
  fig_1_pdf = make_fig_1(file_out("output/figures/fig_1.pdf"), fig_out_folder,POM_all),
  fig_2_pdf = make_fig_2(file_out("output/figures/fig_2.pdf"), fig_out_folder,
                       M.POM_highlat_corr_selected,
                       M.POM_lowlat_corr_selected,
                       testRes_selected_highlat,
                       testRes_selected_lowlat,
                       CNP_highlat_gam_devexpl,
                       CNP_lowlat_gam_devexpl),
  fig_3_pdf = make_fig_3(file_out("output/figures/fig_3.pdf"), fig_out_folder,
                mod_CNP_no_Nutlim$mod_CP,
                mod_CNP_no_Nutlim$mod_NP,
                mod_CNP_no_Nutlim$mod_CN,
                mod_CNP_Nutcline_Nutlim_modGS$mod_CP_Nutcline_Nutlim_modGS,
                mod_CNP_Nutcline_Nutlim_modGS$mod_NP_Nutcline_Nutlim_modGS,
                mod_CNP_Nutcline_Nutlim_modGS$mod_CN_Nutcline_Nutlim_modGS,
	       	POM_genomes_selected_gam_w_highlat),
  ed_fig_2_pdf = make_ed_fig_2(file_out("output/figures/ed_fig_2.pdf"),fig_out_folder,
			      POM_genomes_selected_binned_w_highlat,
			      nutlim_historic,
			      nutlim_SSP370,
			      cesm_lonlat_info),
  sp_fig_1_pdf = make_sp_fig_1(file_out("output/figures/sp_fig_1.pdf"), fig_out_folder,POM_all),

  # Tables -----------------------------------------------
  tab_out_folder = {
    dir.create("output/tables/", recursive = TRUE, showWarnings = FALSE)
    "output/tables"
  },
  sp_table_2 = {
    make_cnp_table(CNP_global_mean, sgnf = 1, rownames = TRUE, tab_out_folder, "sp_table_2.csv")
  },
  sp_table_3 = {
    make_cnp_table(CNP_global_mean_binned, sgnf = 1, rownames = TRUE, tab_out_folder,  "sp_table_3.csv")
  },
  sp_table_4 = {
    make_region_cnp_summary_combined(POM_all, rownames = FALSE, tab_out_folder,  "sp_table_4.csv")
  },
  sp_table_5 = {
    make_corr_pval_table(M.POM_highlat_corr_selected , 
			 testRes_selected_highlat,
			 rownames = TRUE,
			 tab_out_folder,  "sp_table_5.csv")
  },
  sp_table_6 = {
    make_corr_pval_table(M.POM_lowlat_corr_selected , 
			 testRes_selected_lowlat,
			 rownames = TRUE,
			 tab_out_folder,  "sp_table_6.csv")
  },
  sp_table_7 = {
    make_devexpl_pval_table(CNP_highlat_gam_devexpl,
			   CNP_highlat_gam_pval, 
			   digits = 3,
			   rownames = TRUE,
			 tab_out_folder,  "sp_table_7.csv")
  },
  sp_table_8 = {
    make_devexpl_pval_table(CNP_lowlat_gam_devexpl,
			   CNP_lowlat_gam_pval, 
			   digits = 3,
			   rownames = TRUE,
			 tab_out_folder,  "sp_table_8.csv")
  }





)
