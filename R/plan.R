plan  <-  drake::drake_plan(
  # Data -------------------------------------------------
  POM_all = read.csv("data/POM_all.csv"),                           # all POM data after selection
  POM_genomes_selected = read.csv("data/POM_genomes_selected.csv"), # POM data paired with genome-based nut limitation
  # Data wrangling for various analyses ------------------
  POM_all_binned = bin_data_1by1(POM_all),                              
  # POM_genomes_selected_binned = bin_data_1by1(POM_genomes_selected),
  # POM_all_gam = clean_data_for_gam(POM_all), 
  # POM_genomes_selected_gam = clean_data_for_gam(POM_genomes_selected),
  # POM_highlat_gam = sep_data_highlat(POM_all_gam, 45), 
  # POM_lowlat_gam = sep_data_lowlat(POM_genomes_selected_gam, 45),
  POM_highlat_gam = clean_data_for_gam(POM_all) %>% sep_data_highlat(latitude = 45),
  POM_lowlat_gam = clean_data_for_gam(POM_genomes_selected) %>% sep_data_lowlat(latitude = 45),
  scaled.POM_highlat_corr = clean_data_for_corr(sep_data_highlat(POM_all, latitude = 45)),
  scaled.POM_lowlat_corr = clean_data_for_corr(sep_data_lowlat(POM_all, latitude = 45)),
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
    make_region_cnp_summary(cp_boxplot(POM_all), rownames = FALSE, tab_out_folder,  "sp_table_4.csv")
  },
  sp_table_5 = {
    make_region_cnp_summary(np_boxplot(POM_all), rownames = FALSE, tab_out_folder,  "sp_table_5.csv")
  },
  sp_table_6 = {
    make_region_cnp_summary(cn_boxplot(POM_all), rownames = FALSE, tab_out_folder,  "sp_table_6.csv")
  },
  sp_table_7 = {
    make_corr_pval_table(M.POM_highlat_corr_selected , 
			 testRes_selected_highlat,
			 rownames = TRUE,
			 tab_out_folder,  "sp_table_7.csv")
  },
  sp_table_8 = {
    make_corr_pval_table(M.POM_lowlat_corr_selected , 
			 testRes_selected_lowlat,
			 rownames = TRUE,
			 tab_out_folder,  "sp_table_8.csv")
  }




)
