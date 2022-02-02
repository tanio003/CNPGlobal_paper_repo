plan  <-  drake::drake_plan(
  # Data -------------------------------------------------
  POM_all = read.csv("data/POM_all.csv"),
  POM_genomes_selected = read.csv("data/POM_genomes_selected"),
  # Data wrangling for various analyses
  POM_all_binned = bin_data_1by1(POM_all),
  POM_genomes_selected_binned = bin_data_1by1(POM_genomes_selected),
  POM_all_gam = clean_data_for_gam(POM_all), 
  POM_genomes_selected_gam = clean_data_for_gam(POM_genomes_selected),
  POM_highlat_gam = dplyr::filter(POM_all_gam, absLatitude >= 45), 
  POM_lowlat_gam = dplyr::filter(POM_genomes_selected_gam, absLatitude >= 45),

  # Analyses ---------------------------------------------
  CNP_global_mean = calc_cnp_global_mean(tibble(POM_all)),
  CNP_global_mean_binned = calc_cnp_global_mean(tibble(POM_all_binned)),
  # Figures ----------------------------------------------
  fig_out_folder = dir.create("output/figures/",
                              recursive = TRUE,
                              showWarnings = FALSE),
  fig_1_pdf = make_fig_1(file_out("output/figures/fig_1.pdf"), fig_out_folder,POM_all),
  sp_fig_1_pdf = make_sp_fig_1(file_out("output/figures/sp_fig_1.pdf"), fig_out_folder,POM_all),

  # Tables -----------------------------------------------
  tab_out_folder = {
    dir.create("output/tables/", recursive = TRUE, showWarnings = FALSE)
    "output/tables"
  },
  sp_table_2 = {
    make_cnp_table(CNP_global_mean, sgnf = 1, tab_out_folder, "sp_table_2.csv")
  },
  sp_table_3 = {
    make_cnp_table(CNP_global_mean_binned, sgnf = 1, tab_out_folder,  "sp_table_3.csv")
  }
)
