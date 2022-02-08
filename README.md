# Global patterns and drivers of C:N:P in marine ecosystems

[![license](https://img.shields.io/badge/license-MIT%20+%20file%20LICENSE-lightgrey.svg)](https://choosealicense.com/)
[![Ask Us Anything
\!](https://img.shields.io/badge/Ask%20us-anything-1abc9c.svg)](https://github.com/dbarneche/nature20200508666/issues/new)
![Open Source
Love](https://badges.frapsoft.com/os/v2/open-source.svg?v=103)

This repository contains code and data needed to reproduce the article:

**Tanioka, T. Garcia, C. A., Larkin., Garcia, N. S., Fagan, A. J., Martiny A. C.** (*submitted*) Global patterns and drivers of C:N:P in marine ecosystems.

<!---
**When using the data or code from this project, please cite it as:**

**Tanioka, T. Garcia, C. A., Larkin., Garcia, N. S., Fagan, A. J., Martiny A. C.** Initial Submission of paper data and code of manuscript: Global patterns and drivers of C:N:P in marine ecosystems. *Zenodo*. DOI: 
-->

## Instructions

All processing was done in `R`. This routine uses the [drake R package](https://github.com/ropensci/drake) to compile the output time table. First install `drake`:

```r
install.packages("drake")
```

Next you need to open an R session with working directory set to the root of the project.

This routine loads multiple packages which are found in `R/packages.R`, **so make sure to successfully install and load them before running drake** with the code below.

To reproduce particular targets outlined in `R/plan.R`, do e.g.:

```r
source("_drake.R")
drake::make(plan, targets = c("fig_1_pdf", "fig_2_pdf", "ed_fig_1_pdf","sp_table_1"), lock_envir = FALSE)
```

This will create Figures 1 and 2, Extended Data Figure 1, and Table 1 in Supplementary Information as presented in the manuscript along with all its dependencies. All output will be automatically placed in a directory called `output` (it is going to be automatically created for you).

To reproduce all data analyses / figures / tables, and then make them available within an R session, do:

```r
source("make.R")
drake::loadd()
```

This whole process should be completed in less than 5 minutes. ODV Figures 1a-c are located in the separate `pics` folder. Note that figures created here are subsequently cleaned using Adobe Illustrator.

### This paper was produced using the following software and associated packages:
```
R version 4.1.0 (2021-05-18)
Platform: x86_64-apple-darwin17.0 (64-bit)
Running under: macOS Big Sur 11.6

Matrix products: default
LAPACK: /Library/Frameworks/R.framework/Versions/4.1/Resources/lib/libRlapack.dylib

locale:
[1] en_US.UTF-8/en_US.UTF-8/en_US.UTF-8/C/en_US.UTF-8/en_US.UTF-8

attached base packages:
[1] stats     graphics  grDevices utils     datasets  methods   base     

other attached packages:
 [1] Hmisc_4.6-0          Formula_1.2-4        cmocean_0.3-1        ocedata_0.1.8        oce_1.4-0           
 [6] testthat_3.1.0       sf_1.0-3             gsw_1.0-6            ncdf4_1.17           RColorBrewer_1.1-2  
[11] gratia_0.6.0         mgcv_1.8-38          nlme_3.1-153         matrixStats_0.61.0   mosaic_1.8.3        
[16] ggridges_0.5.3       mosaicData_0.20.2    ggformula_0.10.1     ggstance_0.3.5       lattice_0.20-45     
[21] scales_1.1.1         plotly_4.10.0        Matrix_1.3-4         multcomp_1.4-17      TH.data_1.1-0       
[26] survival_3.2-13      mvtnorm_1.1-3        MASS_7.3-54          pals_1.7             corrplot_0.90       
[31] hrbrthemes_0.8.0     viridis_0.6.2        viridisLite_0.4.0    ggpubr_0.4.0         rstan_2.21.2        
[36] StanHeaders_2.21.0-7 data.table_1.14.2    forcats_0.5.1        stringr_1.4.0        purrr_0.3.4         
[41] readr_2.0.2          tidyr_1.1.4          tibble_3.1.5         ggplot2_3.3.5        tidyverse_1.3.1     
[46] naniar_0.6.1         dplyr_1.0.7          plyr_1.8.6           drake_7.13.3        

loaded via a namespace (and not attached):
  [1] utf8_1.2.2          R.utils_2.11.0      tidyselect_1.1.1    htmlwidgets_1.5.4   grid_4.1.0         
  [6] munsell_0.5.0       base64url_1.4       codetools_0.2-18    units_0.7-2         withr_2.4.2        
 [11] colorspace_2.0-2    filelock_1.0.2      knitr_1.36          rstudioapi_0.13     stats4_4.1.0       
 [16] ggsignif_0.6.3      Rttf2pt1_1.3.9      labeling_0.4.2      polyclip_1.10-0     farver_2.1.0       
 [21] txtq_0.2.4          vctrs_0.3.8         generics_0.1.1      xfun_0.27           R6_2.5.1           
 [26] assertthat_0.2.1    nnet_7.3-16         gtable_0.3.0        processx_3.5.2      sandwich_3.0-1     
 [31] rlang_0.4.12        systemfonts_1.0.3   splines_4.1.0       rstatix_0.7.0       extrafontdb_1.0    
 [36] lazyeval_0.2.2      dichromat_2.0-0     checkmate_2.0.0     broom_0.7.9         mosaicCore_0.9.0   
 [41] inline_0.3.19       yaml_2.2.1          reshape2_1.4.4      abind_1.4-5         modelr_0.1.8       
 [46] crosstalk_1.1.1     backports_1.3.0     extrafont_0.17      tools_4.1.0         ellipsis_0.3.2     
 [51] jquerylib_0.1.4     ggdendro_0.1.22     proxy_0.4-26        Rcpp_1.0.7          base64enc_0.1-3    
 [56] progress_1.2.2      classInt_0.4-3      ps_1.6.0            prettyunits_1.1.1   rpart_4.1-15       
 [61] zoo_1.8-9           cluster_2.1.2       haven_2.4.3         ggrepel_0.9.1       fs_1.5.0           
 [66] magrittr_2.0.1      openxlsx_4.2.4      reprex_2.0.1        mvnfast_0.2.7       storr_1.2.5        
 [71] hms_1.1.1           patchwork_1.1.1     evaluate_0.14       leaflet_2.0.4.1     jpeg_0.1-9         
 [76] rio_0.5.27          readxl_1.3.1        gridExtra_2.3       compiler_4.1.0      maps_3.4.0         
 [81] KernSmooth_2.23-20  V8_3.4.2            crayon_1.4.1        R.oo_1.24.0         htmltools_0.5.2    
 [86] tzdb_0.2.0          visdat_0.5.3        RcppParallel_5.1.4  lubridate_1.8.0     DBI_1.1.1          
 [91] tweenr_1.0.2        dbplyr_2.1.1        car_3.0-11          cli_3.1.0           R.methodsS3_1.8.1  
 [96] parallel_4.1.0      igraph_1.2.7        pkgconfig_2.0.3     foreign_0.8-81      xml2_1.3.2         
[101] bslib_0.3.1         rvest_1.0.2         callr_3.7.0         digest_0.6.28       rmarkdown_2.11     
[106] cellranger_1.1.0    htmlTable_2.3.0     gdtools_0.2.3       curl_4.3.2          lifecycle_1.0.1    
[111] jsonlite_1.7.2      carData_3.0-4       mapproj_1.2.7       fansi_0.5.0         labelled_2.8.0     
[116] pillar_1.6.4        loo_2.4.1           fastmap_1.1.0       httr_1.4.2          pkgbuild_1.2.0     
[121] glue_1.4.2          zip_2.2.0           png_0.1-7           ggforce_0.3.3       class_7.3-19       
[126] stringi_1.7.5       sass_0.4.0          latticeExtra_0.6-29 e1071_1.7-9     
```

## License

This repository is provided by the authors under the MIT License ([MIT](http://opensource.org/licenses/MIT)).

### How to download this project for people not familiar with GitHub:  
* on the project main page on GitHub, click on the green button `clone or download` and then click on `Download ZIP`  

## Bug reporting
* Please report any issues or bugs to Tatsuro Tanioka (tatsurt[at]uci.edu).
