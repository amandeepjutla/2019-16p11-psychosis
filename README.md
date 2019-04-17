# 2019-16p11-psychosis

These scripts will reproduce the analyses reported in Jutla A, Turner JB, Snyder LG, Chung WK, Veenstra-VanderWeele J. "Psychotic symptoms in 16p11.2 copy number variant carriers." Manuscript in preparation.

# How to use

Extract the officially released Simons VIP 16p11.2 dataset archive into `data/clean` and place CSV of M-SOPS data into `data/raw.` Run `r/0_initialize.r` in a clean R environment, and look in `r/output` for results.

# Tested configuration

Output of `sessionInfo()` for my environment:

```
R version 3.5.1 (2018-07-02)
Platform: x86_64-apple-darwin15.6.0 (64-bit)
Running under: macOS High Sierra 10.13.6

Matrix products: default
BLAS: /System/Library/Frameworks/Accelerate.framework/Versions/A/Frameworks/vecLib.framework/Versions/A/libBLAS.dylib
LAPACK: /Library/Frameworks/R.framework/Versions/3.5/Resources/lib/libRlapack.dylib

locale:
[1] en_US.UTF-8/en_US.UTF-8/en_US.UTF-8/C/en_US.UTF-8/en_US.UTF-8

attached base packages:
[1] stats     graphics  grDevices utils     datasets  methods   base     

other attached packages:
 [1] bindrcpp_0.2.2   fifer_1.1        arm_1.10-1       lme4_1.1-19      Matrix_1.2-14   
 [6] MASS_7.3-50      conflicted_1.0.1 imputeTS_2.7     geepack_1.2-1    broom_0.5.0     
[11] forcats_0.3.0    stringr_1.3.1    dplyr_0.7.8      purrr_0.2.5      readr_1.1.1     
[16] tidyr_0.8.2      tibble_1.4.2     ggplot2_3.1.0    tidyverse_1.2.1  checkpoint_0.4.5

loaded via a namespace (and not attached):
  [1] TH.data_1.0-9         minqa_1.2.4           colorspace_1.3-2     
  [4] modeltools_0.2-22     estimability_1.3      htmlTable_1.12       
  [7] base64enc_0.1-3       rstudioapi_0.8        mice_3.3.0           
 [10] mvtnorm_1.0-8         lubridate_1.7.4       coin_1.2-2           
 [13] xml2_1.2.0            codetools_0.2-15      splines_3.5.1        
 [16] knitr_1.20            spam_2.2-0            Formula_1.2-3        
 [19] jsonlite_1.5          nloptr_1.2.1          cluster_2.0.7-1      
 [22] compiler_3.5.1        httr_1.3.1            randomForestSRC_2.7.0
 [25] lsmeans_2.30-0        emmeans_1.3.0         backports_1.1.2      
 [28] assertthat_0.2.0      lazyeval_0.2.1        strucchange_1.5-1    
 [31] cli_1.0.1             acepack_1.4.1         htmltools_0.3.6      
 [34] tools_3.5.1           dotCall64_1.0-0       coda_0.19-2          
 [37] gtable_0.2.0          glue_1.3.0            maps_3.3.0           
 [40] Rcpp_1.0.0            cellranger_1.1.0      fracdiff_1.4-2       
 [43] urca_1.3-0            nlme_3.1-137          lmtest_0.9-36        
 [46] timeDate_3043.102     rvest_0.3.2           pan_1.6              
 [49] zoo_1.8-4             scales_1.0.0          hms_0.4.2            
 [52] parallel_3.5.1        sandwich_2.5-0        RColorBrewer_1.1-2   
 [55] fields_9.6            quantmod_0.4-13       curl_3.2             
 [58] memoise_1.1.0         gridExtra_2.3         uroot_2.0-9          
 [61] rpart_4.1-13          latticeExtra_0.6-28   stringi_1.2.4        
 [64] tseries_0.10-45       randomForest_4.6-14   plotrix_3.7-4        
 [67] checkmate_1.8.5       TTR_0.23-4            rlang_0.3.0.1        
 [70] pkgconfig_2.0.2       lattice_0.20-35       bindr_0.1.1          
 [73] stinepack_1.4         htmlwidgets_1.3       cowplot_0.9.3        
 [76] tidyselect_0.2.5      plyr_1.8.4            magrittr_1.5         
 [79] R6_2.3.0              Hmisc_4.1-1           mitml_0.3-6          
 [82] multcomp_1.4-8        pillar_1.3.0          haven_1.1.2          
 [85] foreign_0.8-70        withr_2.1.2           xts_0.11-2           
 [88] survival_2.42-3       abind_1.4-5           nnet_7.3-12          
 [91] modelr_0.1.2          crayon_1.3.4          jomo_2.6-5           
 [94] party_1.3-1           grid_3.5.1            readxl_1.1.0         
 [97] data.table_1.11.8     forecast_8.4          digest_0.6.18        
[100] xtable_1.8-3          stats4_3.5.1          munsell_0.5.0        
[103] quadprog_1.5-5
```

# Changelog

- 20190417: cleaned up a little.
- 20190416: uploaded code.
- 20190202: created repository.

# Questions

Contact <Amandeep.Jutla@nyspi.columbia.edu>.