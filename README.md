# a-ABCD

########
CITATION
########

Rapuano, KM, Conley, MI, Juliano, AC, Conan, GM, Maza, MT, Woodman, K, Martinez, SA, Earl, E, Perrone, A, Feczko, E, Fair, DA, Watts, R, Casey, BJ, Rosenberg, MD. (in press). An open-access accelerated adult equivalent of the ABCD Study neuroimaging dataset (a-ABCD). NeuroImage.

########
DATA
########

a-ABCD_behav.csv: a-ABCD participant task performance data from the in-scanner stop-signal, monetary incentive delay, and emotional n-back tasks and the post-scan recognition memory test for EN-back stimuli. 

a-ABCD_mood_feelings.csv: Participants rated how relaxed, happy, scared, awake, upset, angry, excited, tired, sleepy, and sad they felt on a scale from 1 (“very slightly or not at all”) to 5 (“extremely”) immediately before and after each scan session. This file includes the a-ABCD participants' self-reported mood and feelings data. 

########
SCRIPTS
########

a-ABCD_Figure1.R: Loads curated ABCD data files (which can be downloaded from https://nda.nih.gov/abcd in accordance with an approved Data Use Certification [https://nda.nih.gov/abcd/request-access]) and a-ABCD behavioral data. Generates two plots visualizing the distributions of task performance data in ABCD and a-ABCD Study datasets. Performs percentile analyses reported in the "Overall task performance" section of the text.

a-ABCD_Figure2.R: Loads a-ABCD behavioral data and generates plot showing change in task performance across sessions. 

a-ABCD_behav_stats.R: Loads a-ABCD behavioral data and performs analyses of task performance data reported in the "Changes in performance across sessions" section of the text.

a-ABCD_mood_feelings.R: Loads a-ABCD self-reported mood and feeling questionnaire data and performs analyses reported in the "Self-reported mood and feeling questionnaires" section of the text.

###################
VERSION INFORMATION
###################
The following were used for analyses reported in the manuscript: 

 sessionInfo()
R version 4.0.3 (2020-10-10)
Platform: x86_64-apple-darwin17.0 (64-bit)
Running under: macOS Catalina 10.15.7

Matrix products: default
BLAS:   /System/Library/Frameworks/Accelerate.framework/Versions/A/Frameworks/vecLib.framework/Versions/A/libBLAS.dylib
LAPACK: /Library/Frameworks/R.framework/Versions/4.0/Resources/lib/libRlapack.dylib

locale:
en_US.UTF-8/en_US.UTF-8/en_US.UTF-8/C/en_US.UTF-8/en_US.UTF-8

attached base packages:
stats     graphics  grDevices utils     datasets  methods   base     

other attached packages:
bayestestR_0.9.0  optimx_2020-4.2   lmerTest_3.1-3    lme4_1.1-26       Matrix_1.2-18     ggplot2_3.3.2     dplyr_1.0.2       data.table_1.13.2

loaded via a namespace (and not attached):
Rcpp_1.0.5          pillar_1.4.7        compiler_4.0.3      nloptr_1.2.2.2      tools_4.0.3         boot_1.3-25         statmod_1.4.35     
lifecycle_1.0.0     tibble_3.0.4        gtable_0.3.0        nlme_3.1-149        lattice_0.20-41     pkgconfig_2.0.3     rlang_0.4.11       
cli_3.0.1           rstudioapi_0.13     xfun_0.19           withr_2.4.2         generics_0.1.0      vctrs_0.3.5         grid_4.0.3         
tidyselect_1.1.0    glue_1.4.2          R6_2.5.0            minqa_1.2.4         purrr_0.3.4         magrittr_2.0.1      scales_1.1.1       
ellipsis_0.3.1      splines_4.0.3       MASS_7.3-53         insight_0.14.4      colorspace_2.0-0    numDeriv_2016.8-1.1 tinytex_0.27       
munsell_0.5.0       crayon_1.3.4   
