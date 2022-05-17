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


########
VERSION INFORMATION
########

The following versions were used for analyses reported in the manuscript: 

sessionInfo()

R version 4.0.3 (2020-10-10)

Platform: x86_64-apple-darwin17.0 (64-bit)

Running under: macOS Catalina 10.15.7

Matrix products: default

BLAS:   /System/Library/Frameworks/Accelerate.framework/Versions/A/Frameworks/vecLib.framework/Versions/A/libBLAS.dylib

LAPACK: /Library/Frameworks/R.framework/Versions/4.0/Resources/lib/libRlapack.dylib

locale: en_US.UTF-8/en_US.UTF-8/en_US.UTF-8/C/en_US.UTF-8/en_US.UTF-8

attached base packages:
stats     graphics  grDevices utils     datasets  methods   base    

other attached packages:
forcats_0.5.0     purrr_0.3.4       readr_1.4.0       tibble_3.0.4      tidyverse_1.3.0   gridExtra_2.3     ggridges_0.5.3   tidyr_1.1.2       sjPlot_2.8.9      stringr_1.4.0     bayestestR_0.9.0  optimx_2020-4.2   lmerTest_3.1-3    lme4_1.1-26      Matrix_1.2-18     ggplot2_3.3.2     dplyr_1.0.2       data.table_1.13.2

loaded via a namespace (and not attached):
httr_1.4.2          jsonlite_1.7.1      splines_4.0.3       modelr_0.1.8        datawizard_0.2.0.1  assertthat_0.2.1   statmod_1.4.35      highr_0.8           cellranger_1.1.0    numDeriv_2016.8-1.1 pillar_1.4.7        backports_1.2.0    lattice_0.20-41     glue_1.4.2          digest_0.6.27       rvest_0.3.6         minqa_1.2.4         colorspace_2.0-0   sandwich_3.0-0      htmltools_0.5.0     plyr_1.8.6          pkgconfig_2.0.3     broom_0.7.2         haven_2.3.1        xtable_1.8-4        mvtnorm_1.1-1       scales_1.1.1        emmeans_1.6.3       generics_0.1.0      sjlabelled_1.1.7   ellipsis_0.3.1      TH.data_1.0-10      withr_2.4.2         cli_3.0.1           readxl_1.3.1        survival_3.2-7     magrittr_2.0.1      crayon_1.3.4        effectsize_0.4.5    estimability_1.3    evaluate_0.14       fs_1.5.0           nlme_3.1-149        MASS_7.3-53         xml2_1.3.2          tools_4.0.3         hms_0.5.3           lifecycle_1.0.0    multcomp_1.4-16     reprex_0.3.0        munsell_0.5.0       ggeffects_1.0.2     compiler_4.0.3      tinytex_0.27       rlang_0.4.11        grid_4.0.3          nloptr_1.2.2.2      parameters_0.14.0   rstudioapi_0.13     rmarkdown_2.5      boot_1.3-25         gtable_0.3.0        codetools_0.2-16    sjstats_0.18.1      DBI_1.1.0           sjmisc_2.8.7       R6_2.5.0            lubridate_1.7.9.2   zoo_1.8-9           knitr_1.30          performance_0.7.3   insight_0.14.4     stringi_1.5.3       Rcpp_1.0.5          vctrs_0.3.5         dbplyr_2.0.0        tidyselect_1.1.0    xfun_0.19          coda_0.19-4  
