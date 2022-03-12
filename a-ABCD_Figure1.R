library(dplyr)

rm(list=ls())

######################################################################################
############################## LOAD AND CLEAN VARIABLES ############################## 
######################################################################################
dataDir   <-"ABCDdata/ABCDrelease2/"
dataDir2  <-"ABCDdata/ABCDrelease201/"
outputDir <- getwd()

mysum  <- function(x)sum(x,na.rm = any(!is.na(x)))
mymean <- function(x)mean(x,na.rm = any(!is.na(x)))

######### Read files #########
Demographics <- read.delim(paste(dataDir,"abcddemo01.txt",sep=""), na.strings=c(""," ","NA"), stringsAsFactors=FALSE) %>% slice(-1)
Screener     <- read.delim(paste(dataDir,"abcd_screen01.txt",sep=""), na.strings=c(""," ","NA"), stringsAsFactors=FALSE) %>% slice(-1)
RAChecklist  <- read.delim(paste(dataDir,"abcd_ra01.txt",sep=""), na.strings=c(""," ","NA"), stringsAsFactors=FALSE) %>% slice(-1)
ScannerID    <- read.delim(paste(dataDir2,"abcd_mri01.txt",sep=""), na.strings=c(""," ","NA"), stringsAsFactors=FALSE) %>% slice(-1)
SiteID       <- read.delim(paste(dataDir,"abcd_lt01.txt",sep=""), na.strings=c(""," ","NA"), stringsAsFactors=FALSE) %>% slice(-1)
Family       <- read.delim(paste(dataDir,"acspsw03.txt",sep=""), na.strings=c(""," ","NA"), stringsAsFactors=FALSE) %>% slice(-1)

NIH_toolbox  <- read.delim(paste(dataDir,"abcd_tbss01.txt",sep=""), na.strings=c(""," ","NA"), stringsAsFactors=FALSE) %>% slice(-1)
Pearson      <- read.delim(paste(dataDir,"abcd_ps01.txt",sep=""), na.strings=c(""," ","NA"), stringsAsFactors=FALSE) %>% slice(-1)
CashChoice   <- read.delim(paste(dataDir,"cct01.txt",sep=""), na.strings=c(""," ","NA"), stringsAsFactors=FALSE) %>% slice(-1)
LittleMan    <- read.delim(paste(dataDir,"lmtp201.txt",sep=""), na.strings=c(""," ","NA"), stringsAsFactors=FALSE) %>% slice(-1)

Nback        <- read.delim(paste(dataDir,"abcd_mrinback02.txt",sep=""), na.strings=c(""," ","NA"), stringsAsFactors=FALSE) %>% slice(-1)
RecMem       <- read.delim(paste(dataDir,"mribrec02.txt",sep=""), na.strings=c(""," ","NA"), stringsAsFactors=FALSE) %>% slice(-1)
SST          <- read.delim(paste(dataDir,"abcd_sst02.txt",sep=""), na.strings=c(""," ","NA"), stringsAsFactors=FALSE) %>% slice(-1)
MID          <- read.delim(paste(dataDir,"abcd_mid02.txt",sep=""), na.strings=c(""," ","NA"), stringsAsFactors=FALSE) %>% slice(-1)

######### Define relevant variables #########
cols.Demographics <- c("interview_age","sex")
cols.Screener     <- c("scrn_asd","scrn_medcond_other","scrn_epls","scrn_seizure","scrn_commondx")
cols.RAChecklist  <- c("ra_scan_check_list_rcom","ra_scan_cl_mid_scan_lap","ra_scan_check_list_vemorc","ra_scan_cl_nbac_scan_lap","ra_scan_check_list_sstrc","ra_scan_cl_sst_scan_lap")
cols.ScannerID    <- c("mri_info_deviceserialnumber")
cols.Family       <- c("rel_relationship","rel_family_id")
cols.SiteID       <- c("site_id_l")
cols.NIH_toolbox  <- c("nihtbx_picvocab_uncorrected","nihtbx_flanker_uncorrected","nihtbx_list_uncorrected","nihtbx_cardsort_uncorrected","nihtbx_pattern_uncorrected","nihtbx_picture_uncorrected","nihtbx_reading_uncorrected","nihtbx_fluidcomp_uncorrected","nihtbx_cryst_uncorrected","nihtbx_totalcomp_uncorrected")
cols.Pearson      <- c("pea_wiscv_tss","pea_ravlt_sd_trial_i_tc","pea_ravlt_sd_trial_ii_tc","pea_ravlt_sd_trial_iii_tc","pea_ravlt_sd_trial_iv_tc","pea_ravlt_sd_trial_v_tc","pea_ravlt_sd_trial_i_tr","pea_ravlt_sd_trial_ii_tr","pea_ravlt_sd_trial_iii_tr","pea_ravlt_sd_trial_iv_tr","pea_ravlt_sd_trial_v_tr","pea_ravlt_sd_trial_i_ti","pea_ravlt_sd_trial_ii_ti","pea_ravlt_sd_trial_iii_ti","pea_ravlt_sd_trial_iv_ti","pea_ravlt_sd_trial_v_ti","pea_ravlt_sd_listb_tc","pea_ravlt_sd_listb_tr","pea_ravlt_sd_listb_ti","pea_ravlt_sd_trial_vi_tc","pea_ravlt_sd_trial_vi_tr","pea_ravlt_sd_trial_vi_ti","pea_ravlt_ld_trial_vii_tc","pea_ravlt_ld_trial_vii_tr","pea_ravlt_ld_trial_vii_ti")
cols.CashChoice   <- c("cash_choice_task")
cols.LittleMan    <- c("lmt_scr_efficiency","lmt_scr_perc_correct","lmt_scr_rt_correct")
cols.Nback        <- c("tfmri_nback_beh_switchflag","tfmri_nback_beh_performflag","tfmri_nb_all_beh_ctotal_mrt","tfmri_nb_all_beh_ctotal_stdrt","tfmri_nb_all_beh_c0b_rate","tfmri_nb_all_beh_c0bnf_rate","tfmri_nb_all_beh_c0bngf_rate","tfmri_nb_all_beh_c0bp_rate","tfmri_nb_all_beh_c0bpf_rate","tfmri_nb_all_beh_c2b_rate","tfmri_nb_all_beh_c2bnf_rate","tfmri_nb_all_beh_c2bngf_rate","tfmri_nb_all_beh_c2bp_rate","tfmri_nb_all_beh_c2bpf_rate","tfmri_nb_all_beh_cnf_rate","tfmri_nb_all_beh_cngf_rate","tfmri_nb_all_beh_cpf_rate","tfmri_nb_all_beh_cplace_rate","tfmri_nb_all_beh_ctotal_rate","tfmri_nb_r1_beh_c0b_rate","tfmri_nb_r2_beh_c0b_rate","tfmri_nb_r1_beh_c2b_rate","tfmri_nb_r2_beh_c2b_rate")
cols.RecMem       <- c("tfmri_rec_beh_switchflag","tfmri_rec_all_beh_posface_br","tfmri_rec_all_beh_posf_dpr","tfmri_rec_all_beh_neutface_br","tfmri_rec_all_beh_neutf_dp","tfmri_rec_all_beh_negface_br","tfmri_rec_all_beh_negf_dp","tfmri_rec_all_beh_place_br","tfmri_rec_all_beh_place_dp","tfmri_rec_all_beh_oldpl_hr","tfmri_rec_all_beh_oldposf_hr","tfmri_rec_all_beh_oldnegf_hr","tfmri_rec_all_beh_oldnf_hr","tfmri_rec_all_beh_newnf_fa","tfmri_rec_all_beh_newpl_fa","tfmri_rec_all_beh_newposf_fa","tfmri_rec_all_beh_newnegf_fa")
cols.SST          <- c("tfmri_sst_beh_switchflag","tfmri_sst_beh_performflag","tfmri_sst_all_beh_crgo_rt","tfmri_sst_all_beh_crgo_mrt","tfmri_sst_all_beh_crgo_stdrt","tfmri_sst_all_beh_crlg_rt","tfmri_sst_all_beh_incrgo_rt","tfmri_sst_all_beh_incrlg_rt","tfmri_sst_all_beh_nrgo_rt","tfmri_sst_all_beh_crs_rt","tfmri_sst_all_beh_incrs_rt","tfmri_sst_all_beh_ssds_rt","tfmri_sst_all_beh_tot_mssd","tfmri_sst_all_beh_total_meanrt")
cols.MID          <- c("tfmri_mid_beh_switchflag","tfmri_mid_beh_performflag","tfmri_mid_all_beh_srwpfb_rate","tfmri_mid_all_beh_lrwpfb_rate","tfmri_mid_all_beh_slpfb_rate","tfmri_mid_all_beh_llpfb_rate","tfmri_mid_r1_beh_t_earnings","tfmri_mid_r2_beh_t_earnings","tfmri_mid_all_beh_t_earnings","tfmri_mid_all_beh_t_nt","tfmri_mid_all_beh_srwpfb_nt","tfmri_mid_all_beh_lrwpfb_nt","tfmri_mid_all_beh_slpfb_nt","tfmri_mid_all_beh_llpfb_nt","tfmri_mid_all_beh_ntpfb_nt","tfmri_mid_all_beh_srwpfb_mrt","tfmri_mid_all_beh_lrwpfb_mrt","tfmri_mid_all_beh_slpfb_mrt","tfmri_mid_all_beh_llpfb_mrt","tfmri_mid_all_beh_ntpfb_mrt","tfmri_mid_all_beh_hrwpfb_rate","tfmri_mid_all_beh_hlpfb_rate","tfmri_mid_all_beh_ntpfb_rate")

######### Retain relevant variables #########
Demographics      <- unique(subset(Demographics, select = c("subjectkey", cols.Demographics)))
Screener          <- unique(subset(Screener,     select = c("subjectkey", cols.Screener)))
RAChecklist       <- unique(subset(RAChecklist,  select = c("subjectkey", cols.RAChecklist)))
ScannerID         <- unique(subset(ScannerID,    select = c("subjectkey", cols.ScannerID)))
Family            <- unique(subset(Family,       select = c("subjectkey", cols.Family)))
SiteID            <- SiteID[ which(SiteID$eventname=="baseline_year_1_arm_1"),]
SiteID            <- unique(subset(SiteID,       select = c("subjectkey", cols.SiteID)))
NIH_toolbox       <- unique(subset(NIH_toolbox,  select = c("subjectkey", cols.NIH_toolbox)))
Pearson           <- unique(subset(Pearson,      select = c("subjectkey", cols.Pearson)))
CashChoice        <- unique(subset(CashChoice,   select = c("subjectkey", cols.CashChoice)))
LittleMan         <- unique(subset(LittleMan,    select = c("subjectkey", cols.LittleMan)))
Nback             <- unique(subset(Nback,        select = c("subjectkey", cols.Nback)))
RecMem            <- unique(subset(RecMem,       select = c("subjectkey", cols.RecMem)))
SST               <- unique(subset(SST,          select = c("subjectkey", cols.SST)))
MID               <- unique(subset(MID,          select = c("subjectkey", cols.MID)))

######### Convert variables to numeric #########
Demographics[, "interview_age"] <- lapply("interview_age",  function(x) as.numeric(Demographics[[x]]))
Screener[, cols.Screener]       <- lapply(cols.Screener,    function(x) as.numeric(Screener[[x]]))
Family[, cols.Family]           <- lapply(cols.Family,      function(x) as.numeric(Family[[x]]))
NIH_toolbox[, cols.NIH_toolbox] <- lapply(cols.NIH_toolbox, function(x) as.numeric(NIH_toolbox[[x]]))
Pearson[, cols.Pearson]         <- lapply(cols.Pearson,     function(x) as.numeric(Pearson[[x]]))
CashChoice[, cols.CashChoice]   <- lapply(cols.CashChoice,  function(x) as.numeric(CashChoice[[x]]))
LittleMan[, cols.LittleMan]     <- lapply(cols.LittleMan,   function(x) as.numeric(LittleMan[[x]]))
Nback[, cols.Nback]             <- lapply(cols.Nback,       function(x) as.numeric(Nback[[x]]))
RecMem[, cols.RecMem]           <- lapply(cols.RecMem,      function(x) as.numeric(RecMem[[x]]))
SST[, cols.SST]                 <- lapply(cols.SST,         function(x) as.numeric(SST[[x]]))
MID[, cols.MID]                 <- lapply(cols.MID,         function(x) as.numeric(MID[[x]]))

######### Add performance measure columns #########
RecMem$overall_dprime              <- apply(RecMem[c('tfmri_rec_all_beh_posf_dpr', 'tfmri_rec_all_beh_neutf_dp', 'tfmri_rec_all_beh_negf_dp', 'tfmri_rec_all_beh_place_dp')], 1, mymean)
RecMem$hit_rate                    <- apply(RecMem[c('tfmri_rec_all_beh_oldpl_hr','tfmri_rec_all_beh_oldposf_hr','tfmri_rec_all_beh_oldnegf_hr','tfmri_rec_all_beh_oldnf_hr')], 1, mymean)
RecMem$falsealarm_rate             <- apply(RecMem[c('tfmri_rec_all_beh_newnf_fa','tfmri_rec_all_beh_newpl_fa','tfmri_rec_all_beh_newposf_fa','tfmri_rec_all_beh_newnegf_fa')], 1, mymean)
MID$mean_earnings                  <- apply(MID[c('tfmri_mid_r1_beh_t_earnings', 'tfmri_mid_r2_beh_t_earnings')], 1, mymean)

######### Invert SSRT #########
SST$tfmri_sst_all_beh_total_meanrt_inv <- SST$tfmri_sst_all_beh_total_meanrt*-1

######### Remove cash choice option 3 ("don't know") #########
CashChoice$cash_choice_task_no3 <- CashChoice$cash_choice_task
CashChoice$cash_choice_task_no3[CashChoice$cash_choice_task_no3 == 3] <- NA

######### Merge, clean, crop data #########
data.merge <- Reduce(function(x,y) merge(x = x, y = y, by = "subjectkey", all.x = TRUE, all.y = TRUE), list(Demographics, Screener, RAChecklist, ScannerID, SiteID, Family, NIH_toolbox, Pearson, CashChoice, LittleMan, Nback, RecMem, SST, MID))
data.crop  <- data.merge[ which(data.merge$scrn_asd==0 & (data.merge$scrn_epls!=1 | is.na(data.merge$scrn_epls))), ]
data.crop  <- subset(data.crop, select = c(subjectkey, rel_family_id, site_id_l, ra_scan_cl_mid_scan_lap, ra_scan_cl_nbac_scan_lap, ra_scan_cl_sst_scan_lap, interview_age, sex, tfmri_nback_beh_performflag,  tfmri_sst_beh_performflag, tfmri_mid_beh_performflag, tfmri_nb_all_beh_c0b_rate, tfmri_nb_all_beh_c2b_rate, overall_dprime, hit_rate, falsealarm_rate, tfmri_sst_all_beh_tot_mssd, tfmri_sst_all_beh_total_meanrt_inv, tfmri_mid_all_beh_hrwpfb_rate, tfmri_mid_all_beh_hlpfb_rate, tfmri_mid_all_beh_ntpfb_rate))

start_col <- 12

######################################################################################
################################# Load adult data #################################### 
######################################################################################
library(data.table)
behav.adult <- read.delim("adultABCD_behav.csv",sep=",", na.strings=c(""," ","NA"), header=TRUE, skipNul=TRUE)
behav.table <- data.table(behav.adult)
behav.adult <- as_data_frame(behav.table[behav.table$session=='1'])

library(tidyr)
behav.adult <- pivot_wider(behav.adult, names_from = c(measure, task), values_from = value)
behav.adult$dataset <- 'adult'
behav.adult = subset(behav.adult, select = -c(subject,session,pracrt_mid,earnings_mid) )

# Rename columns
names(behav.adult)[names(behav.adult) == "zeroback_enb"]            <- "tfmri_nb_all_beh_c0b_rate"
names(behav.adult)[names(behav.adult) == "twoback_enb"]             <- "tfmri_nb_all_beh_c2b_rate"
names(behav.adult)[names(behav.adult) == "dprime_recmem"]           <- "overall_dprime"
names(behav.adult)[names(behav.adult) == "hit_rate_recmem"]         <- "hit_rate"
names(behav.adult)[names(behav.adult) == "falsealarm_rate_recmem"]  <- "falsealarm_rate"
names(behav.adult)[names(behav.adult) == "ssd_sst"]                 <- "tfmri_sst_all_beh_tot_mssd"
names(behav.adult)[names(behav.adult) == "ssrt_sst"]                <- "tfmri_sst_all_beh_total_meanrt"
names(behav.adult)[names(behav.adult) == "reward_acc_mid"]          <- "tfmri_mid_all_beh_hrwpfb_rate"
names(behav.adult)[names(behav.adult) == "loss_acc_mid"]            <- "tfmri_mid_all_beh_hlpfb_rate"
names(behav.adult)[names(behav.adult) == "neutral_acc_mid"]         <- "tfmri_mid_all_beh_ntpfb_rate"


######################################################################################
################################### MAKE FIGURES ##################################### 
######################################################################################

######### Figure 1: Behavioral distributions #########
library(ggplot2)
library(ggridges)
library(gridExtra)
library(tidyverse)

behav.abcd  <- data.merge[ which(data.merge$scrn_asd==0 & (data.merge$scrn_epls!=1 | is.na(data.merge$scrn_epls))), ]
behav.abcd  <- subset(behav.abcd, select = c(tfmri_nb_all_beh_c0b_rate, tfmri_nb_all_beh_c2b_rate, overall_dprime, hit_rate, falsealarm_rate, tfmri_sst_all_beh_tot_mssd, tfmri_sst_all_beh_total_meanrt, tfmri_mid_all_beh_hrwpfb_rate, tfmri_mid_all_beh_hlpfb_rate, tfmri_mid_all_beh_ntpfb_rate))
behav.abcd$dataset <- 'abcd'
behav.abcd$tfmri_nb_all_beh_c0b_rate     <- behav.abcd$tfmri_nb_all_beh_c0b_rate*100
behav.abcd$tfmri_nb_all_beh_c2b_rate     <- behav.abcd$tfmri_nb_all_beh_c2b_rate*100
behav.abcd$hit_rate                      <- behav.abcd$hit_rate*100
behav.abcd$falsealarm_rate               <- behav.abcd$falsealarm_rate*100
behav.abcd$tfmri_mid_all_beh_hrwpfb_rate <- behav.abcd$tfmri_mid_all_beh_hrwpfb_rate*100
behav.abcd$tfmri_mid_all_beh_hlpfb_rate  <- behav.abcd$tfmri_mid_all_beh_hlpfb_rate*100
behav.abcd$tfmri_mid_all_beh_ntpfb_rate  <- behav.abcd$tfmri_mid_all_beh_ntpfb_rate*100

fig1_theme1 <- theme_minimal() + theme(legend.position="none", panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.title.x = element_text(size = 14), axis.title.y = element_text(colour = "black", size = 14))
fig1_theme2 <- theme_minimal() + theme(legend.position="none", panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.title.x = element_text(size = 14), axis.title.y = element_text(colour = "white", size = 14))
fig1_theme2 <- theme_minimal() + theme(legend.position="none", panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.title.x = element_text(size = 14), axis.title.y = element_text(colour = "white", size = 14))

color1 <- "#1C237D"
color2 <- "#2771B8"
color3 <- "#4EBCD6"
color4 <- "#BCE3C9"

# Neuroimaging task measures
p.0bk         <- ggplot(behav.abcd, aes(x=tfmri_nb_all_beh_c0b_rate, y=dataset)) + 
  geom_density_ridges(jittered_points = TRUE, position = position_points_jitter(width = 0.05, height = 0),
  point_shape = '|', point_size = 1, point_alpha = .5, aes(height =..ndensity.., fill=dataset), alpha = .7) + 
  scale_fill_manual(values=c(color3)) + xlim(0,100) + xlab("EN-back: 0-back % accuracy") + fig1_theme2

p.2bk         <- ggplot(behav.abcd, aes(x=tfmri_nb_all_beh_c2b_rate, y=dataset)) + 
  geom_density_ridges(jittered_points = TRUE, position = position_points_jitter(width = 0.05, height = 0),
  point_shape = '|', point_size = 1, point_alpha = .5, aes(height =..ndensity.., fill=dataset), alpha = .7) + 
  scale_fill_manual(values=c(color3)) + xlim(0,100) + xlab("EN-back: 2-back % accuracy") + fig1_theme2

dprime_title <- expression(paste("Recognition Memory: ",italic("d'")))
p.dprime     <- ggplot(behav.abcd, aes(x=overall_dprime, y=dataset)) + 
  geom_density_ridges(jittered_points = TRUE, position = position_points_jitter(width = 0.05, height = 0),
  point_shape = '|', point_size = 1, point_alpha = .5, aes(height =..ndensity.., fill=dataset), alpha = .7) + 
  scale_fill_manual(values=c(color4)) + xlim(-6,6) + xlab(dprime_title) + fig1_theme2

p.hit_rate   <- ggplot(behav.abcd, aes(x=hit_rate, y=dataset)) + 
  geom_density_ridges(jittered_points = TRUE, position = position_points_jitter(width = 0.05, height = 0),
  point_shape = '|', point_size = 1, point_alpha = .5, aes(height =..ndensity.., fill=dataset), alpha = .7) + 
  scale_fill_manual(values=c(color4)) + xlim(0,100) + xlab("Recognition Memory: Hit rate") + fig1_theme2

p.falsealarm_rate<- ggplot(behav.abcd, aes(x=falsealarm_rate, y=dataset)) +
  geom_density_ridges(jittered_points = TRUE, position = position_points_jitter(width = 0.05, height = 0),
  point_shape = '|', point_size = 1, point_alpha = .5, aes(height =..ndensity.., fill=dataset), alpha = .7) +
  scale_fill_manual(values=c(color4)) + xlim(0,100) + xlab("Recognition Memory: False alarm rate") + fig1_theme2

p.ssd  <- ggplot(behav.abcd, aes(x=tfmri_sst_all_beh_tot_mssd, y=dataset)) +
  geom_density_ridges(jittered_points = TRUE, position = position_points_jitter(width = 0.05, height = 0),
  point_shape = '|', point_size = 1, point_alpha = .5, aes(height =..ndensity.., fill=dataset), alpha = .7) +
  scale_fill_manual(values=c(color1)) + xlim(0,800) + xlab("SST: Stop-signal delay (ms)") + fig1_theme1 + ylab("density")

p.ssrt <- ggplot(behav.abcd, aes(x=tfmri_sst_all_beh_total_meanrt, y=dataset)) +
  geom_density_ridges(jittered_points = TRUE, position = position_points_jitter(width = 0.05, height = 0),
  point_shape = '|', point_size = 1, point_alpha = .5, aes(height =..ndensity.., fill=dataset), alpha = .7) +
  scale_fill_manual(values=c(color1)) + xlim(-800,800) + xlab("SST: Stop-signal reaction time (ms)") + fig1_theme2

p.reward_acc  <- ggplot(behav.abcd, aes(x=tfmri_mid_all_beh_hrwpfb_rate, y=dataset)) +
  geom_density_ridges(jittered_points = TRUE, position = position_points_jitter(width = 0.05, height = 0),
  point_shape = '|', point_size = 1, point_alpha = .5, aes(height =..ndensity.., fill=dataset), alpha = .7) +
  scale_fill_manual(values=c(color2)) + xlim(0,100) + xlab("MID: Reward % accuracy") + fig1_theme2

p.loss_acc    <- ggplot(behav.abcd, aes(x=tfmri_mid_all_beh_hlpfb_rate, y=dataset))  +
  geom_density_ridges(jittered_points = TRUE, position = position_points_jitter(width = 0.05, height = 0),
  point_shape = '|', point_size = 1, point_alpha = .5, aes(height =..ndensity.., fill=dataset), alpha = .7) +
  scale_fill_manual(values=c(color2)) + xlim(0,100) + xlab("MID: Loss % accuracy") + fig1_theme2

p.neutral_acc <- ggplot(behav.abcd, aes(x=tfmri_mid_all_beh_ntpfb_rate, y=dataset))  +
  geom_density_ridges(jittered_points = TRUE, position = position_points_jitter(width = 0.05, height = 0),
  point_shape = '|', point_size = 1, point_alpha = .5, aes(height =..ndensity.., fill=dataset), alpha = .7) +
  scale_fill_manual(values=c(color2)) + xlim(0,100) + xlab("MID: Neutral % accuracy") + fig1_theme2

pBlank <- ggplot() + theme_void()
g1<-arrangeGrob(p.ssd, p.ssrt, pBlank, p.reward_acc, p.loss_acc, p.neutral_acc, p.0bk, p.2bk, pBlank, p.dprime, p.hit_rate, p.falsealarm_rate, nrow = 4)
ggsave("Dist_ABCD_compare.pdf", g1, width = 12, height = 8, units = c("in"), dpi = 300)


##### Adult plots
p.0bk         <- ggplot(behav.adult, aes(x=tfmri_nb_all_beh_c0b_rate, y=dataset)) + 
  geom_density_ridges(jittered_points = TRUE, 
  position = position_points_jitter(width = 0.05, height = 0),
  point_alpha = 1, aes(height =..ndensity..), alpha = 0) + 
  xlim(0,100) + xlab("EN-back: 0-back % accuracy") + fig1_theme2

p.2bk         <- ggplot(behav.adult, aes(x=tfmri_nb_all_beh_c2b_rate, y=dataset)) + 
  geom_density_ridges(jittered_points = TRUE, 
  position = position_points_jitter(width = 0.05, height = 0),
  point_alpha = 1, aes(height =..ndensity..), alpha = 0) + 
  xlim(0,100) + xlab("EN-back: 2-back % accuracy") + fig1_theme2

dprime_title <- expression(paste("Recognition Memory: ",italic("d'")))
p.dprime     <- ggplot(behav.adult, aes(x=overall_dprime, y=dataset)) + 
  geom_density_ridges(jittered_points = TRUE, 
  position = position_points_jitter(width = 0.05, height = 0),
  point_alpha = 1, aes(height =..ndensity..), alpha = 0) + 
  xlim(-6,6) + xlab(dprime_title) + fig1_theme2

p.hit_rate   <- ggplot(behav.adult, aes(x=hit_rate, y=dataset)) + 
  geom_density_ridges(jittered_points = TRUE, 
  position = position_points_jitter(width = 0.05, height = 0),
  point_alpha = 1, aes(height =..ndensity..), alpha = 0) + 
  xlim(0,100) + xlab("Recognition Memory: Hit rate") + fig1_theme2

p.falsealarm_rate<- ggplot(behav.adult, aes(x=falsealarm_rate, y=dataset)) +
  geom_density_ridges(jittered_points = TRUE, 
  position = position_points_jitter(width = 0.05, height = 0),
  point_alpha = 1, aes(height =..ndensity..), alpha = 0) + 
  xlim(0,100) + xlab("Recognition Memory: False alarm rate") + fig1_theme2

p.ssd  <- ggplot(behav.adult, aes(x=tfmri_sst_all_beh_tot_mssd, y=dataset)) +
  geom_density_ridges(jittered_points = TRUE, 
  position = position_points_jitter(width = 0.05, height = 0),
  point_alpha = 1, aes(height =..ndensity..), alpha = 0) + 
  xlim(0,800) + xlab("SST: Stop-signal delay (ms)") + fig1_theme1 + ylab("density")

p.ssrt <- ggplot(behav.adult, aes(x=tfmri_sst_all_beh_total_meanrt, y=dataset)) +
  geom_density_ridges(jittered_points = TRUE, 
  position = position_points_jitter(width = 0.05, height = 0),
  point_alpha = 1, aes(height =..ndensity..), alpha = 0) + 
  xlim(-800,800) + xlab("SST: Stop-signal reaction time (ms)") + fig1_theme2

p.reward_acc  <- ggplot(behav.adult, aes(x=tfmri_mid_all_beh_hrwpfb_rate, y=dataset)) +
  geom_density_ridges(jittered_points = TRUE, 
  position = position_points_jitter(width = 0.05, height = 0),
  point_alpha = 1, aes(height =..ndensity..), alpha = 0) + 
  xlim(0,100) + xlab("MID: Reward % accuracy") + fig1_theme2

p.loss_acc    <- ggplot(behav.adult, aes(x=tfmri_mid_all_beh_hlpfb_rate, y=dataset))  +
  geom_density_ridges(jittered_points = TRUE, 
  position = position_points_jitter(width = 0.05, height = 0),
  point_alpha = 1, aes(height =..ndensity..), alpha = 0) + 
  xlim(0,100) + xlab("MID: Loss % accuracy") + fig1_theme2

p.neutral_acc <- ggplot(behav.adult, aes(x=tfmri_mid_all_beh_ntpfb_rate, y=dataset))  +
  geom_density_ridges(jittered_points = TRUE, 
  position = position_points_jitter(width = 0.05, height = 0),
  point_alpha = 1, aes(height =..ndensity..), alpha = 0) + 
  xlim(0,100) + xlab("MID: Neutral % accuracy") + fig1_theme2

pBlank <- ggplot() + theme_void()
g2<-arrangeGrob(p.ssd, p.ssrt, pBlank, p.reward_acc, p.loss_acc, p.neutral_acc, p.0bk, p.2bk, pBlank, p.dprime, p.hit_rate, p.falsealarm_rate, nrow = 4)
ggsave("Figure1_adultABCD.pdf", g2, width = 12, height = 8, units = c("in"), dpi = 300)


######################################################################################
################################# GET PERCENTILES #################################### 
######################################################################################
behav.join <- rbind(behav.adult,behav.abcd)
behav.join$dataset <- as.factor(behav.join$dataset)
behav.join$dataset <- fct_rev(behav.join$dataset)

ecdf(behav.join$tfmri_nb_all_beh_c0b_rate[which(behav.join$dataset=='abcd')])(median(behav.join$tfmri_nb_all_beh_c0b_rate[which(behav.join$dataset=='adult')],na.rm=TRUE))
ecdf(behav.join$tfmri_nb_all_beh_c2b_rate[which(behav.join$dataset=='abcd')])(median(behav.join$tfmri_nb_all_beh_c2b_rate[which(behav.join$dataset=='adult')],na.rm=TRUE))
ecdf(behav.join$overall_dprime[which(behav.join$dataset=='abcd')])(median(behav.join$overall_dprime[which(behav.join$dataset=='adult')],na.rm=TRUE))
ecdf(behav.join$hit_rate[which(behav.join$dataset=='abcd')])(median(behav.join$hit_rate[which(behav.join$dataset=='adult')],na.rm=TRUE))
1-ecdf(behav.join$falsealarm_rate[which(behav.join$dataset=='abcd')])(median(behav.join$falsealarm_rate[which(behav.join$dataset=='adult')],na.rm=TRUE))
1-ecdf(behav.join$tfmri_sst_all_beh_tot_mssd[which(behav.join$dataset=='abcd')])(median(behav.join$tfmri_sst_all_beh_tot_mssd[which(behav.join$dataset=='adult')],na.rm=TRUE))
1-ecdf(behav.join$tfmri_sst_all_beh_total_meanrt[which(behav.join$dataset=='abcd')])(median(behav.join$tfmri_sst_all_beh_total_meanrt[which(behav.join$dataset=='adult')],na.rm=TRUE))
ecdf(behav.join$tfmri_mid_all_beh_hrwpfb_rate[which(behav.join$dataset=='abcd')])(median(behav.join$tfmri_mid_all_beh_hrwpfb_rate[which(behav.join$dataset=='adult')],na.rm=TRUE))
ecdf(behav.join$tfmri_mid_all_beh_hlpfb_rate[which(behav.join$dataset=='abcd')])(median(behav.join$tfmri_mid_all_beh_hlpfb_rate[which(behav.join$dataset=='adult')],na.rm=TRUE))
ecdf(behav.join$tfmri_mid_all_beh_ntpfb_rate[which(behav.join$dataset=='abcd')])(median(behav.join$tfmri_mid_all_beh_ntpfb_rate[which(behav.join$dataset=='adult')],na.rm=TRUE))

