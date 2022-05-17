############ Clear environment and load data ############
rm(list=ls())

library(data.table)
library(dplyr)
library(ggplot2)

behav               <- read.delim("a-ABCD_behav.csv",sep=",", na.strings=c(""," ","NA"), header=TRUE, skipNul=TRUE)
behav.table         <- data.table(behav)
behav.table$subject <- as.factor(behav.table$subject)

############ Generate data tables ############
id.0bk             <- as_data_frame(behav.table[behav.table$task=='enb' & behav.table$measure=='zeroback'])
id.2bk             <- as_data_frame(behav.table[behav.table$task=='enb' & behav.table$measure=='twoback'])
id.dprime          <- as_data_frame(behav.table[behav.table$task=='recmem' & behav.table$measure=='dprime'])
id.hit_rate        <- as_data_frame(behav.table[behav.table$task=='recmem' & behav.table$measure=='hit_rate'])
id.falsealarm_rate <- as_data_frame(behav.table[behav.table$task=='recmem' & behav.table$measure=='falsealarm_rate'])
id.ssd             <- as_data_frame(behav.table[behav.table$task=='sst' & behav.table$measure=='ssd'])
id.ssrt            <- as_data_frame(behav.table[behav.table$task=='sst' & behav.table$measure=='ssrt'])
id.pracrt          <- as_data_frame(behav.table[behav.table$task=='mid' & behav.table$measure=='pracrt'])
id.reward_acc      <- as_data_frame(behav.table[behav.table$task=='mid' & behav.table$measure=='reward_acc'])
id.loss_acc        <- as_data_frame(behav.table[behav.table$task=='mid' & behav.table$measure=='loss_acc'])
id.neutral_acc     <- as_data_frame(behav.table[behav.table$task=='mid' & behav.table$measure=='neutral_acc'])

############ Scale data ############
id.0bk$value             <- scale(id.0bk$value)
id.2bk$value             <- scale(id.2bk$value)
id.dprime$value          <- scale(id.dprime$value)
id.hit_rate$value        <- scale(id.hit_rate$value)
id.falsealarm_rate$value <- scale(id.falsealarm_rate$value)
id.ssd$value             <- scale(id.ssd$value)
id.ssrt$value            <- scale(id.ssrt$value)
id.pracrt$value          <- scale(id.pracrt$value)
id.reward_acc$value      <- scale(id.reward_acc$value)
id.loss_acc$value        <- scale(id.loss_acc$value)
id.neutral_acc$value     <- scale(id.neutral_acc$value)

############ Run mixed effects models ############
library(lmerTest)
library(lme4)
library(optimx)
options(contrasts = c("contr.sum","contr.poly"))
m.0bk.0 <- lmer(value ~ (1 | subject), data=id.0bk, verbose=FALSE, control = lmerControl(optimizer ='optimx', optCtrl=list(method='L-BFGS-B')))
m.0bk.1 <- lmer(value ~ session + (1 | subject), data=id.0bk, verbose=FALSE, control = lmerControl(optimizer ='optimx', optCtrl=list(method='L-BFGS-B')))
m.0bk.2 <- lmer(value ~ session + (1 + session | subject), data=id.0bk, verbose=FALSE, control = lmerControl(optimizer ='optimx', optCtrl=list(method='L-BFGS-B')))

m.2bk.0 <- lmer(value ~ (1 | subject), data=id.2bk, verbose=FALSE, control = lmerControl(optimizer ='optimx', optCtrl=list(method='L-BFGS-B')))
m.2bk.1 <- lmer(value ~ session + (1 | subject), data=id.2bk, verbose=FALSE, control = lmerControl(optimizer ='optimx', optCtrl=list(method='L-BFGS-B')))
m.2bk.2 <- lmer(value ~ session + (1 + session | subject), data=id.2bk, verbose=FALSE, control = lmerControl(optimizer ='optimx', optCtrl=list(method='L-BFGS-B')))

m.dprime.0 <- lmer(value ~ (1 | subject), data=id.dprime, verbose=FALSE, control = lmerControl(optimizer ='optimx', optCtrl=list(method='L-BFGS-B')))
m.dprime.1 <- lmer(value ~ session + (1 | subject), data=id.dprime, verbose=FALSE, control = lmerControl(optimizer ='optimx', optCtrl=list(method='L-BFGS-B')))
m.dprime.2 <- lmer(value ~ session + (1 + session | subject), data=id.dprime, verbose=FALSE, control = lmerControl(optimizer ='optimx', optCtrl=list(method='L-BFGS-B')))

m.hit_rate.0 <- lmer(value ~ (1 | subject), data=id.hit_rate, verbose=FALSE, control = lmerControl(optimizer ='optimx', optCtrl=list(method='L-BFGS-B')))
m.hit_rate.1 <- lmer(value ~ session + (1 | subject), data=id.hit_rate, verbose=FALSE, control = lmerControl(optimizer ='optimx', optCtrl=list(method='L-BFGS-B')))
m.hit_rate.2 <- lmer(value ~ session + (1 + session | subject), data=id.hit_rate, verbose=FALSE, control = lmerControl(optimizer ='optimx', optCtrl=list(method='L-BFGS-B')))

m.falsealarm_rate.0 <- lmer(value ~ (1 | subject), data=id.falsealarm_rate, verbose=FALSE, control = lmerControl(optimizer ='optimx', optCtrl=list(method='L-BFGS-B')))
m.falsealarm_rate.1 <- lmer(value ~ session + (1 | subject), data=id.falsealarm_rate, verbose=FALSE, control = lmerControl(optimizer ='optimx', optCtrl=list(method='L-BFGS-B')))
m.falsealarm_rate.2 <- lmer(value ~ session + (1 + session | subject), data=id.falsealarm_rate, verbose=FALSE, control = lmerControl(optimizer ='optimx', optCtrl=list(method='L-BFGS-B')))
m.falsealarm_rate.3 <- lmer(value ~ (1 + session | subject), data=id.falsealarm_rate, verbose=FALSE, control = lmerControl(optimizer ='optimx', optCtrl=list(method='L-BFGS-B')))

m.ssd.0 <- lmer(value ~ (1 | subject), data=id.ssd, verbose=FALSE, control = lmerControl(optimizer ='optimx', optCtrl=list(method='L-BFGS-B')))
m.ssd.1 <- lmer(value ~ session + (1 | subject), data=id.ssd, verbose=FALSE, control = lmerControl(optimizer ='optimx', optCtrl=list(method='L-BFGS-B')))
m.ssd.2 <- lmer(value ~ session + (1 + session | subject), data=id.ssd, verbose=FALSE, control = lmerControl(optimizer ='optimx', optCtrl=list(method='L-BFGS-B')))
m.ssd.3 <- lmer(value ~ (1 + session | subject), data=id.ssd, verbose=FALSE, control = lmerControl(optimizer ='optimx', optCtrl=list(method='L-BFGS-B')))

m.ssrt.0 <- lmer(value ~ (1 | subject), data=id.ssrt, verbose=FALSE, control = lmerControl(optimizer ='optimx', optCtrl=list(method='L-BFGS-B')))
m.ssrt.1 <- lmer(value ~ session + (1 | subject), data=id.ssrt, verbose=FALSE, control = lmerControl(optimizer ='optimx', optCtrl=list(method='L-BFGS-B')))
m.ssrt.2 <- lmer(value ~ session + (1 + session | subject), data=id.ssrt, verbose=FALSE, control = lmerControl(optimizer ='optimx', optCtrl=list(method='L-BFGS-B')))

m.pracrt.0 <- lmer(value ~ (1 | subject), data=id.pracrt, verbose=FALSE, control = lmerControl(optimizer ='optimx', optCtrl=list(method='L-BFGS-B')))
m.pracrt.1 <- lmer(value ~ session + (1 | subject), data=id.pracrt, verbose=FALSE, control = lmerControl(optimizer ='optimx', optCtrl=list(method='L-BFGS-B')))
m.pracrt.2 <- lmer(value ~ session + (1 + session | subject), data=id.pracrt, verbose=FALSE, control = lmerControl(optimizer ='optimx', optCtrl=list(method='L-BFGS-B')))

m.reward_acc.0 <- lmer(value ~ (1 | subject), data=id.reward_acc, verbose=FALSE, control = lmerControl(optimizer ='optimx', optCtrl=list(method='L-BFGS-B')))
m.reward_acc.1 <- lmer(value ~ session + (1 | subject), data=id.reward_acc, verbose=FALSE, control = lmerControl(optimizer ='optimx', optCtrl=list(method='L-BFGS-B')))
m.reward_acc.2 <- lmer(value ~ session + (1 + session | subject), data=id.reward_acc, verbose=FALSE, control = lmerControl(optimizer ='optimx', optCtrl=list(method='L-BFGS-B')))

m.loss_acc.0 <- lmer(value ~ (1 | subject), data=id.loss_acc, verbose=FALSE, control = lmerControl(optimizer ='optimx', optCtrl=list(method='L-BFGS-B')))
m.loss_acc.1 <- lmer(value ~ session + (1 | subject), data=id.loss_acc, verbose=FALSE, control = lmerControl(optimizer ='optimx', optCtrl=list(method='L-BFGS-B')))
m.loss_acc.2 <- lmer(value ~ session + (1 + session | subject), data=id.loss_acc, verbose=FALSE, control = lmerControl(optimizer ='optimx', optCtrl=list(method='L-BFGS-B')))

m.neutral_acc.0 <- lmer(value ~ (1 | subject), data=id.neutral_acc, verbose=FALSE, control = lmerControl(optimizer ='optimx', optCtrl=list(method='L-BFGS-B')))
m.neutral_acc.1 <- lmer(value ~ session + (1 | subject), data=id.neutral_acc, verbose=FALSE, control = lmerControl(optimizer ='optimx', optCtrl=list(method='L-BFGS-B')))
m.neutral_acc.2 <- lmer(value ~ session + (1 + session | subject), data=id.neutral_acc, verbose=FALSE, control = lmerControl(optimizer ='optimx', optCtrl=list(method='L-BFGS-B')))

############ Compare models with and without random slopes ############
anova(m.0bk.1, m.0bk.2)
anova(m.2bk.1, m.2bk.2)
anova(m.dprime.1, m.dprime.2)
anova(m.hit_rate.1, m.hit_rate.2)
anova(m.falsealarm_rate.1, m.falsealarm_rate.2)
anova(m.ssd.1, m.ssd.2)
anova(m.ssrt.1, m.ssrt.2)
anova(m.pracrt.1, m.pracrt.2)
anova(m.reward_acc.1, m.reward_acc.2)
anova(m.loss_acc.1, m.loss_acc.2)
anova(m.neutral_acc.1, m.neutral_acc.2)

############ Compute Bayes factors ############
library(bayestestR) # The following version was used to generate the Bayes factors reported in Table 2 of the manuscript: bayestestR_0.9.0 
bayesfactor_models(m.0bk.1, m.0bk.2, denominator = m.0bk.0)
bayesfactor_models(m.2bk.1, m.2bk.2, denominator = m.2bk.0)
bayesfactor_models(m.dprime.1, m.dprime.2, denominator = m.dprime.0)
bayesfactor_models(m.hit_rate.1, m.hit_rate.2, denominator = m.hit_rate.0)

bayesfactor_models(m.falsealarm_rate.1, m.falsealarm_rate.2, m.falsealarm_rate.3, denominator = m.falsealarm_rate.0)
bayesfactor_models(m.falsealarm_rate.0, m.falsealarm_rate.1, m.falsealarm_rate.2, denominator = m.falsealarm_rate.3)

bayesfactor_models(m.ssd.1, m.ssd.2, m.ssd.3, denominator = m.ssd.0)
bayesfactor_models(m.ssd.0, m.ssd.1, m.ssd.2, denominator = m.ssd.3)

bayesfactor_models(m.ssrt.1, m.ssrt.2, denominator = m.ssrt.0)
bayesfactor_models(m.pracrt.1, m.pracrt.2, denominator = m.pracrt.0)
bayesfactor_models(m.reward_acc.1, m.reward_acc.2, denominator = m.reward_acc.0)
bayesfactor_models(m.loss_acc.1, m.loss_acc.2, denominator = m.loss_acc.0)
bayesfactor_models(m.neutral_acc.1, m.neutral_acc.2, denominator = m.neutral_acc.0)

############ Get mixed effects model results ############
summary(m.0bk.1)
anova(m.0bk.1)

summary(m.2bk.1)
anova(m.2bk.1)

summary(m.dprime.1)
anova(m.dprime.1)

summary(m.hit_rate.1)
anova(m.hit_rate.1)

summary(m.falsealarm_rate.2)
anova(m.falsealarm_rate.2)

summary(m.ssd.2)
anova(m.ssd.2)

summary(m.ssrt.1)
anova(m.ssrt.1)

summary(m.pracrt.1)
anova(m.pracrt.1)

summary(m.reward_acc.1)
anova(m.reward_acc.1)

summary(m.loss_acc.1)
anova(m.loss_acc.1)

summary(m.neutral_acc.1)
anova(m.neutral_acc.1)

############ Visualize lme4 output ############
(plot.falsealarm_rate.0 <- ggplot(id.falsealarm_rate, aes(x = as.numeric(session), y = value, colour = subject)) +
  geom_point() +
  theme_classic() +
  geom_line(data = cbind(id.falsealarm_rate, pred = predict(m.falsealarm_rate.0)), aes(y = pred)) +
  theme(legend.position = "none"))

(plot.falsealarm_rate.1 <- ggplot(id.falsealarm_rate, aes(x = as.numeric(session), y = value, colour = subject)) +
  geom_point() +
  theme_classic() +
  geom_line(data = cbind(id.falsealarm_rate, pred = predict(m.falsealarm_rate.1)), aes(y = pred)) +
  theme(legend.position = "none"))

(plot.falsealarm_rate.2 <- ggplot(id.falsealarm_rate, aes(x = as.numeric(session), y = value, colour = subject)) +
  geom_point() +
  theme_classic() +
  geom_line(data = cbind(id.falsealarm_rate, pred = predict(m.falsealarm_rate.2)), aes(y = pred)) +
  theme(legend.position = "none"))

(plot.falsealarm_rate.3 <- ggplot(id.falsealarm_rate, aes(x = as.numeric(session), y = value, colour = subject)) +
    geom_point() +
    theme_classic() +
    geom_line(data = cbind(id.falsealarm_rate, pred = predict(m.falsealarm_rate.3)), aes(y = pred)) +
    theme(legend.position = "none"))
