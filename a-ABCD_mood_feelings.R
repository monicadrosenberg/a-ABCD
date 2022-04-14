############ Clear environment and load data ############
rm(list=ls())
library(data.table)
library(dplyr)
library(stringr)
feeling      <- read.delim("a-ABCD_mood_feelings.csv",sep=",", na.strings=c(""," ","NA"), header=TRUE, skipNul=TRUE)
feeling      =  feeling[-1,]
feeling      <- feeling%>%dplyr::filter( NDAR != "NDARUD216XXV")

cols.factor           <- c("NDAR")
feeling[cols.factor]  <- lapply(feeling[cols.factor], factor)

cols.numeric          <- c("DurationInSeconds","SessionNumber", "prescan_1","prescan_2",	"prescan_3","prescan_4","prescan_5","prescan_6","prescan_7","prescan_8","prescan_9",	"prescan_10",	"postscan_1",	"postscan_2",	"postscan_3",	"postscan_4",	"postscan_5",	"postscan_6",	"postscan_7",	"postscan_8",	"postscan_9",	"postscan_10",	"excited_1",	"excited_2",	"excited_3",	"excited_4",	"excited_5",	"nervous_1",	"nervous_2",	"nervous_3",	"nervous_4",	"nervous_5",	"try_1",	"try_2",	"try_3",	"try_4",	"try_5","think_1")
feeling[cols.numeric] <- lapply(feeling[cols.numeric], as.integer)

feeling.table           <- data.table(feeling)
feeling.table.long      <- melt(setDT(feeling.table), id.vars = c("DurationInSeconds","NDAR","SessionNumber"), variable.name = "measure")
feeling.table.long$type = str_sub(feeling.table.long$measure,1,3)
feeling.table.long$type <- as.factor(feeling.table.long$type)

feeling.table.long$scan_question_number = str_sub(feeling.table.long$measure,-1)
feeling.df <- feeling.table.long[ which(feeling.table.long$type=="pre" | feeling.table.long$type=="pos"), ]
feeling.df$type <- as.factor(feeling.df$type)


############ Run mixed effects models ############
library(lmerTest)
library(lme4)
library(optimx)
options(contrasts = c("contr.sum","contr.poly"))
model_1 <- lmer(value ~ type*SessionNumber + (1 | NDAR), data=feeling.df[feeling.df$scan_question_number=="1",], verbose=FALSE, control = lmerControl(optimizer ='optimx', optCtrl=list(method='L-BFGS-B')))
model_2 <- lmer(value ~ type*SessionNumber + (1 | NDAR), data=feeling.df[feeling.df$scan_question_number=="2",], verbose=FALSE, control = lmerControl(optimizer ='optimx', optCtrl=list(method='L-BFGS-B')))
model_3 <- lmer(value ~ type*SessionNumber + (1 | NDAR), data=feeling.df[feeling.df$scan_question_number=="3",], verbose=FALSE, control = lmerControl(optimizer ='optimx', optCtrl=list(method='L-BFGS-B')))
model_4 <- lmer(value ~ type*SessionNumber + (1 | NDAR), data=feeling.df[feeling.df$scan_question_number=="4",], verbose=FALSE, control = lmerControl(optimizer ='optimx', optCtrl=list(method='L-BFGS-B')))
model_5 <- lmer(value ~ type*SessionNumber + (1 | NDAR), data=feeling.df[feeling.df$scan_question_number=="5",], verbose=FALSE, control = lmerControl(optimizer ='optimx', optCtrl=list(method='L-BFGS-B')))
model_6 <- lmer(value ~ type*SessionNumber + (1 | NDAR), data=feeling.df[feeling.df$scan_question_number=="6",], verbose=FALSE, control = lmerControl(optimizer ='optimx', optCtrl=list(method='L-BFGS-B')))
model_7 <- lmer(value ~ type*SessionNumber + (1 | NDAR), data=feeling.df[feeling.df$scan_question_number=="7",], verbose=FALSE, control = lmerControl(optimizer ='optimx', optCtrl=list(method='L-BFGS-B')))
model_8 <- lmer(value ~ type*SessionNumber + (1 | NDAR), data=feeling.df[feeling.df$scan_question_number=="8",], verbose=FALSE, control = lmerControl(optimizer ='optimx', optCtrl=list(method='L-BFGS-B')))
model_9 <- lmer(value ~ type*SessionNumber + (1 | NDAR), data=feeling.df[feeling.df$scan_question_number=="9",], verbose=FALSE, control = lmerControl(optimizer ='optimx', optCtrl=list(method='L-BFGS-B')))
model_10 <- lmer(value ~ type*SessionNumber + (1 | NDAR), data=feeling.df[feeling.df$scan_question_number=="0",], verbose=FALSE, control = lmerControl(optimizer ='optimx', optCtrl=list(method='L-BFGS-B')))


summary(model_1)
summary(model_2)
summary(model_3)
summary(model_4)
summary(model_5)
summary(model_6)
summary(model_7)
summary(model_8)
summary(model_9)
summary(model_10)


anova(model_7)
library(sjPlot)
plot_model(model_7,  type = "pred", terms = c("type","SessionNumber"))
