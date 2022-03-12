library(ggplot2)
library(data.table)
library(gridExtra)
library(dplyr)

rm(list=ls())

behav       <- read.delim("adultABCD_behav.csv",sep=",", na.strings=c(""," ","NA"), header=TRUE, skipNul=TRUE)
behav.table <- data.table(behav)

color1 <- "#AE64DC"
color2 <- "#CD68D7"
color3 <- "#E45EB3"
color4 <- "#EF6996"
color5 <- "#EE8C82"
color6 <- "#F5BDAF"
color7 <- "#29066B"

############ Plot data ############
# EN-Back 0-back
id.0bk <- as_data_frame(behav.table[behav.table$task=='enb' & behav.table$measure=='zeroback'])
gd.0bk <- id.0bk %>% 
  group_by(task,measure,session) %>% 
  summarise(value = mean(value))
p.0bk <- ggplot(id.0bk, aes(y=value, x=session)) +
  geom_line(size=.5, alpha=.7, aes(group = subject, color=subject)) +
  scale_color_manual(values=c(color1,color2,color3,color4,color5,color6,color7)) +
  geom_line(data=gd.0bk, size=1, alpha=1, group=1) +
  ylab("EN-back: 0-back % accuracy") + ylim(80, 100) +
  theme_minimal() + theme(panel.grid.minor = element_blank()) + theme(panel.grid.major.x = element_blank()) + theme(axis.title.x = element_blank()) +
  theme(legend.position = "none")

# EN-Back 2-back
id.2bk <- as_data_frame(behav.table[behav.table$task=='enb' & behav.table$measure=='twoback'])
gd.2bk <- id.2bk %>% 
  group_by(task,measure,session) %>% 
  summarise(value = mean(value))
p.2bk <- ggplot(id.2bk, aes(y=value, x=session)) +
  geom_line(size=.5, alpha=.7, aes(group = subject, color=subject)) +
  scale_color_manual(values=c(color1,color2,color3,color4,color5,color6,color7)) +
  geom_line(data=gd.2bk, size=1, alpha=1, group=1) +
  ylab("EN-back: 2-back % accuracy") + ylim(80, 100) + 
  theme_minimal() + theme(panel.grid.minor = element_blank()) + theme(panel.grid.major.x = element_blank()) + theme(axis.title.x = element_blank()) +
  theme(legend.position = "none") 

# Rec Mem d'
id.dprime <- as_data_frame(behav.table[behav.table$task=='recmem' & behav.table$measure=='dprime'])
gd.dprime <- id.dprime %>% 
  group_by(task,measure,session) %>% 
  summarise(value = mean(value))
p.dprime <- ggplot(id.dprime, aes(y=value, x=session)) +
  geom_line(size=.5, alpha=.7, aes(group = subject, color=subject)) +
  scale_color_manual(values=c(color1,color2,color3,color4,color5,color6,color7)) +
  geom_line(data=gd.dprime, size=1, alpha=1, group=1) +
  ylab("Rec. Memory: d'") + ylim(0,4) + 
  theme_minimal() + theme(panel.grid.minor = element_blank()) + theme(panel.grid.major.x = element_blank()) + theme(axis.title.x = element_blank()) +
  theme(legend.position = "none") 

# Rec Mem hit rate
id.hit_rate <- as_data_frame(behav.table[behav.table$task=='recmem' & behav.table$measure=='hit_rate'])
gd.hit_rate <- id.hit_rate %>% 
  group_by(task,measure,session) %>% 
  summarise(value = mean(value))
p.hit_rate <- ggplot(id.hit_rate, aes(y=value, x=session)) +
  geom_line(size=.5, alpha=.7, aes(group = subject, color=subject)) +
  scale_color_manual(values=c(color1,color2,color3,color4,color5,color6,color7)) +
  geom_line(data=gd.hit_rate, size=1, alpha=1, group=1) +
  ylab("Rec. Memory: Hit rate") + ylim(0,100) + 
  theme_minimal() + theme(panel.grid.minor = element_blank()) + theme(panel.grid.major.x = element_blank()) + theme(axis.title.x = element_blank()) +
  theme(legend.position = "none")

# Rec Mem false alarm rate
id.falsealarm_rate <- as_data_frame(behav.table[behav.table$task=='recmem' & behav.table$measure=='falsealarm_rate'])
gd.falsealarm_rate <- id.falsealarm_rate %>% 
  group_by(task,measure,session) %>% 
  summarise(value = mean(value))
p.falsealarm_rate <- ggplot(id.falsealarm_rate, aes(y=value, x=session)) +
  geom_line(size=.5, alpha=.7, aes(group = subject, color=subject)) +
  scale_color_manual(values=c(color1,color2,color3,color4,color5,color6,color7)) +
  geom_line(data=gd.falsealarm_rate, size=1, alpha=1, group=1) +
  ylab("Rec. Memory: False alarm rate") + ylim(0,100) + 
  theme_minimal() + theme(panel.grid.minor = element_blank()) + theme(panel.grid.major.x = element_blank()) + theme(axis.title.x = element_blank()) +
  theme(legend.position = "none") 

# SST stop-signal delay
id.ssd <- as_data_frame(behav.table[behav.table$task=='sst' & behav.table$measure=='ssd'])
gd.ssd <- id.ssd %>% 
  group_by(task,measure,session) %>% 
  summarise(value = mean(value))
p.ssd <- ggplot(id.ssd, aes(y=value, x=session)) +
  geom_line(size=.5, alpha=.7, aes(group = subject, color=subject)) +
  scale_color_manual(values=c(color1,color2,color3,color4,color5,color6,color7)) +
  geom_line(data=gd.ssd, size=1, alpha=1, group=1) +
  ylab("SST: Stop-signal delay (ms)") + ylim(0, 400) +
  theme_minimal() + theme(panel.grid.minor = element_blank()) + theme(panel.grid.major.x = element_blank()) + 
  theme(legend.position = "none")

# SST stop-signal reaction time
id.ssrt <- as_data_frame(behav.table[behav.table$task=='sst' & behav.table$measure=='ssrt'])
gd.ssrt <- id.ssrt %>% 
  group_by(task,measure,session) %>% 
  summarise(value = mean(value))
p.ssrt <- ggplot(id.ssrt, aes(y=value, x=session)) +
  geom_line(size=.5, alpha=.7, aes(group = subject, color=subject)) +
  scale_color_manual(values=c(color1,color2,color3,color4,color5,color6,color7)) +
  geom_line(data=gd.ssrt, size=1, alpha=1, group=1) +
  ylab("SST: Stop-signal RT (ms)") + ylim(0, 400) +
  theme_minimal() + theme(panel.grid.minor = element_blank()) + theme(panel.grid.major.x = element_blank()) + theme(axis.title.x = element_blank()) +
  theme(legend.position = "none") 

# MID practice RT
id.pracrt <- as_data_frame(behav.table[behav.table$task=='mid' & behav.table$measure=='pracrt'])
gd.pracrt <- id.pracrt %>% 
  group_by(task,measure,session) %>% 
  summarise(value = mean(value))
p.pracrt <- ggplot(id.pracrt, aes(y=value, x=session)) +
  geom_line(size=.5, alpha=.7, aes(group = subject, color=subject)) +
  scale_color_manual(values=c(color1,color2,color3,color4,color5,color6,color7)) +
  geom_line(data=gd.pracrt, size=1, alpha=1, group=1) +
  ylab("MID: Practice RT (ms)") + ylim(0, 400) +
  theme_minimal() + theme(panel.grid.minor = element_blank()) + theme(panel.grid.major.x = element_blank()) + theme(axis.title.x = element_blank()) +
  theme(legend.position = "none") 

# MID reward trial accuracy
id.reward_acc <- as_data_frame(behav.table[behav.table$task=='mid' & behav.table$measure=='reward_acc'])
gd.reward_acc <- id.reward_acc %>% 
  group_by(task,measure,session) %>% 
  summarise(value = mean(value, na.rm=TRUE))
p.reward_acc <- ggplot(id.reward_acc, aes(y=value, x=session)) +
  geom_line(size=.5, alpha=.7, aes(group = subject, color=subject)) +
  scale_color_manual(values=c(color1,color2,color3,color4,color5,color6,color7)) +
  geom_line(data=gd.reward_acc, size=1, alpha=1, group=1) +
  ylab("MID: Reward % accuracy") + ylim(0, 100) + 
  theme_minimal() + theme(panel.grid.minor = element_blank()) + theme(panel.grid.major.x = element_blank()) + theme(axis.title.x = element_blank()) +
  theme(legend.position = "none") 

# MID loss trial accuracy
id.loss_acc <- as_data_frame(behav.table[behav.table$task=='mid' & behav.table$measure=='loss_acc'])
gd.loss_acc <- id.loss_acc %>% 
  group_by(task,measure,session) %>% 
  summarise(value = mean(value, na.rm=TRUE))
p.loss_acc <- ggplot(id.loss_acc, aes(y=value, x=session)) +
  geom_line(size=.5, alpha=.7, aes(group = subject, color=subject)) +
  scale_color_manual(values=c(color1,color2,color3,color4,color5,color6,color7)) +
  geom_line(data=gd.loss_acc, size=1, alpha=1, group=1) +
  ylab("MID: Loss % accuracy") + ylim(0, 100) + 
  theme_minimal() + theme(panel.grid.minor = element_blank()) + theme(panel.grid.major.x = element_blank()) + theme(axis.title.x = element_blank()) +
  theme(legend.position = "none") 

# MID neutral trial accuracy
id.neutral_acc <- as_data_frame(behav.table[behav.table$task=='mid' & behav.table$measure=='neutral_acc'])
gd.neutral_acc <- id.neutral_acc %>% 
  group_by(task,measure,session) %>% 
  summarise(value = mean(value, na.rm=TRUE))
p.neutral_acc <- ggplot(id.neutral_acc, aes(y=value, x=session)) +
  geom_line(size=.5, alpha=.7, aes(group = subject, color=subject)) +
  scale_color_manual(values=c(color1,color2,color3,color4,color5,color6,color7)) +
  geom_line(data=gd.neutral_acc, size=1, alpha=1, group=1) +
  ylab("MID: Neutral % accuracy") + ylim(0, 100) + 
  theme_minimal() + theme(panel.grid.minor = element_blank()) + theme(panel.grid.major.x = element_blank()) + theme(axis.title.x = element_blank()) +
  theme(legend.position = "none") 

pBlank <- ggplot() + theme_void()

g<-arrangeGrob(p.ssd, p.ssrt, pBlank, pBlank, p.pracrt, p.reward_acc, p.loss_acc, p.neutral_acc, p.0bk, p.2bk, pBlank, pBlank, p.dprime, p.hit_rate, p.falsealarm_rate, nrow = 4)
ggsave("Figure2_adultABCD.pdf", g, width = 10, height = 10, units = c("in"), dpi = 300)
