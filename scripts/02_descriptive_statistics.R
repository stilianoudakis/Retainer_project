#02_descriptive_statistics

#Loading Libraries
library(openxlsx)
library(ggplot2)

#Setting working directory
setwd("X:/CommonPrograms/Oral Health Services Research Core/Spiro/DuneganKara")

#Absorption Data
absorptiondat <- readRDS("absorptiondat.rds")

##Plots
p1 <- ggplot(absorptiondat, aes(x = Method, y = Absorption, fill=Timing))+
  stat_boxplot(geom = "errorbar")+
  geom_boxplot(aes(fill = Timing))+
  xlab("Method") +
  ylab("Mass (g)")+
  theme(axis.text.x = element_text(size = 15),
        axis.text.y = element_text(size = 15),
        axis.title.x = element_text(size = 20),
        axis.title.y = element_text(size = 20),
        legend.text = element_text(size=15),
        legend.title = element_text(size=20),
        legend.position="top")

dat <- data.frame(ID=absorptiondat$ID[absorptiondat$Timing=="Post"], 
                  Method=absorptiondat$Method[absorptiondat$Timing=="Post"], 
                  Pre=absorptiondat$Absorption[absorptiondat$Timing=="Pre"], 
                  Post=absorptiondat$Absorption[absorptiondat$Timing=="Post"])

p2 <- ggplot(dat, aes(x = Pre, y = Post, color=Method))+
  geom_point()+
  geom_smooth(method=lm, se=FALSE, fullrange=TRUE) +
  xlab("Mass Before (g)") +
  ylab("Mass After (g)")+
  theme(axis.text.x = element_text(size = 15),
        axis.text.y = element_text(size = 15),
        axis.title.x = element_text(size = 20),
        axis.title.y = element_text(size = 20),
        legend.text = element_text(size=15),
        legend.title = element_text(size=20),
        legend.position="top")

ggarrange(p1,p2, ncol=2, nrow=1)

ggplot(absorptiondat, aes(x = Timing, y = Absorption, fill=Method))+
  stat_summary(aes(group = Method, color=Method),geom = "point", fun.y = mean, size = 3) +
  stat_summary(aes(group = Method, color=Method),geom = "line", fun.y = mean, size = 1) +
  stat_summary(aes(group = Method, color=Method,width=.1),fun.data = mean_se, geom = "errorbar")+
  ylab("Absorption (g)")+
  ylim(.4,.5)

ggplot(data = absorptiondat, aes(x = Timing, y = Absorption, group = ID))+
  geom_line(aes(color=Method))+
  #stat_smooth(aes(group = 1), method = "lm", se = FALSE, color="black")+
  stat_smooth(aes(group = 1),method = "lm", size = 1,se=FALSE, color="black")+
  stat_summary(aes(group = 1, color=Method), geom = "point", fun.y = mean,
               shape = 16, size = 3)+
  facet_grid(. ~ Method)+
  ylab("Absorption (g)") +
  theme_bw()+
  theme(legend.position="none")

##Summaries
###TVF
summary(absorptiondat$PreMass[which(absorptiondat$Method=="TVF")])
mean(absorptiondat$PreMass[which(absorptiondat$Method=="TVF")])
sd(absorptiondat$PreMass[which(absorptiondat$Method=="TVF")])
summary(absorptiondat$PostMass[which(absorptiondat$Method=="TVF")])
mean(absorptiondat$PostMass[which(absorptiondat$Method=="TVF")])
sd(absorptiondat$PostMass[which(absorptiondat$Method=="TVF")])

###3D Printed
summary(absorptiondat$PreMass[which(absorptiondat$Method=="3D Printed")])
mean(absorptiondat$PreMass[which(absorptiondat$Method=="3D Printed")])
sd(absorptiondat$PreMass[which(absorptiondat$Method=="3D Printed")])
summary(absorptiondat$PostMass[which(absorptiondat$Method=="3D Printed")])
mean(absorptiondat$PostMass[which(absorptiondat$Method=="3D Printed")])
sd(absorptiondat$PostMass[which(absorptiondat$Method=="3D Printed")])

#HV data
hvdat <- readRDS("hvdat.rds")

##Plots
p1 <- ggplot(hvdat, aes(x = Method, y = HV, fill=Method))+
  stat_boxplot(geom = "errorbar")+
  geom_boxplot()+
  ylab("")+theme_bw()+
  guides(fill=FALSE)+
  theme(axis.text.x = element_text(size = 15),
        axis.text.y = element_text(size = 15),
        axis.title.x = element_text(size = 20),
        axis.title.y = element_text(size = 20),
        legend.text = element_text(size=15),
        legend.title = element_text(size=20),
        legend.position="top")

p2 <- ggplot(hvdat, aes(x = ID, y = HV, fill=Method))+
  stat_summary(aes(group = Method, color=Method),geom = "point", fun.y = mean, size = 3) +
  stat_summary(aes(group = Method, color=Method),geom = "line", fun.y = mean, size = 1) +
  stat_summary(aes(group = Method, color=Method,width=.3),fun.data = mean_se, geom = "errorbar")+
  xlab("Sample Number")+
  ylab("")+theme_bw()+
  theme(axis.text.x = element_text(size = 15),
        axis.text.y = element_text(size = 15),
        axis.title.x = element_text(size = 20),
        axis.title.y = element_text(size = 20),
        legend.text = element_text(size=15),
        legend.title = element_text(size=20),
        legend.position="top")

figure <- ggarrange(p1,p2, ncol=2, nrow=1,common.legend=TRUE, legend="top", labels = "AUTO", font.label = list(size = 20))
figure
annotate_figure(figure, left = text_grob("HV", color = "black", size=20, rot = 90))

ggplot(hvdat, aes(x = ID, y = HV, fill=Method))+
  stat_summary(aes(group = Method, color=Method),geom = "point", fun.y = mean, size = 3) +
  stat_summary(aes(group = Method, color=Method),geom = "line", fun.y = mean, size = 1) +
  stat_summary(aes(group = Method, color=Method,width=.3),fun.data = mean_se, geom = "errorbar")+
  ylab("HV") + 
  theme(axis.text.x = element_text(size=15),
        axis.text.y = element_text(size=15),
        axis.title.x = element_text(size=20),
        axis.title.y = element_text(size=20))


ggplot(data = hvdat, aes(x = Replicate, y = HV, group = ID))+
  geom_line(aes(color=Method))+
  #stat_smooth(aes(group = 1), method = "lm", se = FALSE, color="black")+
  stat_smooth(aes(group = 1),method = "lm", size = 1,se=FALSE, color="black")+
  stat_summary(aes(group = 1, color=Method), geom = "point", fun.y = mean,
               shape = 16, size = 3)+
  facet_grid(. ~ Method)+
  ylab("HV") +
  theme_bw()+
  theme(legend.position="none")


##Summaries
summary(hvdat$HV[which(hvdat$Method=="TVF")])
mean(hvdat$HV[which(hvdat$Method=="TVF")])
sd(hvdat$HV[which(hvdat$Method=="TVF")])
summary(hvdat$HV[which(hvdat$Method=="3D Printed")])
mean(hvdat$HV[which(hvdat$Method=="3D Printed")])
sd(hvdat$HV[which(hvdat$Method=="3D Printed")])


