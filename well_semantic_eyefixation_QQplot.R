library(ggplot2)
library(dplyr)

data <- read.csv("well_semantic_eyefixation_timebin_plot.csv")
data <- data[-c(1)]

look <- subset(data, look_object!="TA" & look_object!="DA" & look_object!="DI")

### Looks to TI
plot <- ggplot(data=look, aes (y=mean, x=TimeBin, color=cond))+
  labs(y = "Proportion of fixations (%)", x = "Time relative to verb onset (msec)")+
  theme_classic()+
  ylim(0, 0.5)+
  geom_line(aes(linetype=cond)) +
  geom_point(aes(shape=cond)) +
  geom_errorbar(aes(ymin=mean-se*0.8, ymax=mean+se*0.8), size=0.2, width=15.5,position=position_dodge(0.05))+
  theme(panel.background = element_rect(fill = "white", colour = "black",size = 1.5,linetype = "solid"))+
  geom_vline(xintercept = 687, linetype="dashed", color = "black")+
  geom_vline(xintercept = 1720, linetype="dashed", color = "black")+
  theme(text = element_text(size=20,color="black"), axis.ticks.length = unit(.3, "cm"),axis.ticks = element_line(color="black"),
        axis.title = element_text(color="black", size=20, face=2))+
  scale_x_continuous(breaks=seq(0,3900,400))+
  scale_colour_manual(values=c(predictable="grey20",unpredictable="grey60"))+
  guides(color=guide_legend(title="Condition"))+
  ylab("Proportion of fixations to semantically-relevant instrument (%)") 



ggsave(plot,file = "semantic_eyefixation_qqplot.eps",width = 14, height = 9, dpi=1000, bg = "transparent")
