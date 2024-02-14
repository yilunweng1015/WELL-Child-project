library(ggplot2)
library(dplyr)

data <- read.csv("well_vb_eyefixation_timebin_plot.csv")
data <- subset(data,TimeBin<3600)

data <- data %>%
  dplyr::mutate(TrainingType = ifelse(cond == "modtest_1", "Modifier", 
                        ifelse(cond == "modtest_2", "Modifier",
                               ifelse(cond == "pre_test_mod", "Modifier",
                                      ifelse(cond == "modtest_3", "Modifier", "Instrument"))))) %>%
  as.data.frame()

data <- data %>%
  dplyr::mutate(order = 
           ifelse(cond == "pre_test_mod", "test0",
                  ifelse(cond == "pre_test_inst", "test0",
                         ifelse(cond == "modtest_1", "test1",
                                ifelse(cond == "insttest_1", "test1",
                                       ifelse(cond == "modtest_2", "test2", 
                                              ifelse(cond == "insttest_2", "test2",
                                                     ifelse(cond == "modtest_3", "test3", "test3")))))))) %>%
  as.data.frame()


data <- subset(data, cond!="modtrain" & cond!="insttrain" & cond!="unpredictable" & cond!="predictable")
look <- subset(data, look_object!="DA" & look_object!="DI")

### Plot the eye movement patterns across the whole sentence in Testing Time Point 3 (last testing)
look_t3 <- subset(look,order!="test0" & order!="test1" & order!="test2")
plot <- ggplot(data=look_t3, aes (y=mean, x=TimeBin, color=look_object))+
  labs(y = "Proportion of fixations (%)", x = "Time relative to verb onset (msec)")+
  theme_classic()+
  ylim(0, 0.5)+
  geom_line(aes(linetype=TrainingType)) +
  geom_point(aes(shape=TrainingType)) +
  geom_errorbar(aes(ymin=mean-se*0.3, ymax=mean+se*0.3), size=0.2, width=15.5,position=position_dodge(0.05))+
  theme(panel.background = element_rect(fill = "white", colour = "black",size = 1.5,linetype = "solid"))+
  geom_vline(xintercept = 687, linetype="dashed", color = "black")+
  geom_vline(xintercept = 1720, linetype="dashed", color = "black")+
  theme(text = element_text(size=20,color="black"), axis.ticks.length = unit(.3, "cm"),axis.ticks = element_line(color="black"),
        axis.title = element_text(color="black", size=20, face=2))+
  scale_x_continuous(breaks=seq(0,3600,400))+
  scale_colour_manual(values=c(TA="grey20",TI="grey60"))+
  guides(color=guide_legend(title="Look Region"))

ggsave(plot,file = "vb_eyefixation_test3_qqplot.eps",width = 11, height = 6, dpi=1000, bg = "transparent")

