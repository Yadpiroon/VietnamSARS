# graph series
getwd()
library(tidyverse)
library(gapminder)
library(ggplot2)
library(ggbeeswarm)
library(rstatix)
library(ggpubr)
library(dplyr)
library(survival)
library(corrplot)
library(NADA)
library(NADA2)
library(EnvStats)
library(stats)
library(base)
library(ggsignif)
library(readxl)
library(readxl)
library(patchwork)
library(cowplot)
library(lattice)
library(PASWR)

###############################Fig1 Box plot
#####p1 #####
library(readxl)
Boxplot_site <- read_excel("Boxplot_site.xlsx", 
                           col_types = c("text", "text", "numeric"))
head(Boxplot_site)

library(readxl)
BoxplotSarcov <- read_excel("BoxplotSarcov.xlsx", 
                            col_types = c("text", "text", "numeric"))
head(BoxplotSarcov)

p1 <- ggplot(Boxplot_site, aes(x=Name, y=TC, fill =Poresize)) +
  geom_boxplot(alpha=1, width=0.4, size=0.1) +
  geom_signif(comparisons = list(c("SiteA", "SiteB"),
                                 c("SiteC", "SiteB"),
                                 c("SiteA", "SiteC")), test = "t.test", color = "black",
              map_signif_level = function(p) {
                if(p < 0.001) {
                  return("p < 0.001***")
                } else if(p < 0.01) {
                  return("p < 0.01**")
                } else if(p < 0.05) {
                  return("p < 0.05*")
                } else {
                  return(sprintf("p = %.3f", p))
                }
              }, textsize = 3, y_position = c(6.0, 5.5, 5.7)) +
  stat_boxplot(geom= 'errorbar' , width = 0.4, alpha=1) +
  xlab("Sampling Location") + labs(fill = "Season") + ylab(expression("SARS-CoV-2 concentration " * (Log[10] * " copies/L"))) +
  theme(axis.title.x = element_text(color = "black", size = 9, face = "bold"), #ชื่อแกนx
        axis.title.y = element_text(color = "black", size = 9, face = "bold"), #ชื่อแกนy
        legend.text = element_text(color = "black", size = 7), #detail site label
        legend.title = element_text(color = "black", size = 7, face = "bold"), #site detail label
        legend.position = c(0.97, 0.97),
        legend.justification = c("right", "top"),
        legend.box.just = "right",
        legend.box.margin = margin(t = -5, r = -5, b = -5, l = -5),
        plot.margin = margin(0.5, 0.5, 0.5, 0.5, "mm"),
        axis.text.x = element_text(color = "black", size = 9),
        axis.text.y = element_text(color = "black", size = 9),
        strip.text.y = element_text(color = "black", size = 5, face = "bold"),
        panel.background = element_rect(fill = "white", colour = NA),
        panel.grid.major = element_line(colour = "white", size = 0.1),
        panel.border = element_rect(colour = "black", fill=NA, size=0.5)) +
  scale_x_discrete(limits=c("SiteA", "SiteB", "SiteC")) +
  scale_fill_manual(values=c("#a9dde1", "#f2c0bc", "#a9dde1", "#f2c0bc","#a9dde1", "#f2c0bc"),
                    labels=c("Rainy", "Dry"))

p1
save_plot("p1.jpeg", p1)

#### edited p1 no season diff just site####

library(readxl)
boxplotsarsite <- read_excel("boxplotsarsite.xlsx", 
                             col_types = c("text", "text", "numeric"))
head(boxplotsarsite)

p1x <- ggplot(boxplotsarsite, aes(x = Season, y = TC, color = Season)) +
  geom_jitter(width = 0.2, alpha = 0.5) +
  xlab("Sampling Location") + labs(color = "Sampling Location") + ylab(expression("SARS-CoV-2 concentration " * (Log[10] * " copies/L"))) +
  theme(axis.title.x = element_text(color = "black", size = 9, face = "bold"), #ชื่อแกนx
        axis.title.y = element_text(color = "black", size = 9, face = "bold"), #ชื่อแกนy
        legend.position = "none",
        legend.text = element_text(color = "black", size = 7), #detail site label
        legend.title = element_text(color = "black", size = 7, face = "bold"), #site detail label
        plot.margin = margin(0.5, 0.5, 0.5, 0.5, "mm"),
        axis.text.x = element_text(color = "black", size = 9),
        axis.text.y = element_text(color = "black", size = 9),
        strip.text.y = element_text(color = "black", size = 5, face = "bold"),
        panel.background = element_rect(fill = "white", colour = NA),
        panel.grid.major = element_line(colour = "white", size = 0.1),
        panel.border = element_rect(colour = "black", fill=NA, size=0.5)) +
  geom_hline(yintercept = 3.0, linetype = "dashed", color = "black") +
  annotate("text", x = 2.5, y = 3.0, label = "LOQ", vjust = -0.5, hjust = 1, color = "black") +
  annotate("text", x = 1, y = min(BoxplotSarcov$TC) - (max(BoxplotSarcov$TC) - min(BoxplotSarcov$TC)) * 0.05, label = "n = 40/105", vjust = 1) +
  annotate("text", x = 2, y = min(BoxplotSarcov$TC) - (max(BoxplotSarcov$TC) - min(BoxplotSarcov$TC)) * 0.05, label = "41/105", vjust = 1) +
  annotate("text", x = 3, y = min(BoxplotSarcov$TC) - (max(BoxplotSarcov$TC) - min(BoxplotSarcov$TC)) * 0.05, label = "45/105", vjust = 1) 

p1x
save_plot("p1x.jpeg", p1x)


###p 2####
library(readxl)
BoxplotSarcov <- read_excel("BoxplotSarcov.xlsx", 
                            col_types = c("text", "text", "numeric"))
head(BoxplotSarcov)

####p2##### box plot remove LOD no sig, combine pos no sig, all sig
p2 <- ggplot(BoxplotSarcov, aes(x = Season, y = TC, color = Season)) +
  geom_jitter(width = 0.2, alpha = 0.5) +
  geom_signif(comparisons = list(c("Dry", "Rainy")), test = "t.test", color = "black",
              map_signif_level = function(p) {
                if(p < 0.001) {
                  return("p < 0.001***")
                } else if(p < 0.01) {
                  return("p < 0.01**")
                } else if(p < 0.05) {
                  return("p < 0.05*")
                } else {
                  return(sprintf("p = %.3f", p))
                }
              }, textsize = 3, y_position = 6) +
  xlab("Season") + labs(color = "Season") + ylab(expression("SARS-CoV-2 concentration " * (Log[10] * " copies/L"))) +
  theme(axis.title.x = element_text(color = "black", size = 9, face = "bold"), #ชื่อแกนx
        axis.title.y = element_text(color = "black", size = 9, face = "bold"), #ชื่อแกนy
        legend.position = "none",
        legend.text = element_text(color = "black", size = 7), #detail site label
        legend.title = element_text(color = "black", size = 7, face = "bold"), #site detail label
        plot.margin = margin(0.5, 0.5, 0.5, 0.5, "mm"),
        axis.text.x = element_text(color = "black", size = 9),
        axis.text.y = element_text(color = "black", size = 9),
        strip.text.y = element_text(color = "black", size = 5, face = "bold"),
        panel.background = element_rect(fill = "white", colour = NA),
        panel.grid.major = element_line(colour = "white", size = 0.1),
        panel.border = element_rect(colour = "black", fill=NA, size=0.5)) +
  geom_hline(yintercept = 3.0, linetype = "dashed", color = "black") +
  annotate("text", x = 2.5, y = 3.0, label = "LOQ", vjust = -0.5, hjust = 1, color = "black") +
  annotate("text", x = 1, y = min(BoxplotSarcov$TC) - (max(BoxplotSarcov$TC) - min(BoxplotSarcov$TC)) * 0.05, label = "n = 71/126", vjust = 1) +
  annotate("text", x = 2, y = min(BoxplotSarcov$TC) - (max(BoxplotSarcov$TC) - min(BoxplotSarcov$TC)) * 0.05, label = "55/189", vjust = 1)

p2

save_plot("p2.jpeg", p2)

#######p2 edited SARV####### 4 season; Spring Summer Autumn Winter
library(readxl)
BoxplotSarcovx <- read_excel("BoxplotSarcovx.xlsx", 
                            col_types = c("text", "text", "numeric"))
head(BoxplotSarcovx)

one.way.anova_event <- aov(BoxplotSarcovx$TC ~ BoxplotSarcovx$Season, data = BoxplotSarcovx)
summary(one.way.anova_event)
TukeyHSD(one.way.anova_event)

####p2##### box plot remove LOD no sig, combine pos no sig, all sig
p2x <- ggplot(BoxplotSarcovx, aes(x = Season, y = TC, color = Season)) +
  geom_jitter(width = 0.2, alpha = 0.5) +
  geom_signif(comparisons = list(c("spring", "autumn"),
                                 c("winter", "autumn"),
                                 c("summer", "spring"),
                                 c("winter", "summer")), test = "t.test", color = "black",
              map_signif_level = function(p) {
                if(p < 0.001) {
                  return("p < 0.001***")
                } else if(p < 0.01) {
                  return("p < 0.01**")
                } else if(p < 0.05) {
                  return("p < 0.05*")
                } else {
                  return(sprintf("p = %.3f", p))
                }
              }, textsize = 3, y_position = c(6.0, 6.5, 7.0, 7.5)) +
  xlab("Season") + labs(color = "Season") + ylab(expression("SARS-CoV-2 concentration " * (Log[10] * " copies/L"))) +
  theme(axis.title.x = element_text(color = "black", size = 9, face = "bold"), #ชื่อแกนx
        axis.title.y = element_text(color = "black", size = 9, face = "bold"), #ชื่อแกนy
        legend.position = "none",
        legend.text = element_text(color = "black", size = 7), #detail site label
        legend.title = element_text(color = "black", size = 7, face = "bold"), #site detail label
        plot.margin = margin(0.5, 0.5, 0.5, 0.5, "mm"),
        axis.text.x = element_text(color = "black", size = 9),
        axis.text.y = element_text(color = "black", size = 9),
        strip.text.y = element_text(color = "black", size = 5, face = "bold"),
        panel.background = element_rect(fill = "white", colour = NA),
        panel.grid.major = element_line(colour = "white", size = 0.1),
        panel.border = element_rect(colour = "black", fill=NA, size=0.5)) +
  geom_hline(yintercept = 3.0, linetype = "dashed", color = "black") +
  annotate("text", x = 2.5, y = 3.0, label = "LOQ", vjust = -0.5, hjust = 1, color = "black") +
  annotate("text", x = 1, y = min(BoxplotSarcovx$TC) - (max(BoxplotSarcovx$TC) - min(BoxplotSarcovx$TC)) * 0.05, label = "n = 23/84", vjust = 1) +
  annotate("text", x = 2, y = min(BoxplotSarcovx$TC) - (max(BoxplotSarcovx$TC) - min(BoxplotSarcovx$TC)) * 0.05, label = "43/72", vjust = 1) +
  annotate("text", x = 3, y = min(BoxplotSarcovx$TC) - (max(BoxplotSarcovx$TC) - min(BoxplotSarcovx$TC)) * 0.05, label = "24/84", vjust = 1) +
  annotate("text", x = 4, y = min(BoxplotSarcovx$TC) - (max(BoxplotSarcovx$TC) - min(BoxplotSarcovx$TC)) * 0.05, label = "36/75", vjust = 1)


p2x

save_plot("p2x.jpeg", p2x)

#####p3 #####

library(readxl)
Boxplot_sitepmm <- read_excel("Boxplot_sitepmm.xlsx", 
                              col_types = c("text", "text", "numeric"))
head(Boxplot_sitepmm)

p3 <- ggplot(Boxplot_sitepmm, aes(x=Name, y=TC, fill =Poresize)) +
  geom_boxplot(alpha=1, width=0.4, size=0.1) +
  geom_signif(comparisons = list(c("SiteA", "SiteB"),
                                 c("SiteC", "SiteB"),
                                 c("SiteA", "SiteC")), test = "t.test", color = "black",
              map_signif_level = function(p) {
                if(p < 0.001) {
                  return("p < 0.001***")
                } else if(p < 0.01) {
                  return("p < 0.01**")
                } else if(p < 0.05) {
                  return("p < 0.05*")
                } else {
                  return(sprintf("p = %.3f", p))
                }
              }, textsize = 3, y_position = c(8.5, 8.8, 9.1)) +
  stat_boxplot(geom= 'errorbar' , width = 0.4, alpha=1) +
  xlab("Sampling Location") + labs(fill = "Season") + ylab(expression("PMMoV concentration " * (Log[10] * " copies/L"))) +
  theme(axis.title.x = element_text(color = "black", size = 9, face = "bold"), #ชื่อแกนx
        axis.title.y = element_text(color = "black", size = 9, face = "bold"), #ชื่อแกนy
        legend.text = element_text(color = "black", size = 7), #detail site label
        legend.title = element_text(color = "black", size = 7, face = "bold"), #site detail label
        legend.position = c(0.97, 0.97),
        legend.justification = c("right", "top"),
        legend.box.just = "right",
        legend.box.margin = margin(t = -5, r = -5, b = -5, l = -5),
        plot.margin = margin(0.5, 0.5, 0.5, 0.5, "mm"),
        axis.text.x = element_text(color = "black", size = 9),
        axis.text.y = element_text(color = "black", size = 9),
        strip.text.y = element_text(color = "black", size = 5, face = "bold"),
        panel.background = element_rect(fill = "white", colour = NA),
        panel.grid.major = element_line(colour = "white", size = 0.1),
        panel.border = element_rect(colour = "black", fill=NA, size=0.5)) +
  scale_x_discrete(limits=c("SiteA", "SiteB", "SiteC")) +
  scale_fill_manual(values=c("#f2c0bc", "#a9dde1", "#f2c0bc", "#a9dde1","#f2c0bc", "#a9dde1"),
                    labels=c("Dry", "Rainy"))
p3
save_plot("p3.jpeg", p3)

####edited p3 to p3x site without season diff####

library(readxl)
pmmovsitediffbx <- read_excel("pmmovsitediffbx.xlsx", 
                              col_types = c("text", "text", "numeric"))
head(pmmovsitediffbx)

p3x <- ggplot(pmmovsitediffbx, aes(x = Season, y = TC, color = Season)) +
  geom_jitter(width = 0.2, alpha = 0.5) +
  geom_signif(comparisons = list(c("SiteA", "SiteB"),
                                 c("SiteA", "SiteC"),
                                 c("SiteB", "SiteC")), test = "t.test", color = "black",
              map_signif_level = function(p) {
                if(p < 0.001) {
                  return("p < 0.001***")
                } else if(p < 0.01) {
                  return("p < 0.01**")
                } else if(p < 0.05) {
                  return("p < 0.05*")
                } else {
                  return(sprintf("p = %.3f", p))
                }
              }, textsize = 3, y_position = c(9, 9.5, 10)) +
  xlab("Sampling Location") + labs(color = "Sampling Location") + ylab(expression("PMMoV concentration " * (Log[10] * " copies/L"))) +
  theme(axis.title.x = element_text(color = "black", size = 9, face = "bold"), #ชื่อแกนx
        axis.title.y = element_text(color = "black", size = 9, face = "bold"), #ชื่อแกนy
        legend.position = "none",
        legend.text = element_text(color = "black", size = 7), #detail site label
        legend.title = element_text(color = "black", size = 7, face = "bold"), #site detail label
        plot.margin = margin(0.5, 0.5, 0.5, 0.5, "mm"),
        axis.text.x = element_text(color = "black", size = 9),
        axis.text.y = element_text(color = "black", size = 9),
        strip.text.y = element_text(color = "black", size = 5, face = "bold"),
        panel.background = element_rect(fill = "white", colour = NA),
        panel.grid.major = element_line(colour = "white", size = 0.1),
        panel.border = element_rect(colour = "black", fill=NA, size=0.5))

p3x
save_plot("p3x.jpeg", p3x)



###p 4####
library(readxl)
BoxplotPmm <- read_excel("BoxplotPmm.xlsx", 
                         col_types = c("text", "text", "numeric"))
head(BoxplotPmm)

####p4##### box plot remove LOD no sig, combine pos no sig, all sig
p4 <- ggplot(BoxplotPmm, aes(x = Season, y = TC, color = Season)) +
  geom_jitter(width = 0.2, alpha = 0.5) +
  stat_summary(fun = mean, geom = "crossbar", width = 0.2, 
               color = "black", fatten = 1.5, position = position_dodge(width = 1)) +
  geom_signif(comparisons = list(c("Dry", "Rainy")), test = "t.test", color = "black",
              map_signif_level = function(p) {
                if(p < 0.001) {
                  return("p < 0.001***")
                } else if(p < 0.01) {
                  return("p < 0.01**")
                } else if(p < 0.05) {
                  return("p < 0.05*")
                } else {
                  return(sprintf("p = %.3f", p))
                }
              }, textsize = 3, y_position = 9) +
  xlab("Season") + labs(color = "Season") + ylab(expression("PMMoV concentration " * (Log[10] * " copies/L"))) +
  theme(axis.title.x = element_text(color = "black", size = 9, face = "bold"), #ชื่อแกนx
        axis.title.y = element_text(color = "black", size = 9, face = "bold"), #ชื่อแกนy
        legend.position = "none",
        legend.text = element_text(color = "black", size = 7), #detail site label
        legend.title = element_text(color = "black", size = 7, face = "bold"), #site detail label
        plot.margin = margin(0.5, 0.5, 0.5, 0.5, "mm"),
        axis.text.x = element_text(color = "black", size = 9),
        axis.text.y = element_text(color = "black", size = 9),
        strip.text.y = element_text(color = "black", size = 5, face = "bold"),
        panel.background = element_rect(fill = "white", colour = NA),
        panel.grid.major = element_line(colour = "white", size = 0.1),
        panel.border = element_rect(colour = "black", fill=NA, size=0.5)) +
  annotate("text", x = 1, y = min(BoxplotPmm$TC) - (max(BoxplotPmm$TC) - min(BoxplotPmm$TC)) * 0.05, label = "n = 126/126", vjust = 1) +
  annotate("text", x = 2, y = min(BoxplotPmm$TC) - (max(BoxplotPmm$TC) - min(BoxplotPmm$TC)) * 0.05, label = "189/189", vjust = 1)

p4

save_plot("p4.jpeg", p4)

#####edite 4 season pmmov p4x####

library(readxl)
pmmov4season <- read_excel("pmmov4season.xlsx", 
                           col_types = c("text", "text", "numeric"))
head(pmmov4season)

one.way.anova_event <- aov(pmmov4season$TC ~ pmmov4season$Season, data = pmmov4season)
summary(one.way.anova_event)
TukeyHSD(one.way.anova_event)

####p2##### box plot remove LOD no sig, combine pos no sig, all sig
p4x <- ggplot(pmmov4season, aes(x = Season, y = TC, color = Season)) +
  geom_jitter(width = 0.2, alpha = 0.5) +
  geom_signif(comparisons = list(c("spring", "autumn"),
                                 c("winter", "autumn"),
                                 c("summer", "spring"),
                                 c("winter", "summer")), test = "t.test", color = "black",
              map_signif_level = function(p) {
                if(p < 0.001) {
                  return("p < 0.001***")
                } else if(p < 0.01) {
                  return("p < 0.01**")
                } else if(p < 0.05) {
                  return("p < 0.05*")
                } else {
                  return(sprintf("p = %.3f", p))
                }
              }, textsize = 3, y_position = c(9, 9.5, 10, 10.5)) +
  xlab("Season") + labs(color = "Season") + ylab(expression("PMMoV concentration " * (Log[10] * " copies/L"))) +
  theme(axis.title.x = element_text(color = "black", size = 9, face = "bold"), #ชื่อแกนx
        axis.title.y = element_text(color = "black", size = 9, face = "bold"), #ชื่อแกนy
        legend.position = "none",
        legend.text = element_text(color = "black", size = 7), #detail site label
        legend.title = element_text(color = "black", size = 7, face = "bold"), #site detail label
        plot.margin = margin(0.5, 0.5, 0.5, 0.5, "mm"),
        axis.text.x = element_text(color = "black", size = 9),
        axis.text.y = element_text(color = "black", size = 9),
        strip.text.y = element_text(color = "black", size = 5, face = "bold"),
        panel.background = element_rect(fill = "white", colour = NA),
        panel.grid.major = element_line(colour = "white", size = 0.1),
        panel.border = element_rect(colour = "black", fill=NA, size=0.5)) 
p4x

save_plot("p4x.jpeg", p4x)


######Combine p1-p4 ######
fig1AD <- plot_grid(p1, p3, p2, p4, ncol = 2,
                  labels = c("a", "b", "c", "d"), label_size = 10)

ggsave(file="fig1AD.jpeg", fig1AD, width= 180, height = 180, units = "mm", dpi=600)

fig1ADx <- plot_grid(p1x, p3x, p2x, p4x, ncol = 2,
                    labels = c("a", "b", "c", "d"), label_size = 10)

ggsave(file="fig1ADx.jpeg", fig1ADx, width= 180, height = 180, units = "mm", dpi=600)

####Graph plot time case and con

###############################Fig2 case and covid
#####simple graph####
library(ggplot2)
library(readxl)
library(scales)
library(dplyr)
library(patchwork) # To display 2 charts together
library(hrbrthemes)
library(tidyverse)
library(tidyquant)
library(lubridate)
library(tidyverse)
library(gapminder)
library(ggplot2)
library(ggbeeswarm)
library(rstatix)
library(ggpubr)
library(dplyr)
library(survival)
library(corrplot)
library(NADA)
library(NADA2)
library(EnvStats)
library(stats)
library(base)
library(ggsignif)
library(readxl)
library(readxl)
library(patchwork)
library(cowplot)
library(lattice)
library(PASWR)

graphH <- read_excel("graphH.xlsx", col_types = c("date", "numeric", "numeric"))
head(graphH)

pz <- graphH %>%
  ggplot() + 
  geom_bar(mapping = aes(x = Date, y = case), stat = "identity") + 
  ylab("Weekly case")
pz

py <- graphH %>%
  ggplot() + 
  geom_point(mapping = aes(x = Date, y = value)) + 
  geom_line(mapping = aes(x = Date, y = value)) + 
  ylab(expression("SARS-CoV2 Concentration" *  (Log[10] * " copies/L")))
py

summary(graphH) #set (min, max) value 3-5.645, case 0-1845400 -> (0-1845500) (0-6) 

gxy <- graphH %>%
  ggplot() + 
  geom_bar(mapping = aes(x = Date, y = (case - min(case)) / 
                           (max(case) - min(case)) * (max(value) - min(value)) + 
                           min(value)), stat = "identity") + 
  geom_point(mapping = aes(x = Date, y = value)) + 
  geom_line(mapping = aes(x = Date, y = value)) + 
  scale_y_continuous((name = "Weekly case"), limits = c(0, 6))
gxy

gxy <- graphH %>%
  ggplot() + 
  geom_bar(mapping = aes(x = Date, y = case * 6 / 1845400), stat = "identity") + 
  geom_point(mapping = aes(x = Date, y = value)) + 
  geom_line(mapping = aes(x = Date, y = value)) + 
  scale_y_continuous((name = "Weekly case"), limits = c(0, 6))
gxy

gxy <- gxy %+% 
  scale_y_continuous(name = expression("SARS-CoV2 Concentration" *  (Log[10] * " copies/L")), 
                     sec.axis = sec_axis(~ . * 1845400 / 6, name = "Weekly case"), limits = c(0, 6))
gxy

#Final plot fp 1####
graphH <- read_excel("graphH.xlsx", col_types = c("date", "numeric", "numeric"))
head(graphH)

fp1 <- graphH %>%
  mutate(Date = as.Date(Date)) %>%
  ggplot() + 
  geom_bar(aes(x = Date, y = case * 6 / 1845400, fill = "Weekly new case"), stat = "identity", color = "#a9dde1") + 
  geom_line(aes(x = Date, y = value, color = "SARS-CoV2 Concentration")) + 
  geom_point(aes(x = Date, y = value, color = "SARS-CoV2 Concentration"), size = 1, shape = 21) + 
  geom_vline(xintercept = as.Date("2022-03-18"), linetype = "dashed", color = "black") + # Add dashed line
  annotate("text", x = as.Date("2022-08-01"), y = 6, label = "6th wave: 2022-03-18", vjust = -0.5, color = "black") + # Add label
  geom_vline(xintercept = as.Date("2022-02-16"), linetype = "dashed", color = "red") + # Add dashed line
  annotate("text", x = as.Date("2022-08-01"), y = 6.5, label = "Peak Concentration: 2022-02-16", vjust = -0.5, color = "red") + # Add label
  geom_vline(xintercept = as.Date("2022-03-23"), linetype = "dashed", color = "blue") + # Add dashed line
  annotate("text", x = as.Date("2022-07-26"), y = 5, label = "Peak case: 2022-03-23", vjust = -0.5, color = "blue") + # Add label
  scale_x_date(
    name = "Month/Year", 
    date_breaks = "2 month",
    date_labels = "%m/%y",
    limits = as.Date(c("2021-10-01", "2023-10-31")) # Set limits for the x-axis
  ) +
  scale_y_continuous(
    name = expression("SARS-CoV2 Concentration " *  (Log[10] * " copies/L")), 
    sec.axis = sec_axis(~ . * 1845400 / 6, name = "Weekly new case"), limits = c(0, 7)) + 
  scale_fill_manual(values = c("Weekly new case" = "#a9dde1")) +
  scale_color_manual(values = c("SARS-CoV2 Concentration" = "#f2c0bc")) +
  theme_bw() + 
  labs(
    title = "Site A",
    x = "Date",
    fill = "Legend",
    color = "Legend"
  ) + labs(fill = " ") +
  theme(
    axis.title.x = element_text(color = "black", size = 8, face = "bold"),
    axis.title.y = element_text(color = "black", size = 8, face = "bold"),
    legend.title = element_text(color = "black", size = 8, face = "bold"),
    legend.position = "top",
    plot.margin = margin(0.5, 0.5, 0.5, 0.5, "mm"),
    axis.text.x = element_text(color = "black", size = 8),
    axis.text.y = element_text(color = "black", size = 8),
    panel.background = element_rect(fill = "white", colour = NA),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_rect(color = "black", fill = NA, size = 0.5)
  ) 

fp1
save_plot("fp1.jpeg", fp1)

######fp 2####
library(readxl)
graphw <- read_excel("graphw.xlsx", col_types = c("date", 
                                                  "numeric", "numeric"))
View(graphw)

fp2 <- graphw %>%
  mutate(Date = as.Date(Date)) %>%
  ggplot() + 
  geom_bar(mapping = aes(x = Date, y = case * 6 / 1845400), stat = "identity", color = "#a9dde1", fill = "#a9dde1") + 
  geom_line(mapping = aes(x = Date, y = value), color = "#f2c0bc") + 
  geom_point(mapping = aes(x = Date, y = value), size = 1, shape = 21, fill = "#f2c0bc", color = "#f2c0bc") + 
  geom_vline(xintercept = as.Date("2022-03-18"), linetype = "dashed", color = "black") + # Add dashed line
  annotate("text", x = as.Date("2022-07-26"), y = 6, label = "6th wave: 2022-03-18", vjust = -0.5, color = "black") + # Add label
  geom_vline(xintercept = as.Date("2022-03-02"), linetype = "dashed", color = "red") + # Add dashed line
  annotate("text", x = as.Date("2022-08-01"), y = 6.5, label = "Peak Concentration: 2022-03-02", vjust = -0.5, color = "red") + # Add label
  geom_vline(xintercept = as.Date("2022-03-23"), linetype = "dashed", color = "blue") + # Add dashed line
  annotate("text", x = as.Date("2022-07-26"), y = 5, label = "Peak case: 2022-03-23", vjust = -0.5, color = "blue") + # Add label
  scale_x_date(
    name = "Month/Year", 
    date_breaks = "2 month",
    date_labels = "%m/%y",
    limits = as.Date(c("2021-10-01", "2023-10-31")) # Set limits for the x-axis
  ) +
  scale_y_continuous(
    name = expression("SARS-CoV2 Concentration" *  (Log[10] * " copies/L")), 
    sec.axis = sec_axis(~ . * 1845400 / 6, name = "Weekly new case"), limits = c(0, 7)) + 
  theme_bw() + 
  labs(title = "WWTP", x = "Date") +
  theme(
    axis.title.x = element_text(color = "black", size = 7, face = "bold"),
    axis.title.y = element_text(color = "black", size = 7, face = "bold"),
    legend.text = element_text(color = "black", size = 7),
    legend.title = element_text(color = "black", size = 7, face = "bold"),
    plot.margin = margin(0.5, 0.5, 0.5, 0.5, "mm"),
    axis.text.x = element_text(color = "black", size = 7),
    axis.text.y = element_text(color = "black", size = 7),
    panel.background = element_rect(fill = "white", colour = NA),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_rect(color = "black", fill = NA, size = 0.5)
  ) 

fp2
save_plot("fp2.jpeg", fp2)

######fp 3####
library(readxl)
graphr <- read_excel("graphr.xlsx", col_types = c("date", 
                                                  "numeric", "numeric"))
head(graphr)

library(tidyverse)
library(scales) # For date formatting

fp3 <- graphr %>%
  mutate(Date = as.Date(Date)) %>%
  ggplot() + 
  geom_bar(aes(x = Date, y = case * 6 / 1845400, fill = "Weekly new case"), stat = "identity", color = "#a9dde1") + 
  geom_line(aes(x = Date, y = value, color = "SARS-CoV2 Concentration")) + 
  geom_point(aes(x = Date, y = value, color = "SARS-CoV2 Concentration"), size = 1, shape = 21) + 
  geom_vline(xintercept = as.Date("2022-03-18"), linetype = "dashed", color = "black") + 
  annotate("text", x = as.Date("2022-07-26"), y = 6, label = "6th wave: 2022-03-18", vjust = -0.5, color = "black") + 
  geom_vline(xintercept = as.Date("2022-02-23"), linetype = "dashed", color = "red") + 
  annotate("text", x = as.Date("2022-08-01"), y = 6.5, label = "Peak Concentration: 2022-02-23", vjust = -0.5, color = "red") + 
  geom_vline(xintercept = as.Date("2022-03-23"), linetype = "dashed", color = "blue") + 
  annotate("text", x = as.Date("2022-07-26"), y = 5, label = "Peak case: 2022-03-23", vjust = -0.5, color = "blue") + 
  scale_x_date(
    name = "Month/Year", 
    date_breaks = "2 month",
    date_labels = "%m/%y",
    limits = as.Date(c("2021-10-01", "2023-10-31"))
  ) +
  scale_y_continuous(
    name = expression("SARS-CoV2 Concentration" * (Log[10] * " copies/L")), 
    sec.axis = sec_axis(~ . * 1845400 / 6, name = "Weekly new case"), limits = c(0, 7)
  ) +
  scale_fill_manual(values = c("Weekly new case" = "#a9dde1")) +
  scale_color_manual(values = c("SARS-CoV2 Concentration" = "#f2c0bc")) +
  theme_bw() + 
  labs(
    title = "River",
    x = "Date",
    fill = "Legend",
    color = "Legend"
  ) + labs(fill = " ") +
  theme(
    axis.title.x = element_text(color = "black", size = 8, face = "bold"),
    axis.title.y = element_text(color = "black", size = 8, face = "bold"),
    legend.title = element_text(color = "black", size = 8, face = "bold"),
    legend.position = "none",
    plot.margin = margin(0.5, 0.5, 0.5, 0.5, "mm"),
    axis.text.x = element_text(color = "black", size = 8),
    axis.text.y = element_text(color = "black", size = 8),
    panel.background = element_rect(fill = "white", colour = NA),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_rect(color = "black", fill = NA, size = 0.5)
  ) 

fp3
save_plot("fp3.jpeg", fp3)

###Fig 2AC####

fig2ac <- plot_grid(fp1, fp2, fp3, ncol = 1,
                    labels = c("a", "b", "c"), label_size = 10)

ggsave(file="fig2ac.jpeg", fig2ac, width= 180, height = 240, units = "mm", dpi=600)


###############################Fig 3 Flip graph
####multigraph####
library(ggplot2)
library(grid)
library(gridExtra)
library(ggplot2)
library(patchwork)
library(readxl)
library(ggplot2)
graphmulti2 <- read_excel("graphmulti2.xlsx", 
                          col_types = c("date", "numeric", "numeric", 
                                        "numeric", "numeric", "numeric", 
                                        "numeric"))
head(graphmulti2)

graphmulti2$date <- as.Date(graphmulti2$date, format = "%m/%d/%Y")
graphmulti2$date
graphmulti2 <- graphmulti2[order(graphmulti2$date, decreasing = TRUE),]
graphmulti2$date <- factor(graphmulti2$date, levels = rev(unique(graphmulti2$date)))

p1 <- ggplot(graphmulti2) +           
  geom_point(aes(Precipitation, date), size = 2, shape = 17, color = "#009E73") +
  xlab(expression(atop("Precipitation", paste("(mm)", sep="")))) +
  scale_x_continuous(position = "top") +
  scale_y_discrete(limits = rev(levels(graphmulti2$date))) + # Set the limits to reverse the levels
  theme(
    axis.title.y = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    axis.title.x = element_text(color = "black", size = 8, face = "bold"),
    legend.title = element_text(color = "black", size = 8, face = "bold"),
    legend.position = "none",
    plot.margin = margin(0.5, 0.5, 0.5, 0.5, "mm"),
    axis.text.x = element_text(color = "black", size = 8),
    panel.background = element_rect(fill = "white", colour = NA),
    panel.grid.major = element_line(color = "grey", size = 0.1, linetype = 2),
    panel.grid.minor = element_blank(),
    panel.border = element_rect(color = "black", fill = NA, size = 0.5),
  )
p1

p2 <- graphmulti2 %>%          
  ggplot(aes(x = Case, y = date, fill = "case")) +
  geom_bar(stat = "identity", fill = "#0072B2") +
  xlab(expression(atop("Weekly new", paste("case", sep="")))) +
  scale_x_continuous(position = "top") +
  scale_y_discrete(limits = rev(levels(graphmulti2$date))) + # Set the limits to reverse the levels
  theme(
    axis.title.y = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    axis.title.x = element_text(color = "black", size = 8, face = "bold"),
    legend.title = element_text(color = "black", size = 8, face = "bold"),
    legend.position = "none",
    plot.margin = margin(0.5, 0.5, 0.5, 0.5, "mm"),
    axis.text.x = element_text(color = "black", size = 8),
    panel.background = element_rect(fill = "white", colour = NA),
    panel.grid.major = element_line(color = "grey", size = 0.1, linetype = 2),
    panel.grid.minor = element_blank(),
    panel.border = element_rect(color = "black", fill = NA, size = 0.5),
  ) 
p2

p3 <- ggplot(graphmulti2) +           
  geom_point(aes(SARSCoV2, date), size = 2, shape = 18, color = "red") +
  xlab(expression(atop("SARSCoV2", paste("(Log copies/L)", sep="")))) +
  ylab("Date (yyyy-mm-dd)") +
  scale_x_continuous(position = "top") +
  scale_y_discrete(limits = rev(levels(graphmulti2$date))) + # Set the limits to reverse the levels
  theme(
    axis.title.x = element_text(color = "black", size = 8, face = "bold"),
    legend.title = element_text(color = "black", size = 8, face = "bold"),
    legend.position = "none",
    plot.margin = margin(0.5, 0.5, 0.5, 0.5, "mm"),
    axis.text.x = element_text(color = "black", size = 8),
    panel.background = element_rect(fill = "white", colour = NA),
    panel.grid.major = element_line(color = "grey", size = 0.1, linetype = 2),
    panel.grid.minor = element_blank(),
    panel.border = element_rect(color = "black", fill = NA, size = 0.5),
  ) 
p3

p4 <- ggplot(graphmulti2) +           
  geom_point(aes(PMMoV, date), size = 2, shape = 15, color = "#56B4E9") +
  xlab(expression(atop("PMMoV", paste("(Log copies/L)", sep="")))) +
  scale_x_continuous(position = "top") +
  scale_y_discrete(limits = rev(levels(graphmulti2$date))) + # Set the limits to reverse the levels
  theme(
    axis.title.y = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    axis.title.x = element_text(color = "black", size = 8, face = "bold"),
    legend.title = element_text(color = "black", size = 8, face = "bold"),
    legend.position = "none",
    plot.margin = margin(0.5, 0.5, 0.5, 0.5, "mm"),
    axis.text.x = element_text(color = "black", size = 8),
    panel.background = element_rect(fill = "white", colour = NA),
    panel.grid.major = element_line(color = "grey", size = 0.1, linetype = 2),
    panel.grid.minor = element_blank(),
    panel.border = element_rect(color = "black", fill = NA, size = 0.5),
  ) 
p4

p5 <- ggplot(graphmulti2) +           
  geom_point(aes(Omicron, date), size = 2, shape = 18, color = "red") +
  xlab(expression(atop("Omicron", paste("(Log copies/L)", sep="")))) +
  scale_x_continuous(position = "top") +
  scale_y_discrete(limits = rev(levels(graphmulti2$date))) + # Set the limits to reverse the levels
  theme(
    axis.title.y = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    axis.title.x = element_text(color = "black", size = 8, face = "bold"),
    legend.title = element_text(color = "black", size = 8, face = "bold"),
    legend.position = "none",
    plot.margin = margin(0.5, 0.5, 0.5, 0.5, "mm"),
    axis.text.x = element_text(color = "black", size = 8),
    panel.background = element_rect(fill = "white", colour = NA),
    panel.grid.major = element_line(color = "grey", size = 0.1, linetype = 2),
    panel.grid.minor = element_blank(),
    panel.border = element_rect(color = "black", fill = NA, size = 0.5),
  ) 
p5

p6 <- ggplot(graphmulti2) +           
  geom_point(aes(Temp, date), size = 2, shape = 17, color = "#009E73") +
  xlab(expression(atop("Temperature", paste("(", degree, "C", ")", sep="")))) +
  scale_x_continuous(position = "top") +
  scale_y_discrete(limits = rev(levels(graphmulti2$date))) + # Set the limits to reverse the levels
  theme(
    axis.title.y = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    axis.title.x = element_text(color = "black", size = 8, face = "bold"),
    legend.title = element_text(color = "black", size = 8, face = "bold"),
    legend.position = "none",
    plot.margin = margin(0.5, 0.5, 0.5, 0.5, "mm"),
    axis.text.x = element_text(color = "black", size = 8),
    panel.background = element_rect(fill = "white", colour = NA),
    panel.grid.major = element_line(color = "grey", size = 0.1, linetype = 2),
    panel.grid.minor = element_blank(),
    panel.border = element_rect(color = "black", fill = NA, size = 0.5),
  ) 
p6

fig3 <- plot_grid(p3, p5, p4, p1, p6, p2, ncol = 6, rel_widths = c(2, 1, 1, 1, 1, 1), label_size = 10)
fig3

ggsave(file="fig3.jpeg", fig3, width= 190, height = 120, units = "mm", dpi=600)

#####edited column graph fig3x p1x-p6x####

####multigraph####
library(ggplot2)
library(grid)
library(gridExtra)
library(ggplot2)
library(patchwork)
library(readxl)
library(ggplot2)
library(readxl)
library(readxl)

graphmulti2 <- read_excel("graphmulti2.xlsx", 
                          col_types = c("date", "numeric", "numeric", 
                                        "numeric", "numeric", "numeric", 
                                        "numeric", "numeric", "numeric", 
                                        "numeric", "numeric", "numeric", 
                                        "numeric"))
head(graphmulti2)



graphmulti2$date <- as.Date(graphmulti2$date, format = "%m/%d/%Y")
graphmulti2$date
graphmulti2 <- graphmulti2[order(graphmulti2$date, decreasing = TRUE),]
graphmulti2$date <- factor(graphmulti2$date, levels = rev(unique(graphmulti2$date)))

p0x <- ggplot(graphmulti2) +           
  geom_point(aes(humidity, date), size = 2, shape = 17, color = "#009E73") +
  xlab(expression(atop("Humidity", paste("(%)", sep="")))) +
  scale_x_continuous(position = "top", n.breaks= 4) +
  scale_y_discrete(limits = rev(levels(graphmulti2$date))) + # Set the limits to reverse the levels
  theme(
    axis.title.y = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    axis.title.x = element_text(color = "black", size = 11, face = "bold"),
    legend.title = element_text(color = "black", size = 11, face = "bold"),
    legend.position = "none",
    plot.margin = margin(0.5, 0.5, 0.5, 0.5, "mm"),
    axis.text.x = element_text(color = "black", size = 10),
    panel.background = element_rect(fill = "white", colour = NA),
    panel.grid.major = element_line(color = "grey", size = 0.1, linetype = 2),
    panel.grid.minor = element_blank(),
    panel.border = element_rect(color = "black", fill = NA, size = 0.5),
  )
p0x

p1x <- ggplot(graphmulti2) +           
  geom_point(aes(Precipitation, date), size = 2, shape = 17, color = "#009E73") +
  xlab(expression(atop("Precipitation", paste("(mm/day)", sep="")))) +
  scale_x_continuous(position = "top", n.breaks= 4) +
  scale_y_discrete(limits = rev(levels(graphmulti2$date))) + # Set the limits to reverse the levels
  theme(
    axis.title.y = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    axis.title.x = element_text(color = "black", size = 11, face = "bold"),
    legend.title = element_text(color = "black", size = 11, face = "bold"),
    legend.position = "none",
    plot.margin = margin(0.5, 0.5, 0.5, 0.5, "mm"),
    axis.text.x = element_text(color = "black", size = 8),
    panel.background = element_rect(fill = "white", colour = NA),
    panel.grid.major = element_line(color = "grey", size = 0.1, linetype = 2),
    panel.grid.minor = element_blank(),
    panel.border = element_rect(color = "black", fill = NA, size = 0.5),
  )
p1x

library(ggplot2)
library(scales)

# Custom label function to display log scale values as powers of 10
log10_labels <- function(x) {
  ifelse(x == 0, "0", parse(text = paste0("10^", log10(x))))
}

p2x <- graphmulti2 %>%          
  ggplot(aes(x = Case, y = date, fill = "case")) +
  geom_bar(stat = "identity", fill = "#0072B2") +
  xlab(expression(atop("New cases", paste("(no. of cases)", sep="")))) +
  scale_x_continuous(
    position = "top", 
    trans = "log10", 
    labels = log10_labels
  ) +
  scale_y_discrete(limits = rev(levels(graphmulti2$date))) + # Set the limits to reverse the levels
  theme(
    axis.title.y = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    axis.title.x = element_text(color = "black", size = 11, face = "bold"),
    legend.title = element_text(color = "black", size = 11, face = "bold"),
    legend.position = "none",
    plot.margin = margin(0.5, 0.5, 0.5, 0.5, "mm"),
    axis.text.x = element_text(color = "black", size = 8),
    panel.background = element_rect(fill = "white", colour = NA),
    panel.grid.major = element_line(color = "grey", size = 0.1, linetype = 2),
    panel.grid.minor = element_blank(),
    panel.border = element_rect(color = "black", fill = NA, size = 0.5),
  ) 
p2x


p3x <- ggplot(graphmulti2) +           
  geom_point(aes(SARSCoV2, date), size = 2, shape = 18, color = "red") +
  xlab(expression(atop("SARS-CoV-2", paste("(Log copies/L)", sep="")))) +
  ylab("Date (yyyy-mm)") +
  scale_x_continuous(position = "top", n.breaks= 4) +
  scale_y_discrete(
    limits = rev(levels(graphmulti2$date)), 
    labels = function(x) format(as.Date(x), "%Y-%m")
  ) + # Set the limits to reverse the levels and format date labels
  theme(
    axis.title.x = element_text(color = "black", size = 11, face = "bold"),
    axis.title.y = element_text(color = "black", size = 11, face = "bold"),
    legend.title = element_text(color = "black", size = 11, face = "bold"),
    legend.position = "none",
    plot.margin = margin(0.5, 0.5, 0.5, 0.5, "mm"),
    axis.text.x = element_text(color = "black", size = 8),
    axis.text.y = element_text(color = "black", size = 8),
    panel.background = element_rect(fill = "white", colour = NA),
    panel.grid.major = element_line(color = "grey", size = 0.1, linetype = 2),
    panel.grid.minor = element_blank(),
    panel.border = element_rect(color = "black", fill = NA, size = 0.5),
  ) 
p3x


p4x <- ggplot(graphmulti2) +           
  geom_point(aes(PMMoV, date), size = 2, shape = 15, color = "#56B4E9") +
  xlab(expression(atop("PMMoV", paste("(Log copies/L)", sep="")))) +
  scale_x_continuous(position = "top") +
  scale_y_discrete(limits = rev(levels(graphmulti2$date))) + # Set the limits to reverse the levels
  theme(
    axis.title.y = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    axis.title.x = element_text(color = "black", size = 11, face = "bold"),
    legend.title = element_text(color = "black", size = 11, face = "bold"),
    legend.position = "none",
    plot.margin = margin(0.5, 0.5, 0.5, 0.5, "mm"),
    axis.text.x = element_text(color = "black", size = 8),
    panel.background = element_rect(fill = "white", colour = NA),
    panel.grid.major = element_line(color = "grey", size = 0.1, linetype = 2),
    panel.grid.minor = element_blank(),
    panel.border = element_rect(color = "black", fill = NA, size = 0.5),
  ) 
p4x

p5x <- ggplot(graphmulti2) +           
  geom_point(aes(Omicron, date), size = 2, shape = 18, color = "red") +
  xlab(expression(atop("Omicron_mutant", paste("(Log copies/L)", sep="")))) +
  scale_x_continuous(position = "top", n.breaks= 4) +
  scale_y_discrete(limits = rev(levels(graphmulti2$date))) + # Set the limits to reverse the levels
  theme(
    axis.title.y = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    axis.title.x = element_text(color = "black", size = 11, face = "bold"),
    legend.title = element_text(color = "black", size = 11, face = "bold"),
    legend.position = "none",
    plot.margin = margin(0.5, 0.5, 0.5, 0.5, "mm"),
    axis.text.x = element_text(color = "black", size = 8),
    panel.background = element_rect(fill = "white", colour = NA),
    panel.grid.major = element_line(color = "grey", size = 0.1, linetype = 2),
    panel.grid.minor = element_blank(),
    panel.border = element_rect(color = "black", fill = NA, size = 0.5),
  ) 
p5x

p6x <- ggplot(graphmulti2) +           
  geom_point(aes(temp, date), size = 2, shape = 17, color = "#009E73") +
  xlab(expression(atop("Air temperature", paste("(", degree, "C", ")", sep="")))) +
  scale_x_continuous(position = "top", n.breaks= 4) +
  scale_y_discrete(limits = rev(levels(graphmulti2$date))) + # Set the limits to reverse the levels
  theme(
    axis.title.y = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    axis.title.x = element_text(color = "black", size = 11, face = "bold"),
    legend.title = element_text(color = "black", size = 11, face = "bold"),
    legend.position = "none",
    plot.margin = margin(0.5, 0.5, 0.5, 0.5, "mm"),
    axis.text.x = element_text(color = "black", size = 8),
    panel.background = element_rect(fill = "white", colour = NA),
    panel.grid.major = element_line(color = "grey", size = 0.1, linetype = 2),
    panel.grid.minor = element_blank(),
    panel.border = element_rect(color = "black", fill = NA, size = 0.5),
  ) 
p6x


p7x <- ggplot(graphmulti2) +           
  geom_point(aes(EC, date), size = 2, shape = 17, color = "#009E73") +
  xlab(expression(atop("EC", paste("(µS/cm)", sep="")))) +
  scale_x_continuous(position = "top", n.breaks= 4) +
  scale_y_discrete(limits = rev(levels(graphmulti2$date))) + # Set the limits to reverse the levels
  theme(
    axis.title.y = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    axis.title.x = element_text(color = "black", size = 11, face = "bold"),
    legend.title = element_text(color = "black", size = 11, face = "bold"),
    legend.position = "none",
    plot.margin = margin(0.5, 0.5, 0.5, 0.5, "mm"),
    axis.text.x = element_text(color = "black", size = 8),
    panel.background = element_rect(fill = "white", colour = NA),
    panel.grid.major = element_line(color = "grey", size = 0.1, linetype = 2),
    panel.grid.minor = element_blank(),
    panel.border = element_rect(color = "black", fill = NA, size = 0.5),
  ) 
p7x

head(graphmulti2)

p8x <- ggplot(graphmulti2) +           
  geom_point(aes(pH, date), size = 2, shape = 17, color = "#009E73") +
  xlab(expression(atop("pH"))) +
  scale_x_continuous(position = "top", n.breaks= 4) +
  scale_y_discrete(limits = rev(levels(graphmulti2$date))) + # Set the limits to reverse the levels
  theme(
    axis.title.y = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    axis.title.x = element_text(color = "black", size = 11, face = "bold"),
    legend.title = element_text(color = "black", size = 11, face = "bold"),
    legend.position = "none",
    plot.margin = margin(0.5, 0.5, 0.5, 0.5, "mm"),
    axis.text.x = element_text(color = "black", size = 8),
    panel.background = element_rect(fill = "white", colour = NA),
    panel.grid.major = element_line(color = "grey", size = 0.1, linetype = 2),
    panel.grid.minor = element_blank(),
    panel.border = element_rect(color = "black", fill = NA, size = 0.5),
  ) 
p8x

p9x <- ggplot(graphmulti2) +           
  geom_point(aes(DO, date), size = 2, shape = 17, color = "#009E73") +
  xlab(expression(atop("Dissolved oxygen", paste("(mg/L)")))) +
  scale_x_continuous(position = "top") +
  scale_y_discrete(limits = rev(levels(graphmulti2$date))) + # Set the limits to reverse the levels
  theme(
    axis.title.y = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    axis.title.x = element_text(color = "black", size = 11, face = "bold"),
    legend.title = element_text(color = "black", size = 11, face = "bold"),
    legend.position = "none",
    plot.margin = margin(0.5, 0.5, 0.5, 0.5, "mm"),
    axis.text.x = element_text(color = "black", size = 8),
    panel.background = element_rect(fill = "white", colour = NA),
    panel.grid.major = element_line(color = "grey", size = 0.1, linetype = 2),
    panel.grid.minor = element_blank(),
    panel.border = element_rect(color = "black", fill = NA, size = 0.5),
  ) 
p9x

p10x <- ggplot(graphmulti2) +           
  geom_point(aes(ORP, date), size = 2, shape = 17, color = "#009E73") +
  xlab(expression(atop("ORP", paste("(mV)")))) +
  scale_x_continuous(position = "top", n.breaks= 4) +
  scale_y_discrete(limits = rev(levels(graphmulti2$date))) + # Set the limits to reverse the levels
  theme(
    axis.title.y = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    axis.title.x = element_text(color = "black", size = 11, face = "bold"),
    legend.title = element_text(color = "black", size = 11, face = "bold"),
    legend.position = "none",
    plot.margin = margin(0.5, 0.5, 0.5, 0.5, "mm"),
    axis.text.x = element_text(color = "black", size = 8),
    panel.background = element_rect(fill = "white", colour = NA),
    panel.grid.major = element_line(color = "grey", size = 0.1, linetype = 2),
    panel.grid.minor = element_blank(),
    panel.border = element_rect(color = "black", fill = NA, size = 0.5),
  ) 
p10x

fig3x <- plot_grid(p3x, p5x, p1x, p6x, p2x, p7x, p8x, p10x, ncol = 9, rel_widths = c(2, 1, 1, 1, 1, 1, 1, 1, 1), label_size = 10)
fig3x

fig3x <- plot_grid(p3x, p5x, p6x, p1x, p7x, p8x, p10x, ncol = 7, rel_widths = c(2, 1, 1, 1, 1, 1, 1), label_size = 10)
fig3x

fig3xx <- plot_grid(p3x, p5x, p2x, p1x, p0x, p6x, ncol = 6, rel_widths = c(2, 1, 1, 1, 1, 1), label_size = 10)
fig3xx

ggsave(file="fig3xx.jpeg", fig3xx, width= 200, height = 120, units = "mm", dpi=900)


######corr fig3ab####
####corr plot ####
#########by original####
library(readr)
corr3site <- read_csv("corr3site.csv", col_types = cols(avg_rainfall = col_number(), 
                                                        temp = col_number(), case = col_number(), 
                                                        SARSCoV2_SiteA = col_number(), PMMoV_SiteA = col_number(), 
                                                        SARSCoV2_SiteB = col_number(), PMMoV_SiteB = col_number(), 
                                                        SARSCoV2_SiteC = col_number(), PMMoV_SiteC = col_number()))
head(corr3site)

######all by site####
library(readr)
corr3site <- read_csv("corr3site.csv", col_types = cols(avg_rainfall = col_number(), 
                                                        temp = col_number(), case = col_number(), 
                                                        SARSCoV2_SiteA = col_number(), PMMoV_SiteA = col_number(), 
                                                        SARSCoV2_SiteC = col_number(), PMMoV_SiteC = col_number(), 
                                                        SARSCoV2_SiteB = col_number(), PMMoV_SiteB = col_number(), 
                                                        EC_SiteA = col_number(), ORP_SiteA = col_number(), 
                                                        DO_SiteA = col_number(), pH_SiteA = col_number(), 
                                                        EC_SiteB = col_number(), ORP_SiteB = col_number(), 
                                                        DO_SiteB = col_number(), pH_SiteB = col_number(), 
                                                        EC_SiteC = col_number(), ORP_SiteC = col_number(), 
                                                        DO_SiteC = col_number(), pH_SiteC = col_number()))
head(corr3site)

####### pH ORP pH DO avg#####
library(readr)
corr3site <- read_csv("corrtryhumid.csv", 
                      col_types = cols(avg_rainfall = col_number(), 
                                       temp = col_number(), case = col_number(), 
                                       SARSCoV2_SiteA = col_number(), PMMoV_SiteA = col_number(), 
                                       SARSCoV2_SiteC = col_number(), PMMoV_SiteC = col_number(), 
                                       SARSCoV2_SiteB = col_number(), PMMoV_SiteB = col_number(), 
                                       EC = col_number(), ORP = col_number(), 
                                       DO = col_number(), pH = col_number(), 
                                       humidity = col_number()))
head(corr3site)

attach(corr3site)
p.mat <- cor_pmat(corr3site) #correlation matrix with p-values
head(p.mat)
p.mat
m <- cor(corr3site) 
m

#sig
# new corr plot
library(ggcorrplot)
library(reshape2)
library(ggcorrplot)
library(ggplot2)
library(reshape2)
library(ggcorrplot)
library(reshape2)
library(tidyverse)
library(vegan)
library(grid)
library(gridExtra)
library(readr)
library(dplyr)
library(ggcorrplot)
library(ggplot2)
library(reshape2)


corr <- round(cor(corr3site), 2)
corr

p.df <- as.data.frame(ggcorrplot::cor_pmat(corr3site))

labs.function = function(x){
  case_when(x >= 0.05 ~ "",
            x < 0.05 & x >= 0.01 ~ "*",
            x < 0.01 & x >= 0.001 ~ "**",
            x < 0.001 ~ "***")
}

p.labs = p.df %>%
  mutate_all(labs.function)

p.labs$Var1 = as.factor(rownames(p.labs))
p.labs = melt(p.labs, id.vars = "Var1", variable.name = "Var2", value.name = "lab")

cor_plot = ggcorrplot(corr, hc.order = F, type = "lower",
                      lab = T, ggtheme = ggplot2::theme_gray, colors = c("#F4EEEE", "#FFE6E6", "#E1AFD1")) +
  theme(legend.text = element_text(color = "black", family = "Arial", size = 10), #detail site label
        legend.title = element_text(color = "black", family = "Arial", size = 10, face = "bold"), #site detail label
        axis.text = element_text(color = "black", family = "Arial", size = 2),
        panel.background = element_rect(fill = "grey95", colour = NA),
        panel.grid.major = element_line(colour = "white", size = 0.2))

p.labs$in.df = ifelse(is.na(match(paste0(p.labs$Var1, p.labs$Var2),
                                  paste0(cor_plot[["data"]]$Var1, cor_plot[["data"]]$Var2))),
                      "No", "Yes")

p.labs = select(filter(p.labs, in.df == "Yes"), -in.df)

cor.plot.labs = cor_plot +
  geom_text(aes(x = p.labs$Var1,
                y = p.labs$Var2),
            label = p.labs$lab,
            nudge_y = 0.25,
            size = 5)

cor.plot.labs

ggsave(file="cor.plot.labs.jpeg", cor.plot.labs, width= 250, height = 250, units = "mm", dpi=600)


# (removed)humidity and rainfall do not correlate with COVID-19 cases 
# (PMMoV, SARS-CoV concentration, and temperature are correlated significantly) 
# but humidity correlated SARS-CoV concentration and rainfall are correlated humidity

dev.off()

F3AB <- plot_grid(fig3xx, cor.plot.labs, ncol = 1, rel_heights = c(1, 1.5),
                  labels = c("a", "b"), label_size = 10)
F3AB

ggsave(file="F3AB.jpeg", F3AB, width= 190, height = 250, units = "mm", dpi=900)
dev.off()

###################################################### try 24 n omi####

library(readr)
corr_omi <- read_csv("corr_omi.csv", col_types = cols(sarcov = col_number(), 
                                                      case = col_number(), Omi_339m = col_number(), 
                                                      Omi_339w = col_number()))
View(corr_omi)

attach(corr_omi)

p.mat <- cor_pmat(corr_omi) #correlation matrix with p-values
head(p.mat)
p.mat
m <- cor(corr_omi) 
m

#sig
# new corr plot
library(dplyr)
library(ggcorrplot)
library(ggplot2)
library(reshape2)

corr <- round(cor(corr_omi), 2)

p.df <- as.data.frame(ggcorrplot::cor_pmat(corr_omi))

labs.function = function(x){
  case_when(x >= 0.05 ~ "",
            x < 0.05 & x >= 0.01 ~ "*",
            x < 0.01 & x >= 0.001 ~ "**",
            x < 0.001 ~ "***")
}

p.labs = p.df %>%
  mutate_all(labs.function)

p.labs$Var1 = as.factor(rownames(p.labs))
p.labs = melt(p.labs, id.vars = "Var1", variable.name = "Var2", value.name = "lab")

cor_plot = ggcorrplot(corr, hc.order = F, type = "lower",
                      lab = T, ggtheme = ggplot2::theme_gray, colors = c("#F4EEEE", "#FFE6E6", "#E1AFD1")) +
  theme(legend.text = element_text(color = "black", family = "Arial", size = 10), #detail site label
        legend.title = element_text(color = "black", family = "Arial", size = 10, face = "bold"), #site detail label
        axis.text = element_text(color = "black", family = "Arial", size = 2),
        panel.background = element_rect(fill = "grey95", colour = NA),
        panel.grid.major = element_line(colour = "white", size = 0.2))

p.labs$in.df = ifelse(is.na(match(paste0(p.labs$Var1, p.labs$Var2),
                                  paste0(cor_plot[["data"]]$Var1, cor_plot[["data"]]$Var2))),
                      "No", "Yes")

p.labs = select(filter(p.labs, in.df == "Yes"), -in.df)

cor.plot.labs2 = cor_plot +
  geom_text(aes(x = p.labs$Var1,
                y = p.labs$Var2),
            label = p.labs$lab,
            nudge_y = 0.25,
            size = 5)

cor.plot.labs2

ggsave(file="cor.plot.labsazz.jpeg", cor.plot.labs2,
       width= 90, height = 90, units = "mm", dpi=600)
dev.off()

#n24 not enough to sig with envi next try to incclude sarc and case 
#but no sar and case sig

###################################################### add in fig 3####

ggsave(file="cor.plot.labsazz.jpeg", cor.plot.labs,
       width= 90, height = 90, units = "mm", dpi=600)
dev.off()

F3AB <- plot_grid(fig3, cor.plot.labs, ncol = 1, 
                  labels = c("a", "b"), label_size = 10)
F3AB

ggsave(file="F3AB.jpeg", F3AB, width= 180, height = 200, units = "mm", dpi=600)
dev.off()

####fig 4 ad####
##### r1 ####
library(readxl)
data <- read_excel("regress_sarcase.xlsx", 
                              col_types = c("numeric", "numeric"))
View(data)

# Perform linear regression
model <- lm(new_cases ~ RNA_concentration, data = data)

# Calculate Pearson's correlation coefficient and p-value
cor_test <- cor.test(data$RNA_concentration, data$new_cases)

# Extract r and p-value
r_value <- cor_test$estimate
p_value <- cor_test$p.value
# Function to format p-value
format_p_value <- function(p) {
  if(p < 0.001) {
    return("p < 0.001***")
  } else if(p < 0.01) {
    return("p < 0.01**")
  } else if(p < 0.05) {
    return("p < 0.05*")
  } else {
    return(sprintf("p = %.3f", p))
  }
}

# Plotting
r1 <- ggplot(data, aes(x = RNA_concentration, y = new_cases)) +
  geom_point() +  # Add points
  geom_smooth(method = "lm", se = TRUE, linetype = "solid", color = "blue") +  # Linear regression line + CI
  labs(x = "SARS-CoV-2 RNA Concentrations in Hospital", y = "Weekly Reported COVID-19 new Cases")+ 
  theme(axis.title.x = element_text(color = "black", size = 7, face = "bold"), #ชื่อแกนx
        axis.title.y = element_text(color = "black", size = 7, face = "bold"), #ชื่อแกนy
        legend.text = element_text(color = "black", size = 7), #detail site label
        legend.title = element_text(color = "black", size = 7, face = "bold"), #site detail label
        legend.position = c(0.97, 0.97),
        legend.justification = c("right", "top"),
        legend.box.just = "right",
        legend.box.margin = margin(t = -5, r = -5, b = -5, l = -5),
        plot.margin = margin(0.5, 0.5, 0.5, 0.5, "mm"),
        axis.text.x = element_text(color = "black", size = 7),
        axis.text.y = element_text(color = "black", size = 7),
        strip.text.y = element_text(color = "black", size = 5, face = "bold"),
        panel.background = element_rect(fill = "white", colour = NA),
        panel.grid.major = element_line(colour = "white", size = 0.1),
        panel.border = element_rect(colour = "black", fill=NA, size=0.5)) +
  annotate("text", x = Inf, y = Inf, label = sprintf("Pearson's r = %.4f\n%s", r_value, format_p_value(p_value)), 
           hjust = 1.1, vjust = 1.1, size = 4, color = "black")
r1
save_plot("r1.jpeg", r1)


####r2#####
library(readxl)
data <- read_excel("regress2.xlsx", col_types = c("numeric", 
                                                      "numeric"))
head(data)

# Perform linear regression
model <- lm(new_cases ~ RNA_concentration, data = data)

# Calculate Pearson's correlation coefficient and p-value
cor_test <- cor.test(data$RNA_concentration, data$new_cases)

# Extract r and p-value
r_value <- cor_test$estimate
p_value <- cor_test$p.value
# Function to format p-value
format_p_value <- function(p) {
  if(p < 0.001) {
    return("p < 0.001***")
  } else if(p < 0.01) {
    return("p < 0.01**")
  } else if(p < 0.05) {
    return("p < 0.05*")
  } else {
    return(sprintf("p = %.3f", p))
  }
}

# Plotting
r2 <- ggplot(data, aes(x = RNA_concentration, y = new_cases)) +
  geom_point() +  # Add points
  geom_smooth(method = "lm", se = TRUE, linetype = "solid", color = "blue") +  # Linear regression line + CI
  labs(x = "SARS-CoV-2 RNA Concentrations in River", y = "Weekly Reported COVID-19 new Cases")+ 
  theme(axis.title.x = element_text(color = "black", size = 7, face = "bold"), #ชื่อแกนx
        axis.title.y = element_text(color = "black", size = 7, face = "bold"), #ชื่อแกนy
        legend.text = element_text(color = "black", size = 7), #detail site label
        legend.title = element_text(color = "black", size = 7, face = "bold"), #site detail label
        legend.position = c(0.97, 0.97),
        legend.justification = c("right", "top"),
        legend.box.just = "right",
        legend.box.margin = margin(t = -5, r = -5, b = -5, l = -5),
        plot.margin = margin(0.5, 0.5, 0.5, 0.5, "mm"),
        axis.text.x = element_text(color = "black", size = 7),
        axis.text.y = element_text(color = "black", size = 7),
        strip.text.y = element_text(color = "black", size = 5, face = "bold"),
        panel.background = element_rect(fill = "white", colour = NA),
        panel.grid.major = element_line(colour = "white", size = 0.1),
        panel.border = element_rect(colour = "black", fill=NA, size=0.5)) +
  annotate("text", x = Inf, y = Inf, label = sprintf("Pearson's r = %.4f\n%s", r_value, format_p_value(p_value)), 
           hjust = 1.1, vjust = 1.1, size = 4, color = "black")
r2
save_plot("r2.jpeg", r2)

######r3####
library(readxl)
data <- read_excel("regress3.xlsx", col_types = c("numeric", 
                                                      "numeric"))
head(data)

# Perform linear regression
model <- lm(new_cases ~ RNA_concentration, data = data)

# Calculate Pearson's correlation coefficient and p-value
cor_test <- cor.test(data$RNA_concentration, data$new_cases)

# Extract r and p-value
r_value <- cor_test$estimate
p_value <- cor_test$p.value
# Function to format p-value
format_p_value <- function(p) {
  if(p < 0.001) {
    return("p < 0.001***")
  } else if(p < 0.01) {
    return("p < 0.01**")
  } else if(p < 0.05) {
    return("p < 0.05*")
  } else {
    return(sprintf("p = %.3f", p))
  }
}

# Plotting
r3 <- ggplot(data, aes(x = RNA_concentration, y = new_cases)) +
  geom_point() +  # Add points
  geom_smooth(method = "lm", se = TRUE, linetype = "solid", color = "blue") +  # Linear regression line + CI
  labs(x = "SARS-CoV-2 RNA Concentrations in River", y = "Weekly Reported COVID-19 new Cases")+ 
  theme(axis.title.x = element_text(color = "black", size = 7, face = "bold"), #ชื่อแกนx
        axis.title.y = element_text(color = "black", size = 7, face = "bold"), #ชื่อแกนy
        legend.text = element_text(color = "black", size = 7), #detail site label
        legend.title = element_text(color = "black", size = 7, face = "bold"), #site detail label
        legend.position = c(0.97, 0.97),
        legend.justification = c("right", "top"),
        legend.box.just = "right",
        legend.box.margin = margin(t = -5, r = -5, b = -5, l = -5),
        plot.margin = margin(0.5, 0.5, 0.5, 0.5, "mm"),
        axis.text.x = element_text(color = "black", size = 7),
        axis.text.y = element_text(color = "black", size = 7),
        strip.text.y = element_text(color = "black", size = 5, face = "bold"),
        panel.background = element_rect(fill = "white", colour = NA),
        panel.grid.major = element_line(colour = "white", size = 0.1),
        panel.border = element_rect(colour = "black", fill=NA, size=0.5)) +
  annotate("text", x = Inf, y = Inf, label = sprintf("Pearson's r = %.4f\n%s", r_value, format_p_value(p_value)), 
           hjust = 1.1, vjust = 1.1, size = 4, color = "black")
r3
save_plot("r3.jpeg", r3)

#### r4 corr bar hos ####
####new ccf####
library(readr)
library(dplyr)
library(tidyr)
library(zoo) # For linear interpolation
library(ggplot2)
library(openxlsx)
library(forecast)
library(dplyr)
library(tibble) # For using tibble()

# Load the data
data <- read_csv("weekly_data.csv", col_types = cols(
  site = col_character(),
  week = col_date(format = "%m/%d/%Y"),
  RNA_concentration = col_number(),
  new_cases = col_number()
))
View(data)


# Interpolate the data (this is a simplification, adjust as needed for your data structure)
# This example does not interpolate because weekly data doesn't necessarily need it
# for this type of analysis. But if daily data points were missing, you'd interpolate here.
calculate_correlations <- function(site_data, lags=seq(-20, 20)) {
  results <- tibble(lag = integer(), correlation = numeric(), p_value = numeric())
  
  for (lag in lags) {
    if (lag < 0) {
      # Use lead() for negative lags
      adjusted_data <- site_data %>%
        mutate(RNA_concentration_adjusted = lead(RNA_concentration, abs(lag))) %>%
        na.omit()
    } else {
      # Use lag() for positive lags
      adjusted_data <- site_data %>%
        mutate(RNA_concentration_adjusted = lag(RNA_concentration, lag)) %>%
        na.omit()
    }
    
    # Calculate Pearson correlation
    cor_test <- cor.test(adjusted_data$RNA_concentration_adjusted, adjusted_data$new_cases, method = "pearson")
    
    # Append to results
    results <- bind_rows(results, tibble(lag = lag, correlation = cor_test$estimate, p_value = cor_test$p.value))
  }
  
  return(results)
}

# Apply the function to each site and collect results for the correct range of lags
all_sites_correlations <- data %>%
  group_by(site) %>%
  do(calculate_correlations(., lags=seq(-20, 20))) %>%
  ungroup()

# View the results
print(all_sites_correlations)
write.xlsx(all_sites_correlations, file = "all_sites_correlations.xlsx")
#################################################################################
# Define the maximum lag
lag.max <- 20

# Calculate CCF and store the result
ccf_result <- ccf(data$RNA_concentration, data$new_cases, lag.max = lag.max, plot = TRUE)

# Calculate the threshold for significance
threshold <- 2 / sqrt(nrow(data))

# Find indices where the absolute correlation is significant
significant_indices <- which(abs(ccf_result$acf) > threshold)

# Adjust indices to actual lag values considering the structure of ccf_result
# The middle of the acf array corresponds to lag 0; indices before this point are negative lags
actual_lags <- significant_indices - (lag.max + 1)

# Extract significant correlations using indices
significant_correlations <- ccf_result$acf[significant_indices]

# Name the correlations with their corresponding actual lags for clarity
names(significant_correlations) <- actual_lags

# Print significant lags and their correlations if any are found
if(length(significant_correlations) > 0) {
  print(significant_correlations)
} else {
  print("No significant lags found")
}

#################################################################################
# After you've calculated all_sites_correlations with the updated lag range

# Find the row(s) with the highest correlation across all sites
peak_correlation <- all_sites_correlations %>%
  ungroup() %>%
  arrange(desc(correlation)) %>%
  slice(1)
peak_correlation

# Plotting the results with the peak correlation highlighted
pastel_colors <- c("Hospital" = "#26547c", "River" = "#ef476f", "WWTP" = "#ffd166")

r4 <- ggplot(all_sites_correlations, aes(x = lag, y = correlation, color = site)) +
  geom_line() + 
  geom_point() +
  geom_point(data = peak_correlation, aes(x = lag, y = correlation), color = "red", size = 1) +  # Highlight peak
  geom_vline(xintercept = 4, linetype = "dashed") +
  scale_y_continuous(limits = c(min(all_sites_correlations$correlation), max(all_sites_correlations$correlation))) +  # Dynamically set y-axis limits
  scale_color_manual(values = pastel_colors) +
  labs(x = "Lag time (weeks)", y = "Pearson's Correlation Coefficient", title = "Time-lagged Cross-Correlation Across Sites") +
  theme_minimal() +
  theme(axis.title.x = element_text(color = "black", size = 12, face = "bold"),
        axis.title.y = element_text(color = "black", size = 12, face = "bold"),
        legend.text = element_text(color = "black", size = 10),
        legend.title = element_text(color = "black", size = 10, face = "bold"),
        plot.margin = margin(5, 5, 5, 5, "mm"),
        axis.text.x = element_text(color = "black", size = 10),
        axis.text.y = element_text(color = "black", size = 10),
        panel.background = element_rect(fill = "white", colour = NA),
        panel.grid.major = element_line(colour = "grey", size = 0.2, linetype = "dashed"),
        panel.border = element_rect(colour = "black", fill=NA, size=0.5))

r4
ggsave(file="r4.jpeg", r4, width= 180, height = 100, units = "mm", dpi=600)


#####combine fig4####
fig4AB <- plot_grid(r1, r4, ncol = 2, rel_widths = c(1, 2),
                    labels = c("a", "b"), label_size = 10)

ggsave(file="fig4AB.jpeg", fig4AB, width= 180, height = 100, units = "mm", dpi=600)

###Set graph figure time series and ccf####

#px1
graphH <- read_excel("graphH.xlsx", col_types = c("date", "numeric", "numeric"))
head(graphH)

fp1 <- graphH %>%
  mutate(Date = as.Date(Date)) %>%
  ggplot() + 
  geom_bar(aes(x = Date, y = case * 6 / 1845400, fill = "Weekly new case"), stat = "identity", color = "#349cf7") + 
  geom_line(aes(x = Date, y = value, color = "SARS-CoV-2 concentration")) + 
  geom_point(aes(x = Date, y = value, color = "SARS-CoV-2 concentration"), size = 1, shape = 21) + 
  geom_vline(xintercept = as.Date("2022-03-18"), linetype = "dashed", color = "black") + # Add dashed line
  annotate("text", x = as.Date("2022-12-01"), y = 6, label = "6th wave: 2022-03-18", vjust = -0.5, color = "black") + # Add label
  geom_vline(xintercept = as.Date("2022-02-16"), linetype = "dashed", color = "red") + # Add dashed line
  annotate("text", x = as.Date("2022-12-01"), y = 6.5, label = "Peak concentration: 2022-02-16", vjust = -0.5, color = "red") + # Add label
  geom_vline(xintercept = as.Date("2022-03-23"), linetype = "dashed", color = "blue") + # Add dashed line
  annotate("text", x = as.Date("2022-12-01"), y = 5, label = "Peak case: 2022-03-23", vjust = -0.5, color = "blue") + # Add label
  geom_hline(yintercept = 2, linetype = "dashed", color = "black") + 
  annotate("text", x = as.Date("2021-10-30"), y = 2, label = "LOQ", vjust = -0.5, hjust = 1, color = "black") + # Add label
  scale_x_date(
    name = "Month/Year", 
    date_breaks = "2 month",
    date_labels = "%m/%y",
    limits = as.Date(c("2021-10-01", "2023-10-31")) # Set limits for the x-axis
  ) +
  scale_y_continuous(
    name = expression("SARS-CoV-2 concentration " *  (Log[10] * " copies/L")), 
    sec.axis = sec_axis(~ . * 1845400 / 6, name = "Weekly new case"), limits = c(0, 7)) + 
  scale_fill_manual(values = c("Weekly new case" = "#349cf7")) +
  scale_color_manual(values = c("SARS-CoV-2 concentration" = "#f76fea")) +
  theme_bw() + 
  labs(
    title = "Site A",
    x = "Date",
    fill = "Legend",
    color = "Legend"
  ) + labs(fill = " ") +
  theme(
    axis.title.x = element_text(color = "black", size = 9, face = "bold"),
    axis.title.y = element_text(color = "black", size = 9, face = "bold"),
    legend.title = element_text(color = "black", size = 7, face = "bold"),
    legend.position = "top",
    plot.margin = margin(0.5, 0.5, 0.5, 0.5, "mm"),
    axis.text.x = element_text(color = "black", size = 9),
    axis.text.y = element_text(color = "black", size = 9),
    panel.background = element_rect(fill = "white", colour = NA),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_rect(color = "black", fill = NA, size = 0.5)
  )

fp1

ggsave(file="fp1.jpeg", fp1, width= 180, height = 100, units = "mm", dpi=600)

#px2

# Find the row(s) with the highest correlation across all sites
peak_correlation <- all_sites_correlations %>%
  ungroup() %>%
  arrange(desc(correlation)) %>%
  slice(1)
peak_correlation

# Plotting the results with the peak correlation highlighted
pastel_colors <- c("SiteA" = "#26547c", "SiteC" = "#ef476f", "SiteB" = "#ffd166", "All3site" = "grey")

head(all_sites_correlations)

r4 <- ggplot(all_sites_correlations, aes(x = lag, y = correlation, color = site, shape = site)) +
  geom_line() + 
  geom_point() +
  geom_point(data = peak_correlation, aes(x = lag, y = correlation), color = "red", size = 0.5) +  # Highlight peak
  geom_vline(xintercept = 4, linetype = "dashed") +
  scale_y_continuous(limits = c(min(all_sites_correlations$correlation), max(all_sites_correlations$correlation))) +  # Dynamically set y-axis limits
  scale_color_manual(values = pastel_colors) +
  scale_shape_manual(values = c(15,16,17,23), guide = "none") +
  labs(x = "Lag time (weeks)", y = "Pearson's Correlation Coefficient", title = "Time-lagged Cross-Correlation Across Sites") +
  theme_minimal() +
  theme(axis.title.x = element_text(color = "black", size = 10, face = "bold"),
        axis.title.y = element_text(color = "black", size = 10),
        legend.text = element_text(color = "black", size = 7),
        legend.title = element_text(color = "black", size = 7, face = "bold"),
        plot.margin = margin(5, 5, 5, 5, "mm"),
        axis.text.x = element_text(color = "black", size = 10),
        axis.text.y = element_text(color = "black", size = 10),
        panel.background = element_rect(fill = "white", colour = NA),
        panel.grid.major = element_line(colour = "grey", size = 0.2, linetype = "dashed"),
        panel.border = element_rect(colour = "black", fill=NA, size=0.5))

r4
ggsave(file="r4.jpeg", r4, width= 180, height = 100, units = "mm", dpi=600)

###Fig 2AC####

ccf_ts <- plot_grid(fp1, r4, ncol = 1,
                    labels = c("a", "b"), label_size = 10)

ggsave(file="ccf_ts.jpeg", ccf_ts, width= 160, height = 180, units = "mm", dpi=600)

##################ccombine 3 sites####
####new ccf####
library(readr)
library(dplyr)
library(tidyr)
library(zoo) # For linear interpolation
library(ggplot2)
library(openxlsx)
library(forecast)
library(dplyr)
library(tibble) # For using tibble()


# Load the data
data2 <- read_csv("weeklydataall.csv", col_types = cols(
  site = col_character(),
  week = col_date(format = "%m/%d/%Y"),
  RNA_concentration = col_number(),
  new_cases = col_number()
))
View(data2)

calculate_correlations <- function(site_data, lags=seq(-20, 20)) {
  results <- tibble(lag = integer(), correlation = numeric(), p_value = numeric())
  
  for (lag in lags) {
    if (lag < 0) {
      adjusted_data <- site_data %>%
        mutate(RNA_concentration_adjusted = lead(RNA_concentration, abs(lag))) %>%
        na.omit()
    } else {
      adjusted_data <- site_data %>%
        mutate(RNA_concentration_adjusted = lag(RNA_concentration, lag)) %>%
        na.omit()
    }
    
    cor_test <- cor.test(adjusted_data$RNA_concentration_adjusted, adjusted_data$new_cases, method = "pearson")
    
    results <- bind_rows(results, tibble(lag = lag, correlation = cor_test$estimate, p_value = cor_test$p.value))
  }
  
  return(results)
}

all_sites_correlations2 <- data2 %>%
  group_by(site) %>%
  do(calculate_correlations(., lags=seq(-20, 20))) %>%
  ungroup()

# View the results
print(all_sites_correlations2)
write.xlsx(all_sites_correlations2, file = "all_sites_correlations2.xlsx")
#################################################################################
# Define the maximum lag
lag.max <- 20

# Calculate CCF and store the result
ccf_result <- ccf(data2$RNA_concentration, data$new_cases, lag.max = lag.max, plot = TRUE)

# Calculate the threshold for significance
threshold <- 2 / sqrt(nrow(data2))

# Find indices where the absolute correlation is significant
significant_indices <- which(abs(ccf_result$acf) > threshold)

# Adjust indices to actual lag values considering the structure of ccf_result
# The middle of the acf array corresponds to lag 0; indices before this point are negative lags
actual_lags <- significant_indices - (lag.max + 1)

# Extract significant correlations using indices
significant_correlations2 <- ccf_result$acf[significant_indices]

# Name the correlations with their corresponding actual lags for clarity
names(significant_correlations2) <- actual_lags

# Print significant lags and their correlations if any are found
if(length(significant_correlations2) > 0) {
  print(significant_correlations2)
} else {
  print("No significant lags found")
}

#################################################################################
# After you've calculated all_sites_correlations with the updated lag range

# Find the row(s) with the highest correlation across all sites
peak_correlation <- all_sites_correlations2 %>%
  ungroup() %>%
  arrange(desc(correlation)) %>%
  slice(1)
peak_correlation

# Plotting the results with the peak correlation highlighted
pastel_colors <- c("Hospital" = "#26547c", "River" = "#ef476f", "WWTP" = "#ffd166")

r43 <- ggplot(all_sites_correlations2, aes(x = lag, y = correlation, color = site)) +
  geom_line() + 
  geom_point() +
  geom_point(data = peak_correlation, aes(x = lag, y = correlation), color = "red", size = 1) +  # Highlight peak
  geom_vline(xintercept = 4, linetype = "dashed") +
  scale_y_continuous(limits = c(min(all_sites_correlations2$correlation), max(all_sites_correlations2$correlation))) +  # Dynamically set y-axis limits
  scale_color_manual(values = pastel_colors) +
  labs(x = "Lag time (weeks)", y = "Pearson's Correlation Coefficient", title = "Time-lagged Cross-Correlation Across All Sites") +
  theme_minimal() +
  theme(axis.title.x = element_text(color = "black", size = 12, face = "bold"),
        axis.title.y = element_text(color = "black", size = 12, face = "bold"),
        legend.text = element_text(color = "black", size = 10),
        legend.title = element_text(color = "black", size = 10, face = "bold"),
        plot.margin = margin(5, 5, 5, 5, "mm"),
        axis.text.x = element_text(color = "black", size = 10),
        axis.text.y = element_text(color = "black", size = 10),
        panel.background = element_rect(fill = "white", colour = NA),
        panel.grid.major = element_line(colour = "grey", size = 0.2, linetype = "dashed"),
        panel.border = element_rect(colour = "black", fill=NA, size=0.5))

r43
ggsave(file="r43.jpeg", r43, width= 180, height = 100, units = "mm", dpi=600)

ccf_tsx <- plot_grid(fp1, r4, r43, ncol = 1,
                    labels = c("a", "b", "c"), label_size = 10)

ggsave(file="ccf_tsx.jpeg", ccf_tsx, width= 160, height = 220, units = "mm", dpi=600)


##### lag time for each season####

library(readr)
library(dplyr)
library(tidyr)
library(zoo) # For linear interpolation
library(ggplot2)
library(openxlsx)
library(forecast)
library(dplyr)
library(tibble) # For using tibble()

# Load the data

library(readr)
data2 <- read_csv("corrseason.csv", 
                       col_types = cols(week = col_date(format = "%m/%d/%Y"), 
                                        RNA_concentration = col_number(), 
                                        season = col_character(), new_cases = col_number()))
head(data2)

calculate_correlations <- function(site_data, lags=seq(-20, 20)) {
  results <- tibble(lag = integer(), correlation = numeric(), p_value = numeric())
  
  for (lag in lags) {
    if (lag < 0) {
      adjusted_data <- site_data %>%
        mutate(RNA_concentration_adjusted = lead(RNA_concentration, abs(lag))) %>%
        na.omit()
    } else {
      adjusted_data <- site_data %>%
        mutate(RNA_concentration_adjusted = lag(RNA_concentration, lag)) %>%
        na.omit()
    }
    
    cor_test <- cor.test(adjusted_data$RNA_concentration_adjusted, adjusted_data$new_cases, method = "pearson")
    
    results <- bind_rows(results, tibble(lag = lag, correlation = cor_test$estimate, p_value = cor_test$p.value))
  }
  
  return(results)
}

all_sites_correlationsx <- data2 %>%
  group_by(season) %>%
  do(calculate_correlations(., lags=seq(-20, 20))) %>%
  ungroup()

# View the results
print(all_sites_correlationsx)
write.xlsx(all_sites_correlationsx, file = "all_sites_correlationsx.xlsx")
#################################################################################
# Define the maximum lag
lag.max <- 20

# Calculate CCF and store the result
ccf_result <- ccf(data2$RNA_concentration, data$new_cases, lag.max = lag.max, plot = TRUE)

# Calculate the threshold for significance
threshold <- 2 / sqrt(nrow(data2))

# Find indices where the absolute correlation is significant
significant_indices <- which(abs(ccf_result$acf) > threshold)

# Adjust indices to actual lag values considering the structure of ccf_result
# The middle of the acf array corresponds to lag 0; indices before this point are negative lags
actual_lags <- significant_indices - (lag.max + 1)

# Extract significant correlations using indices
significant_correlations2 <- ccf_result$acf[significant_indices]

# Name the correlations with their corresponding actual lags for clarity
names(significant_correlations2) <- actual_lags

# Print significant lags and their correlations if any are found
if(length(significant_correlations2) > 0) {
  print(significant_correlations2)
} else {
  print("No significant lags found")
}

#################################################################################
# After you've calculated all_sites_correlations with the updated lag range

# Find the row(s) with the highest correlation across all sites
peak_correlation <- all_sites_correlationsx %>%
  ungroup() %>%
  arrange(desc(correlation)) %>%
  slice(1)
peak_correlation

# Plotting the results with the peak correlation highlighted
pastel_colors <- c("autumn" = "#26547c", "spring" = "#ef476f", "summer" = "#ffd166", "winter" = "grey")

rxx <- ggplot(all_sites_correlationsx, aes(x = lag, y = correlation, color = season, shape = season)) +
  geom_line() + 
  geom_point() +
  geom_point(data = peak_correlation, aes(x = lag, y = correlation), color = "red", size = 1) +  # Highlight peak
  geom_vline(xintercept = 0, linetype = "dashed") +
  scale_y_continuous(limits = c(min(all_sites_correlationsx$correlation), max(all_sites_correlationsx$correlation))) +  # Dynamically set y-axis limits
  scale_color_manual(values = pastel_colors) +
  scale_shape_manual(values = c(15,16,17,23), guide = "none") +
  labs(x = "Lag time (weeks)", y = "Pearson's Correlation Coefficient", title = "Time-lagged Cross-Correlation Across Seasonal Changes") +
  theme_minimal() +
  theme(axis.title.x = element_text(color = "black", size = 10, face = "bold"),
        axis.title.y = element_text(color = "black", size = 10),
        legend.text = element_text(color = "black", size = 7),
        legend.title = element_text(color = "black", size = 7, face = "bold"),
        plot.margin = margin(5, 5, 5, 5, "mm"),
        axis.text.x = element_text(color = "black", size = 10),
        axis.text.y = element_text(color = "black", size = 10),
        panel.background = element_rect(fill = "white", colour = NA),
        panel.grid.major = element_line(colour = "grey", size = 0.2, linetype = "dashed"),
        panel.border = element_rect(colour = "black", fill=NA, size=0.5))

rxx
ggsave(file="rxx.jpeg", rxx, width= 180, height = 100, units = "mm", dpi=600)



##### new with season change####
###Fig 2AC####

ccf_tsx <- plot_grid(fp1, r4, rxx, ncol = 1,
                    labels = c("a", "b", "c"), label_size = 10)

ggsave(file="ccf_tsx.jpeg", ccf_tsx, width= 160, height = 240, units = "mm", dpi=600)

#####corr try add humid####
library(readr)
corr3site <- read_csv("corrtryhumid.csv", 
                         col_types = cols(avg_rainfall = col_number(), 
                                          temp = col_number(), case = col_number(), 
                                          SARSCoV2_SiteA = col_number(), PMMoV_SiteA = col_number(), 
                                          SARSCoV2_SiteC = col_number(), PMMoV_SiteC = col_number(), 
                                          SARSCoV2_SiteB = col_number(), PMMoV_SiteB = col_number(), 
                                          EC = col_number(), ORP = col_number(), 
                                          DO = col_number(), pH = col_number(), 
                                          humidity = col_number()))
head(corr3site)

attach(corr3site)
p.mat <- cor_pmat(corr3site) #correlation matrix with p-values
head(p.mat)
p.mat
m <- cor(corr3site) 
m

#sig
# new corr plot
library(ggcorrplot)
library(reshape2)
library(ggcorrplot)
library(ggplot2)
library(reshape2)
library(ggcorrplot)
library(reshape2)
library(tidyverse)
library(vegan)
library(grid)
library(gridExtra)
library(readr)
library(dplyr)
library(ggcorrplot)
library(ggplot2)
library(reshape2)


corr <- round(cor(corr3site), 2)
corr

p.df <- as.data.frame(ggcorrplot::cor_pmat(corr3site))

labs.function = function(x){
  case_when(x >= 0.05 ~ "",
            x < 0.05 & x >= 0.01 ~ "*",
            x < 0.01 & x >= 0.001 ~ "**",
            x < 0.001 ~ "***")
}

p.labs = p.df %>%
  mutate_all(labs.function)

p.labs$Var1 = as.factor(rownames(p.labs))
p.labs = melt(p.labs, id.vars = "Var1", variable.name = "Var2", value.name = "lab")

cor_plot = ggcorrplot(corr, hc.order = F, type = "lower",
                      lab = T, ggtheme = ggplot2::theme_gray, colors = c("#F4EEEE", "#FFE6E6", "#E1AFD1")) +
  theme(legend.text = element_text(color = "black", family = "Arial", size = 10), #detail site label
        legend.title = element_text(color = "black", family = "Arial", size = 10, face = "bold"), #site detail label
        axis.text = element_text(color = "black", family = "Arial", size = 2),
        panel.background = element_rect(fill = "grey95", colour = NA),
        panel.grid.major = element_line(colour = "white", size = 0.2))

p.labs$in.df = ifelse(is.na(match(paste0(p.labs$Var1, p.labs$Var2),
                                  paste0(cor_plot[["data"]]$Var1, cor_plot[["data"]]$Var2))),
                      "No", "Yes")

p.labs = select(filter(p.labs, in.df == "Yes"), -in.df)

cor.plot.labs = cor_plot +
  geom_text(aes(x = p.labs$Var1,
                y = p.labs$Var2),
            label = p.labs$lab,
            nudge_y = 0.25,
            size = 5)

cor.plot.labs

cor.plot.labs <- cor.plot.labs + 
  theme(plot.margin = margin(b = 0.5, unit = "cm"))

# Combine the plots with adjusted relative heights
F3AB <- plot_grid(fig3xx, cor.plot.labs, ncol = 1, rel_heights = c(1, 1.7),
                  labels = c("a", "b"), label_size = 11)

F3AB

dev.off()

F3AB <- plot_grid(fig3xx, cor.plot.labs, ncol = 1, rel_heights = c(1, 1.5),
                  labels = c("a", "b"), label_size = 11)
F3AB

ggsave(file="F3AB.jpeg", F3AB, width= 190, height = 250, units = "mm", dpi=700)
dev.off()
