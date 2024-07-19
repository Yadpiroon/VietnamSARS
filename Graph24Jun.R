# Vietnam Sample 24 Jun 2024

# library
getwd()
library(ggplot2)
library(rstatix)
library(ggpubr)
library(dplyr)
library(survival)
library(NADA)
library(NADA2)
library(EnvStats)
library(stats)
library(base)

#####SAR-CoV and PMMoV####
######### Descriptive statistics size # site sum #############

library(readr)
sardata_all <- read_csv("sardata_all.csv", 
                        col_types = cols(Rainfall = col_number(), 
                                         temp = col_number(), case = col_number()))
head(sardata_all)

library(readr)
sardata_all <- read_csv("sardata_all.csv", 
                        col_types = cols(Rainfall = col_number(), 
                                         temp = col_number(), sarcov = col_number(), 
                                         sarcov_cen = col_logical(), case = col_number(), 
                                         PMMoV = col_number(), PMMoV_cen = col_logical(), 
                                         EC = col_number(), ORP = col_number(), 
                                         DO = col_number(), pH = col_number(), 
                                         temperature = col_number()))
head(sardata_all)

########Site cen test#####################
#sar
sardata_all$sarcov <- as.numeric(sardata_all$sarcov)
sardata_all$sarcov_cen <- as.logical(sardata_all$sarcov_cen)
sardata_all$site <- as.factor(sardata_all$site)
obs <- sardata_all$sarcov
censored <- sardata_all$sarcov_cen
cenfit_model <- cenfit(obs, censored, sardata_all$site, data = sardata_all)
print(cenfit_model)

#PMMoV
sardata_all$PMMoV <- as.numeric(sardata_all$PMMoV)
sardata_all$PMMoV_cen <- as.logical(sardata_all$PMMoV_cen)
sardata_all$site <- as.factor(sardata_all$site)
obs <- sardata_all$PMMoV
censored <- sardata_all$PMMoV_cen
cenfit_model <- cenfit(obs, censored, sardata_all$site, data = sardata_all)
print(cenfit_model)

###########Site Normality test ###########################

Hospital <- sardata_all[1:105, ]
WWTP <- sardata_all[106:210, ]
River <- sardata_all[211:315, ]

#sarcov
summary(Hospital$sarcov)
sd(Hospital$sarcov)
shapiro.test(Hospital$sarcov)

summary(WWTP$sarcov)
sd(WWTP$sarcov)
shapiro.test(WWTP$sarcov)

summary(River$sarcov)
sd(River$sarcov)
shapiro.test(River$sarcov)

#PMMoV

summary(Hospital$PMMoV)
sd(Hospital$PMMoV)
shapiro.test(Hospital$PMMoV)

summary(WWTP$PMMoV)
sd(WWTP$PMMoV)
shapiro.test(WWTP$PMMoV)

summary(River$PMMoV)
sd(River$PMMoV)
shapiro.test(River$PMMoV)

#rainfall
summary(Hospital$Rainfall)
sd(Hospital$Rainfall)
shapiro.test(Hospital$Rainfall)

#temp
summary(Hospital$temp)
sd(Hospital$temp)
shapiro.test(Hospital$temp)

#case
summary(Hospital$case)
sd(Hospital$case)
shapiro.test(Hospital$case)

# Site different p-value > 0.05
one.way.anova_event <- aov(sardata_all$sarcov ~ sardata_all$site, data = sardata_all)
summary(one.way.anova_event)
TukeyHSD(one.way.anova_event)

one.way.anova_event <- aov(sardata_all$PMMoV ~ sardata_all$site, data = sardata_all)
summary(one.way.anova_event)
TukeyHSD(one.way.anova_event)

######### Descriptive statistics size # season sum #############


library(readr)
seasonVT <- read_csv("seasonVT.csv", col_types = cols(Rainfall = col_number(), 
                                                      temp = col_number(), case = col_number()))
View(seasonVT)


########season cen test#####################
#sar
seasonVT$sarcov <- as.numeric(seasonVT$sarcov)
seasonVT$sarcov_cen <- as.logical(seasonVT$sarcov_cen)
seasonVT$Season <- as.factor(seasonVT$Season)
obs <- seasonVT$sarcov
censored <- seasonVT$sarcov_cen
cenfit_model <- cenfit(obs, censored, seasonVT$Season, data = seasonVT)
print(cenfit_model)


#PMMoV
seasonVT$PMMoV <- as.numeric(seasonVT$PMMoV)
seasonVT$PMMoV_cen <- as.logical(seasonVT$PMMoV_cen)
seasonVT$Season <- as.factor(seasonVT$Season)
obs <- seasonVT$PMMoV
censored <- seasonVT$PMMoV_cen
cenfit_model <- cenfit(obs, censored, seasonVT$Season, data = seasonVT)
print(cenfit_model)

###########Season Normality test ###########################
Dry <- seasonVT[1:126, ]
Wet <- seasonVT[127:315, ]

#sarcov
summary(Dry$sarcov)
sd(Dry$sarcov)
shapiro.test(Dry$sarcov)

summary(Wet$sarcov)
sd(Wet$sarcov)
shapiro.test(Wet$sarcov)

#PMMoV

summary(Dry$PMMoV)
sd(Dry$PMMoV)
shapiro.test(Dry$PMMoV)

summary(Wet$PMMoV)
sd(Wet$PMMoV)
shapiro.test(Wet$PMMoV)

#rainfall

summary(Dry$Rainfall)
sd(Dry$Rainfall)
shapiro.test(Dry$Rainfall)

summary(Wet$Rainfall)
sd(Wet$Rainfall)
shapiro.test(Wet$Rainfall)

#temp

summary(Dry$temp)
sd(Dry$temp)
shapiro.test(Dry$temp)

summary(Wet$temp)
sd(Wet$temp)
shapiro.test(Wet$temp)

#case

summary(Dry$case)
sd(Dry$case)
shapiro.test(Dry$case)

summary(Wet$case)
sd(Wet$case)
shapiro.test(Wet$case)


# Site different p-value > 0.05
one.way.anova_event <- aov(seasonVT$sarcov ~ seasonVT$Season, data = seasonVT)
summary(one.way.anova_event)
TukeyHSD(one.way.anova_event)

one.way.anova_event <- aov(seasonVT$PMMoV ~ seasonVT$Season, data = seasonVT)
summary(one.way.anova_event)
TukeyHSD(one.way.anova_event)

#3.1.2 Two Sample t-test (unpaired)
t.test(Dry$sarcov, Wet$sarcov)
#one tail test
t.test(Dry$sarcov, Wet$sarcov, paired = F, alternative="greater")
t.test(Dry$sarcov, Wet$sarcov, paired = F, alternative="less")

#3.1.2 Two Sample t-test (unpaired)
t.test(Dry$PMMoV, Wet$PMMoV)
#one tail test
t.test(Dry$PMMoV, Wet$PMMoV, paired = F, alternative="greater")
t.test(Dry$PMMoV, Wet$PMMoV, paired = F, alternative="less")


#3.1.2 Two Sample t-test (unpaired)
t.test(Dry$Rainfall, Wet$Rainfall)
#one tail test
t.test(Dry$Rainfall, Wet$Rainfall, paired = F, alternative="greater")
t.test(Dry$Rainfall, Wet$Rainfall, paired = F, alternative="less")

#3.1.2 Two Sample t-test (unpaired)
t.test(Dry$temp, Wet$temp)
#one tail test
t.test(Dry$temp, Wet$temp, paired = F, alternative="greater")
t.test(Dry$temp, Wet$temp, paired = F, alternative="less")

#3.1.2 Two Sample t-test (unpaired)
t.test(Dry$case, Wet$case)
#one tail test
t.test(Dry$case, Wet$case, paired = F, alternative="greater")
t.test(Dry$case, Wet$case, paired = F, alternative="less")

#3.1.2 Two Sample t-test (unpaired)
t.test(Dry$sarcov, Dry$PMMoV)
#one tail test
t.test(Dry$sarcov, Dry$PMMoV, paired = F, alternative="greater")
t.test(Dry$sarcov, Dry$PMMoV, paired = F, alternative="less")

#3.1.2 Two Sample t-test (unpaired)
t.test(Wet$sarcov, Wet$PMMoV)
#one tail test
t.test(Wet$sarcov, Wet$PMMoV, paired = F, alternative="greater")
t.test(Wet$sarcov, Wet$PMMoV, paired = F, alternative="less")

##########################################case 2 year####

graphH <- read_excel("graphH.xlsx", col_types = c("date", "numeric", "numeric"))
View(graphH)

library(readxl)
yearVT <- read_excel("yearVT.xlsx", col_types = c("numeric", 
                                                  "numeric", "text", "numeric", "text", 
                                                  "numeric", "numeric", "text"))
View(yearVT)

Hospital <- yearVT[1:105, ]
View(Hospital)
hstyrs <- Hospital[1:52, ]
hndyrs <- Hospital[53:105, ]

WWTP <- yearVT[106:210, ]
wstyrs <- WWTP[1:52, ]
wndyrs <- WWTP[53:105, ]

River <- yearVT[211:315, ]
rstyrs <- River[1:52, ]
rndyrs <- River[53:105, ]

#h1
summary(hstyrs$sarcov)
sd(hstyrs$sarcov)
shapiro.test(hstyrs$sarcov)

#h2
summary(hndyrs$sarcov)
sd(hndyrs$sarcov)
shapiro.test(hndyrs$sarcov)

#w1
summary(wstyrs$sarcov)
sd(wstyrs$sarcov)
shapiro.test(wstyrs$sarcov)

#w2
summary(wstyrs$sarcov)
sd(wndyrs$sarcov)
shapiro.test(wndyrs$sarcov)

#r1
summary(rstyrs$sarcov)
sd(rstyrs$sarcov)
shapiro.test(rstyrs$sarcov)

#r2
summary(rndyrs$sarcov)
sd(rndyrs$sarcov)
shapiro.test(rndyrs$sarcov)

#st =3.981
summary(styrs$value)
sd(styrs$value)
shapiro.test(styrs$value)

#nd = 3.25
summary(ndyrs$value)
sd(ndyrs$value)
shapiro.test(ndyrs$value)

#case
t.test(styrs$case, ndyrs$case)
#one tail test
t.test(styrs$case, ndyrs$case, paired = F, alternative="greater")
t.test(styrs$case, ndyrs$case, paired = F, alternative="less")

#h
t.test(hstyrs$sarcov, hstyrs$sarcov)
#one tail test
t.test(hstyrs$sarcov, hstyrs$sarcov, paired = F, alternative="greater")
t.test(hstyrs$sarcov, hstyrs$sarcov, paired = F, alternative="less")

#w
t.test(wstyrs$sarcov, wstyrs$sarcov)
#one tail test
t.test(wstyrs$sarcov, wstyrs$sarcov, paired = F, alternative="greater")
t.test(wstyrs$sarcov, wstyrs$sarcov, paired = F, alternative="less")

#r
t.test(rstyrs$sarcov, rndyrs$sarcov)
#one tail test
t.test(rstyrs$sarcov, rndyrs$sarcov, paired = F, alternative="greater")
t.test(rstyrs$sarcov, rndyrs$sarcov, paired = F, alternative="less")


###########################################################

#####Omicron 339 484####

######### Descriptive statistics size # season sum #############

library(readr)
seasonOmi <- read_csv("seasonOmi.csv", col_types = cols(sarcov = col_number(), 
                                                        PMMoV = col_number(), Omi_339m = col_number(), 
                                                        Omi_339w = col_number(), Omi_484m = col_number(), 
                                                        Omi_484w = col_number()))
View(seasonOmi)

########season cen test#####################
#339m
seasonOmi$Omi_339m <- as.numeric(seasonOmi$Omi_339m)
seasonOmi$sarcov_cen <- as.logical(seasonOmi$sarcov_cen)
seasonOmi$Season <- as.factor(seasonOmi$Season)
obs <- seasonOmi$Omi_339m
censored <- seasonOmi$sarcov_cen
cenfit_model <- cenfit(obs, censored, seasonOmi$Season, data = seasonOmi)
print(cenfit_model)

#484m
seasonOmi$Omi_484m <- as.numeric(seasonOmi$Omi_484m)
seasonOmi$sarcov_cen <- as.logical(seasonOmi$sarcov_cen)
seasonOmi$Season <- as.factor(seasonOmi$Season)
obs <- seasonOmi$Omi_484m
censored <- seasonOmi$sarcov_cen
cenfit_model <- cenfit(obs, censored, seasonOmi$Season, data = seasonOmi)
print(cenfit_model)

#339w
seasonOmi$Omi_339w <- as.numeric(seasonOmi$Omi_339w)
seasonOmi$PMMoV_cen <- as.logical(seasonOmi$PMMoV_cen)
seasonOmi$Season <- as.factor(seasonOmi$Season)
obs <- seasonOmi$Omi_339w
censored <- seasonOmi$PMMoV_cen
cenfit_model <- cenfit(obs, censored, seasonOmi$Season, data = seasonOmi)
print(cenfit_model)

#484w
seasonOmi$Omi_484w <- as.numeric(seasonOmi$Omi_484w)
seasonOmi$PMMoV_cen <- as.logical(seasonOmi$PMMoV_cen)
seasonOmi$Season <- as.factor(seasonOmi$Season)
obs <- seasonOmi$Omi_484w
censored <- seasonOmi$PMMoV_cen
cenfit_model <- cenfit(obs, censored, seasonOmi$Season, data = seasonOmi)
print(cenfit_model)

###########Season Normality test ###########################
Dry <- seasonOmi[1:10, ]
Wet <- seasonOmi[11:24, ]

#Omi_339m
summary(Dry$Omi_339m)
sd(Dry$Omi_339m)
shapiro.test(Dry$Omi_339m)

summary(Wet$Omi_339m)
sd(Wet$Omi_339m)
shapiro.test(Wet$Omi_339m)

#Omi_484m

summary(Dry$Omi_484m)
sd(Dry$Omi_484m)
shapiro.test(Dry$Omi_484m)

summary(Wet$Omi_484m)
sd(Wet$Omi_484m)
shapiro.test(Wet$Omi_484m)

#Omi_339w
summary(Dry$Omi_339w)
sd(Dry$Omi_339w)
shapiro.test(Dry$Omi_339w)

summary(Wet$Omi_339w)
sd(Wet$Omi_339w)
shapiro.test(Wet$Omi_339w)

#Omi_484w

summary(Dry$Omi_484w)
sd(Dry$Omi_484w)
shapiro.test(Dry$Omi_484w)

summary(Wet$Omi_484w)
sd(Wet$Omi_484w)
shapiro.test(Wet$Omi_484w)


#Omi_339m
t.test(Dry$Omi_339m, Wet$Omi_339m)
#one tail test
t.test(Dry$Omi_339m, Wet$Omi_339m, paired = F, alternative="greater")
t.test(Dry$Omi_339m, Wet$Omi_339m, paired = F, alternative="less")

#Omi_484m
t.test(Dry$Omi_484m, Wet$Omi_484m)
#one tail test
t.test(Dry$Omi_484m, Wet$Omi_484m, paired = F, alternative="greater")
t.test(Dry$Omi_484m, Wet$Omi_484m, paired = F, alternative="less")

#Omi_339w
wilcox.test(Dry$Omi_339w, Wet$Omi_339w)                                        
wilcox.test(Dry$Omi_339w, Wet$Omi_339w, alternative="greater")
wilcox.test(Dry$Omi_339w, Wet$Omi_339w, alternative="less") 

#Omi_484w
wilcox.test(Dry$Omi_484w, Wet$Omi_484w)                                        
wilcox.test(Dry$Omi_484w, Wet$Omi_484w, alternative="greater")
wilcox.test(Dry$Omi_484w, Wet$Omi_484w, alternative="less")


#overall

summary(seasonOmi$Omi_484m)
sd(seasonOmi$Omi_484m)
shapiro.test(seasonOmi$Omi_484m)

#Omi_339w
summary(seasonOmi$Omi_339w)
sd(seasonOmi$Omi_339w)
shapiro.test(seasonOmi$Omi_339w)



