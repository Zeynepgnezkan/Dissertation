# Zeynep Gunes Ozkan 2022, Dissertation Analysis
# Bournemouth University

# This script runs with cleaned, preprocessed and ready-to-analyze data.
# If you haven't preprocessed dataMix.Rda, you can do it from Preproc.R script.
# This script includes assumption checks, all analysis, descriptive information and data visualization.

rm(list= ls())

# Import data

load("dataMix.Rda")

#######################################################
#                  dataMix CodeBook                   #
#######################################################
# Progress:      
# Age:           Age of the participants, numeric variable
# Gender:        Gender of the participants, (1 male, 2 female, 3 non-binary, 4 other, 5 prefer not to say (PNS))
# Ethnicity:     Ethnic group of participants, (1 white, 2 mixed or multiple ethnic group, 3 asian or asian british, 4 black caribbean or african, 5 other, 6 PNS)
# Education:     Highest qualification of participants, (1 no degree, 2 less than a high school diploma, 3 high school diploma, 4 bachelor’s degree, 5 master's degree, 6 PhD and higher, 7 PNS)
# Marital:       Relationship status of participants, (1 married or civil partnership, 2 single, 3 divorced or legally dissolved civil partnerchip, 4 widowed, 5 other, 6 PNS)
# Employment:    Employment status of participants, (1 full-time employment, 2 part-time employment, 3 unemployment, 4 self-employment, 5 home-maker, 6 student, 7 retired, 8 other, 9 PNS)
# FreqUse:       Average duration of internet usage per week, (1 less then 1 hour, 2 1-5 hours, 3 5-10 hours, 4 10-20 hours, 5 21-40 hours/week, 6 over 40 hours/week)
# Nationality:   1 = Turkey, 2 = British (0 = Turkey, 1 = British with dummy coded version)
# Purpose:       Purpose of internet usage of participants, (1 education and study-related purposes, 2 work-related purpose, 3 job-hunting, 4 communication, 5 shopping, 6 games, 7 filling up shape time, 8 finding information/news, 9 social media, 10 other)
# Purpose_G:     Groupized version of Purpose variable (detailed description is in Preproc.R script)
# OCI_score:     Total score of othe Online Cognition Scale
# OCI_socsup:    Social Comfort sub-dimension of the Online Cognition Scale
# OCI_att:       The Distraction sub-dimension of the Online Cognition Scale
# OCI_dep:       Loneliness/Depression sub-dimension of the Online Cognition Scale
# OCI_imp:       Diminished Impulse Control sub-dimension of the Online Cognition Scale
# Ucla_score:    UCLA Loneliness scale total score

# Required packages:
packages= c("tidyverse", "ggpubr", "ez", "lme4", "readr","stringr","performance","lattice",
            "lsr","Matrix","MASS","ggstatsplot","effects","ggeffects","rstatix","car",
            "gmodels","DescTools","qqplotr","rcompanion","grDevices") 

for(i in 1:length(packages)){
  
  if(packages[i] %in% rownames(installed.packages())==FALSE){
    install.packages(packages[i])
    library(packages[i], character.only=TRUE)
  }else{
    library(packages[i], character.only=TRUE)
  }
}


# Color Palette

paletteM= c("#7B3014","#CB5414", "#FFB68E","#A8C3DC","#3472A0","#26456E")

### Contrast

dataMix$Nationality <- as.factor(ifelse(dataMix$Nationality == 1,0,1))
levels(dataMix$Nationality)
contrasts(dataMix$Nationality)

dataMix$FreqUse <- as.factor(dataMix$FreqUse)
levels(dataMix$FreqUse)
contrasts(dataMix$FreqUse)

### Centring Ucla and OCS

dataMix$Ucla_score_c <- scale(dataMix$Ucla_score, scale = F)
dataMix$OCI_score_c <- scale(dataMix$OCI_score, scale = F)

### Subset by Nationality

data_uk <- subset(dataMix, Nationality == "1")
data_tr <- subset(dataMix, Nationality == "0")

#### Data Visualization ####

# Online Cognition Inventory Scores (OCI score)

OCIuk <- hist(data_uk$OCI_score)
OCItr <- hist(data_tr$OCI_score)
plot( OCIuk, col=rgb(1/8,1/3,1/2,1/4), xlim=c(0,300))
plot( OCItr, col=rgb(1,1/2,0,1/2/4), xlim=c(0,300), add=T)
legend("topright", 
       pch = 16,
       c("United Kingdom", "Turkey"), 
       col = c(rgb(1/8,1/3,1/2,1/4),rgb(1,1/2,0,1/2/4)),
       cex=1)

# Loneliness score (Ucla score) 

UCLAuk <- hist(data_uk$Ucla_score,)
UCLAtr <- hist(data_tr$Ucla_score)
plot( UCLAuk, col=rgb(1/8,1/3,1/2,1/4), xlim=c(10,80))
plot( UCLAtr, col=rgb(1,1/2,0,1/2/4), xlim=c(10,80), add=T)
legend("topleft", 
       pch = 16,
       c("United Kingdom", "Turkey"), 
       col = c(rgb(1/8,1/3,1/2,1/4),rgb(1,1/2,0,1/2/4)),
       cex=1)

# Violin plots

ggplot(dataMix, aes(Nationality,OCI_score))+
  geom_violin()+ geom_dotplot(binaxis = 'y', stackdir = 'center', dotsize = .2)

ggplot(dataMix, aes(Nationality,Ucla_score))+
  geom_violin()+ geom_dotplot(binaxis = 'y', stackdir = 'center', dotsize = .2)

# Scatter plot
windowsFonts(A = windowsFont("Times New Roman"))

plot(dataMix$Ucla_score_c, dataMix$OCI_score_c, 
     main = "Distribution of online cognition and loneliness scores",
     xlab = "Loneliness Score",
     ylab = "Online Cognition Score",
     col = (alpha(ifelse(dataMix$Nationality == "0",paletteM[2],paletteM[5]), 0.56)),
     pch = 16,
     family = "A")
legend("topleft", 
       pch = 16,
       c("Turkey", "United Kingdom"), 
       col = c(paletteM[2],paletteM[5]),
       text=element_text(family="Times New Roman", size=12))
abline(lm(data_uk$OCI_score_c ~ data_uk$Ucla_score_c), col = paletteM[5])
abline(lm(data_tr$OCI_score_c ~ data_tr$Ucla_score_c), col = paletteM[2])

## Black-White

plot(dataMix$Ucla_score_c, dataMix$OCI_score_c, 
     main = "Distribution of online cognition and loneliness scores",
     xlab = "Loneliness Score",
     ylab = "Online Cognition Score",
     col = (alpha(ifelse(dataMix$Nationality == "0","#888888","#222222"),0.75)),
     pch = 16)
     #family = "A")
legend("topleft", 
       pch = 16,
       c("Turkey", "United Kingdom"), 
       col = c("#888888","#222222"))
abline(lm(data_uk$OCI_score_c ~ data_uk$Ucla_score_c), col = "#222222")
abline(lm(data_tr$OCI_score_c ~ data_tr$Ucla_score_c), col = "#888888")


# With ggplot/Black-White

ggplot(dataMix)+geom_jitter(aes(x= Ucla_score_c, y=OCI_score_c, color= Nationality),alpha=0.7,size=2)+
  scale_color_manual(labels=c('Turkey', 'United Kingdom'),values=c("#888888","#222222"))+
  geom_smooth(data= data_uk, aes(Ucla_score_c,OCI_score_c,col=Nationality),method="lm",se=T,col="#222222")+
  geom_smooth(data = data_tr,aes(Ucla_score_c,OCI_score_c,col=Nationality),method="lm",se=T,col="#888888")+
  theme_classic()+
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_rect(colour = "black", size=1),
        text=element_text(family="Times New Roman", size=12),
        legend.position="top")+
  xlab("Loneliness Scores")+
  ylab("Online Cognition Scores")

# With ggplot/Colour

ggplot(dataMix)+geom_jitter(aes(x= Ucla_score_c, y=OCI_score_c, color= Nationality),alpha=0.55,size=2)+
  scale_color_manual(labels=c('Turkey', 'United Kingdom'),values=c(paletteM[2],paletteM[5]))+
  geom_smooth(data= data_uk, aes(Ucla_score_c,OCI_score_c,col=Nationality),method="lm",se=T,col=paletteM[5])+
  geom_smooth(data = data_tr,aes(Ucla_score_c,OCI_score_c,col=Nationality),method="lm",se=T,col=paletteM[2])+
  theme_classic()+
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_rect(colour = "black", size=1),
        text=element_text(family="Times New Roman", size=12),
        legend.position="top")+
  xlab("Loneliness Scores")+
  ylab("Online Cognition Scores")

# Distributions

#dataMix %>%
#  mutate(Nationality = fct_reorder(Nationality, desc(Ucla_score))) %>% 
  ggplot(dataMix, aes(x= Nationality, Ucla_score, fill=Nationality)) +
  ggdist::stat_halfeye(adjust = .3, width = .4, .width = 0, justification = -.3) + 
  geom_boxplot(width = .1, outlier.shape = NA) +
  ggdist::stat_dots(side = "left", dotsize = .26, justification = 1.1, binwidth = 1,col=paletteM[5])+
  theme_classic()+
  scale_fill_manual(labels=c('Turkey', 'United Kingdom'),values=paletteM[3:4])+
  coord_flip()+
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_rect(colour = "black", size=1),
        text=element_text(family="Times New Roman", size=12),
        legend.position="top")+
  ylab("Loneliness Scores")
  
## Black-White 
  ggplot(dataMix, aes(x= Nationality, Ucla_score, fill=Nationality)) +
    ggdist::stat_halfeye(adjust = .3, width = .4, .width = 0, justification = -.3) + 
    geom_boxplot(width = .1, outlier.shape = NA) +
    ggdist::stat_dots(side = "left", dotsize = .26, justification = 1.1, binwidth = 1)+
    theme_classic()+
    scale_fill_manual(labels=c('Turkey', 'United Kingdom'),values=c("#888888","#555555"))+
    coord_flip()+
    theme(panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank(),
          panel.background = element_rect(colour = "black", size=1),
          text=element_text(family="Times New Roman", size=12),
          legend.position="top")+
    ylab("Loneliness Scores")

#dataMix %>%
#  mutate(Nationality = fct_reorder(Nationality, desc(OCI_score))) %>% 
  ggplot(dataMix,aes(x= Nationality, OCI_score, fill=Nationality)) +
  ggdist::stat_halfeye(adjust = .2, width = .4, .width = 0, justification = -.3) + 
  geom_boxplot(width = .1, outlier.shape = NA) +
  ggdist::stat_dots(side = "left", dotsize = 0.4, justification = 1.1, binwidth = 3,col=paletteM[5])+
  theme_classic() +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_rect(colour = "black", size=1),
        text=element_text(family="Times New Roman", size=12),
        legend.position="top")+
  scale_fill_manual(labels=c('Turkey', 'United Kingdom'),values=paletteM[3:4])+
  coord_flip()+
  ylab("Online Cognition Scores")

## Black-White
  ggplot(dataMix,aes(x= Nationality, OCI_score, fill=Nationality)) +
    ggdist::stat_halfeye(adjust = .2, width = .4, .width = 0, justification = -.3) + 
    geom_boxplot(width = .1, outlier.shape = NA) +
    ggdist::stat_dots(side = "left", dotsize = 0.4, justification = 1.1, binwidth = 3)+
    theme_classic() +
    theme(panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank(),
          panel.background = element_rect(colour = "black", size=1),
          text=element_text(family="Times New Roman", size=12),
          legend.position="top")+
    scale_fill_manual(labels=c('Turkey', 'United Kingdom'),values=c("#888888","#555555"))+
    coord_flip()+
    ylab("Online Cognition Scores")

#### Demographic ####

 mean(as.numeric(dataMix$FreqUse))
 mean(dataMix$Age)
 max(dataMix$Age)
 min(dataMix$Age)
 sd(dataMix$Age)
 mean(data_uk$Age)
 max(data_uk$Age)
 min(data_uk$Age)
 sd(data_uk$Age)
 mean(data_tr$Age)
 sd(data_tr$Age)
 max(data_tr$Age)
 min(data_tr$Age)

 dataMix %>% count(Gender)
 data_uk %>% count(Gender)
 data_tr %>% count(Gender)

 dataMix %>% count(Ethnicity)
 data_uk %>% count(Ethnicity)
 data_tr %>% count(Ethnicity)

 dataMix %>% count(Education)
 data_uk %>% count(Education)
 data_tr %>% count(Education)

 dataMix %>% count(Marital)
 data_uk %>% count(Marital)
 data_tr %>% count(Marital)

 dataMix %>% count(Employment)
 data_uk %>% count(Employment)
 data_tr %>% count(Employment)
 
 dataMix %>% count(FreqUse)
 data_tr %>% count(FreqUse)
 data_uk %>% count(FreqUse)
 
 dataM <- subset(dataMix, dataMix$Gender == 1)
 dataW <- subset(dataMix, dataMix$Gender == 2)
 
 dataM %>% count(Marital)
 dataW %>% count(Marital)
 
 sum(dataMix$Ucla_score >= 20 & dataMix$Ucla_score < 35)
 sum(dataMix$Ucla_score >= 35 & dataMix$Ucla_score < 50)
 sum(dataMix$Ucla_score >= 50 & dataMix$Ucla_score < 65)
 sum(dataMix$Ucla_score >= 65 & dataMix$Ucla_score < 80)
 
 median(dataMix$OCI_score)
 round(sum(dataMix$OCI_score < 137)/679,2)
 
 sum(dataMix$OCI_score < 137)-679
 mean(dataMix$OCI_score)
 sd(dataMix$OCI_score)

#### Hypothesis #####

### H1: There is a relationship between loneliness and internet addiction 
### H2: There is a difference in internet addiction between the two countries 
### H3: There is a difference in loneliness between the two countries
### H4: There is a difference in the relationship between loneliness and addiction between the two countries.

# Multiple linear regression
 
H4 <- lm(OCI_score_c ~ Ucla_score_c*Nationality, data=dataMix)
summary(H4)

#### Assumptions ####

check_model(H4)

# Outlier

assumptioncheck_oci <- dataMix %>% select(OCI_score,Nationality)
assumptioncheck_oci$Nationality <- as.numeric(assumptioncheck_oci$Nationality)

assumptioncheck_oci$Nationality <- as.factor(assumptioncheck_oci$Nationality)
levels(assumptioncheck_oci$Nationality)
contrasts(assumptioncheck_oci$Nationality)

assumptioncheck_oci %>%
  group_by(Nationality) %>%
  identify_outliers(OCI_score)

assumptioncheck <- dataMix %>% select(Ucla_score,Nationality)
assumptioncheck$Nationality <- as.numeric(assumptioncheck$Nationality)

assumptioncheck$Nationality <- as.factor(assumptioncheck$Nationality)
levels(assumptioncheck$Nationality)
contrasts(assumptioncheck$Nationality)

assumptioncheck %>%
  group_by(Nationality) %>%
  identify_outliers(Ucla_score)

# Shapiro Wilk & Normality 

assumptioncheck_oci %>%
  shapiro_test(OCI_score)

assumptioncheck %>%
  shapiro_test(Ucla_score)

check_normality(H4)

# Equal variance Welch t-test

assumptioncheck %>% levene_test(Ucla_score ~ Nationality)
assumptioncheck_oci %>% levene_test(OCI_score ~ Nationality)

# Linearity 

plot(H4, which = 1,pch = 16,
     col = alpha(paletteM[6],0.6))

# Normal Residuals

plot(H4, which = 2,
     pch = 16,
     col = alpha(paletteM[6],0.6)) 

# Homogeneity

plot(H4, which = 3, pch = 16,
     col = alpha(paletteM[6],0.6))
check_heteroscedasticity(H4)

# Cook's distance

plot(H4, which = 4,id.n = 5)

model.diag.metrics <- augment(H4)
model.diag.metrics %>%
  top_n(3, wt = .cooksd)
ggplot(model.diag.metrics, aes(Ucla_score_c, OCI_score_c)) +
  geom_point() +
  stat_smooth(method = lm, se = FALSE) +
  geom_segment(aes(xend = Ucla_score_c, yend = .fitted), color = "red", size = 0.3)

max(cooks.distance(H4))
mean(cooks.distance(H4))

#Leverage

plot(H4, which = 5)

# Collinearity

check_model(H4,
            check = "vif")

# Normality of Residuals

check_model(H4,
            check = "normality")

#### Mann-Whitney U tests ####

# Online Cognition Score
# cummulative logit link model
# Produce descriptive statistics by group

c<-dataMix %>% select(Nationality, OCI_score) %>% group_by(Nationality) %>% 
  summarise(n = n(), 
            mean = mean(OCI_score, na.rm = TRUE), 
            sd = sd(OCI_score, na.rm = TRUE),
            stderr = sd/sqrt(n),
            LCL = mean - qt(1 - (0.05 / 2), n - 1) * stderr,
            UCL = mean + qt(1 - (0.05 / 2), n - 1) * stderr,
            median = median(OCI_score, na.rm = TRUE),
            min = min(OCI_score, na.rm = TRUE), 
            max = max(OCI_score, na.rm = TRUE),
            IQR = IQR(OCI_score, na.rm = TRUE),
            LCLmed = MedianCI(OCI_score, na.rm=TRUE)[2],
            UCLmed = MedianCI(OCI_score, na.rm=TRUE)[3])

c$Nationality <- as.numeric(c$Nationality)
c <- round(c,digits=2)

# Produce Boxplots and visually check for outliers

ggplot(dataMix, aes(x = Nationality, y = OCI_score, fill = Nationality)) +
  stat_boxplot(geom ="errorbar", width = 0.5) +
  geom_boxplot(fill = "light blue") + 
  stat_summary(fun.y=mean, geom="point", shape=10, size=3.5, color="black") + 
  ggtitle("Boxplot of Online cognition scores") + 
  theme_bw() + theme(legend.position="none") +
  scale_x_discrete(labels=c("0" = "TR", "1" = "UK"))
                           
# Test each group for normality

dataMix %>%
  group_by(Nationality) %>%
  summarise(`W Stat` = shapiro.test(OCI_score)$statistic,
            p.value = shapiro.test(OCI_score)$p.value)

# Perform QQ plots by group

ggplot(data = dataMix, mapping = aes(sample = OCI_score, color = Nationality, fill = Nationality)) +
  stat_qq_band(alpha=0.5, conf=0.95, qtype=1, bandType = "boot") +
  stat_qq_line(identity=TRUE) +
  stat_qq_point(col="black") +
  facet_wrap(~ Nationality, scales = "free") +
  labs(x = "Theoretical Quantiles", y = "Sample Quantiles") + theme_bw()

# The Mann-Whitney U test

H2<-wilcox.test(OCI_score ~ Nationality, data=dataMix, na.rm=TRUE, paired=FALSE, exact=FALSE, conf.int=TRUE)
print(H2)

# Hodges Lehmann Estimator

H2$estimate

# UCLA Loneliness Scores

# Produce descriptive statistics by group

z <- dataMix %>% select(Nationality, Ucla_score) %>% group_by(Nationality) %>% 
  summarise(n = n(), 
            mean = mean(Ucla_score, na.rm = TRUE), 
            sd = sd(Ucla_score, na.rm = TRUE),
            stderr = sd/sqrt(n),
            LCL = mean - qt(1 - (0.05 / 2), n - 1) * stderr,
            UCL = mean + qt(1 - (0.05 / 2), n - 1) * stderr,
            median = median(Ucla_score, na.rm = TRUE),
            min = min(Ucla_score, na.rm = TRUE), 
            max = max(Ucla_score, na.rm = TRUE),
            IQR = IQR(Ucla_score, na.rm = TRUE),
            LCLmed = MedianCI(Ucla_score, na.rm=TRUE)[2],
            UCLmed = MedianCI(Ucla_score, na.rm=TRUE)[3])
z$Nationality <- as.numeric(z$Nationality)
z <- round(z, digits = 2)

# Produce Boxplots and visually check for outliers

ggplot(dataMix, aes(x = Nationality, y = Ucla_score, fill = Nationality)) +
  stat_boxplot(geom ="errorbar", width = 0.5) +
  geom_boxplot(fill = "light blue") + 
  stat_summary(fun.y=mean, geom="point", shape=10, size=3.5, color="black") + 
  ggtitle("Boxplot of Loneliness scores") + 
  theme_bw() + theme(legend.position="none") +
  scale_x_discrete(labels=c("0" = "TR", "1" = "UK"))

# Test each group for normality

dataMix %>%
  group_by(Nationality) %>%
  summarise(`W Stat` = shapiro.test(Ucla_score)$statistic,
            p.value = shapiro.test(Ucla_score)$p.value)

# Perform QQ plots by group

ggplot(data = dataMix, mapping = aes(sample = Ucla_score, color = Nationality, fill = Nationality)) +
  stat_qq_band(alpha=0.5, conf=0.95, qtype=1, bandType = "boot") +
  stat_qq_line(identity=TRUE) +
  stat_qq_point(col="black") +
  facet_wrap(~ Nationality, scales = "free") +
  labs(x = "Theoretical Quantiles", y = "Sample Quantiles") + theme_bw()

# The Mann-Whitney U test

H3<-wilcox.test(Ucla_score ~ Nationality, data=dataMix, na.rm=TRUE, paired=FALSE, exact=FALSE, conf.int=TRUE)
print(H3)

# R scores 
wilcoxonR(x = dataMix$Ucla_score,
          g = dataMix$Nationality)

wilcoxonR(x = dataMix$OCI_score,
          g = dataMix$Nationality)

#### Gender ####

dataMix$Gender <- as.numeric(dataMix$Gender)
subset1 <- subset(dataMix, Gender == 1)
subset2 <- subset(dataMix, Gender == 2)
dataGender <- rbind(subset1,subset2)
dataGender$Gender <- as.factor(dataGender$Gender)
contrasts(dataGender$Gender)
#summary(H_alternative1<- lm( OCI_score_c ~ Gender, data=dataGender))
#summary(H_alternative2<- lm( Ucla_score_c ~ Gender, data=dataGender))


genderOCI<-wilcox.test(OCI_score_c ~ Gender, data=dataGender, na.rm=TRUE, paired=F, exact=FALSE, conf.int=TRUE)
print(genderOCI)

ggplot(dataGender, aes(x = Gender, y = OCI_score, fill = Gender)) +
  stat_boxplot(geom ="errorbar", width = 0.5) +
  geom_boxplot(fill = "light blue") + 
  stat_summary(fun.y=mean, geom="point", shape=10, size=3.5, color="black") + 
  ggtitle("Boxplot of Online cognition scores") + 
  theme_bw() + theme(legend.position="none") +
  scale_x_discrete(labels=c("0" = "Men", "1" = "Women"))

genderUCLA<-wilcox.test(Ucla_score ~ Gender, data=dataGender, na.rm=TRUE, paired=F, exact=FALSE, conf.int=TRUE)
print(genderUCLA)

genderUCLA_D <-dataGender %>% select(Gender, Ucla_score) %>% group_by(Gender) %>% 
  summarise(n = n(), 
            mean = mean(Ucla_score, na.rm = TRUE), 
            sd = sd(Ucla_score, na.rm = TRUE),
            stderr = sd/sqrt(n),
            LCL = mean - qt(1 - (0.05 / 2), n - 1) * stderr,
            UCL = mean + qt(1 - (0.05 / 2), n - 1) * stderr,
            median = median(Ucla_score, na.rm = TRUE),
            min = min(Ucla_score, na.rm = TRUE), 
            max = max(Ucla_score, na.rm = TRUE),
            IQR = IQR(Ucla_score, na.rm = TRUE),
            LCLmed = MedianCI(Ucla_score, na.rm=TRUE)[2],
            UCLmed = MedianCI(Ucla_score, na.rm=TRUE)[3])


ggplot(dataGender, aes(x = Gender, y = Ucla_score_c, fill = Gender)) +
  stat_boxplot(geom ="errorbar", width = 0.5) +
  geom_boxplot(fill = "light blue") + 
  stat_summary(fun.y=mean, geom="point", shape=10, size=3.5, color="black") + 
  ggtitle("Boxplot of Online cognition scores") + 
  theme_bw() + theme(legend.position="none") +
  scale_x_discrete(labels=c("1" = "Men", "2" = "Women"))

wilcoxonR(x = dataGender$Ucla_score,
          g = dataGender$Gender)

#### Age ####

# Age distribution
dataMix$Age_c <- scale(dataMix$Age, scale = F)
data_uk$Age_c <- scale(data_uk$Age, scale = F)
data_tr$Age_c <- scale(data_tr$Age, scale = F)

hist(dataMix$Age)
ukage <-hist(data_uk$Age)
trage <-hist(data_tr$Age)

plot( ukage, col=rgb(0,0,1,1/4), xlim=c(10,100))
plot( trage, col=rgb(1,0,0,1/4), xlim=c(10,100), add=T)


summary(H4_alternative<- glm(Ucla_score ~ Age, data=dataMix))
summary(H4_alternative4<- glm(OCI_score_c ~ Age, data=dataMix))

cor(dataMix$Age,dataMix$Ucla_score)
cor(dataMix$Age,dataMix$OCI_score)
options("scipen"=100, "digits"=3)
UCLA_age <- ggscatter(dataMix, x = "Age", y = "Ucla_score", 
                      add = "reg.line", conf.int = TRUE, 
                      cor.coef = TRUE, cor.method = "pearson",
                      xlab = "Age", ylab = "Loneliness",
                      color = paletteM[5],alpha = 0.6,
                      cor.coef.coord=c(45,60),
                      cor.coeff.args = list(digits = 3),
                      digits = 3); UCLA_age

OCI_age <- ggscatter(dataMix, x = "Age", y = "OCI_score", 
                    add = "reg.line", conf.int = TRUE, 
                    cor.coef = TRUE, cor.method = "pearson", 
                    cor.coeff.args = list(digits = 3),
                    xlab = "Age", ylab = "Online Cognition",
                    color = paletteM[5],alpha = 0.6,
                    cor.coef.coord = c(50,200),
                    digits = 3);OCI_age

AGE_plots <- ggarrange(UCLA_age, OCI_age, 
                            labels = c("A", "B"),
                            ncol = 2, nrow = 1, legend= "none");AGE_plots

plot(data_tr$Age,data_tr$Ucla_score)
points(data_uk$Age,data_uk$Ucla_score, col="red")

plot(dataMix$Age_c,dataMix$OCI_score_c,
     main = "Distribution of Age and Internet addiction",
     xlab = "Age",
     ylab = "Loneliness",
     col = (alpha(ifelse(dataMix$Nationality == "0",paletteM[2],paletteM[5]), 0.56)),
     pch = 16)
legend("topright", 
       pch = 16,
       c("Turkey", "United Kingdom","General"), 
       col = c(paletteM[2],paletteM[5],paletteM[1]),
       cex=1)

abline(lm(data_uk$OCI_score_c ~ data_uk$Age_c), col = paletteM[5],lwd=2)
abline(lm(data_tr$OCI_score_c ~ data_tr$Age_c), col = paletteM[2],lwd=2)
abline(lm(dataMix$OCI_score_c ~ dataMix$Age_c), col = paletteM[1],lwd=4)

#### Frequency of Use ####

z <- dataMix %>% group_by(FreqUse) %>% mutate(meanOciT = mean(OCI_score),
                                              sdOciT= sd(OCI_score),
                                              meanOcisoc = mean(OCI_socsup),
                                              sdOcisoc= sd(OCI_socsup),
                                              meanOcidep = mean(OCI_dep),
                                              sdOcidep= sd(OCI_dep),
                                              meanOciatt = mean(OCI_att),
                                              sdOciatt= sd(OCI_att),
                                              meanOciimp = mean(OCI_imp),
                                              sdOciimp= sd(OCI_imp))

f <- dataMix %>% group_by(FreqUse) %>% summarise(meanlon = mean(Ucla_score),
                                                 sdlon= sd(Ucla_score))

# One Way ANOVAs

dataMix$FreqUse <- as.factor(dataMix$FreqUse)
levels(dataMix$FreqUse)
contrasts(dataMix$FreqUse)

H4_alternative1<- lm( OCI_att ~ FreqUse, data=dataMix)
H4_alternative1_1<- lm( OCI_att ~ 1, data=dataMix)
anova(H4_alternative1_1,H4_alternative1)

H4_alternative2<- lm( OCI_score ~ FreqUse, data=dataMix)
H4_alternative2_1<- lm( OCI_score ~ 1, data=dataMix)
anova(H4_alternative2_1,H4_alternative2)

H4_alternative3<- lm( OCI_dep ~ FreqUse, data=dataMix)
H4_alternative3_1<- lm( OCI_dep ~ 1, data=dataMix)
anova(H4_alternative3_1,H4_alternative3)

H4_alternative4<- lm( OCI_socsup ~ FreqUse, data=dataMix)
H4_alternative4_1<- lm( OCI_socsup ~ 1, data=dataMix)
anova(H4_alternative4_1, H4_alternative4)

H4_alternative5<- lm( OCI_imp ~ FreqUse, data=dataMix)
H4_alternative5_1<- lm( OCI_imp ~ 1, data=dataMix)
anova(H4_alternative5_1,H4_alternative5)

H4_alternative6<- lm( Ucla_score ~ FreqUse, data=dataMix)
H4_alternative6_1<- lm( Ucla_score ~ 1, data=dataMix)
anova(H4_alternative6_1, H4_alternative6)


ggplot(dataMix, aes(x=OCI_score_c, y=Ucla_score_c,color=FreqUse,fill=FreqUse))+
  geom_point(aes(fill=FreqUse),
             size = 1.3,
             alpha = .9,
             position =  position_jitterdodge(jitter.width = 0.1, dodge.width = 0.7))

#### Correlations ####

dataS <- dataMix[,c("OCI_socsup","OCI_dep","OCI_imp","OCI_att","Ucla_score")]
ggcorrmat(data= dataS)
mean(dataMix$OCI_socsup)
mean(dataMix$OCI_att)
mean(dataMix$OCI_imp)
mean(dataMix$OCI_dep)
mean(dataMix$OCI_score)
mean(dataMix$Ucla_score)

sd(dataMix$OCI_socsup)
sd(dataMix$OCI_att)
sd(dataMix$OCI_imp)
sd(dataMix$OCI_dep)
sd(dataMix$OCI_score)
sd(dataMix$Ucla_score)

cor.test(dataMix$OCI_dep,dataMix$OCI_score)

#### Purpose of Use ####

y <- dataMix %>% group_by(Purpose_G) %>% mutate(meanOciT = mean(OCI_score),
                                                sdOciT= sd(OCI_score),
                                                meanOcisoc = mean(OCI_socsup),
                                                sdOcisoc= sd(OCI_socsup),
                                                meanOcidep = mean(OCI_dep),
                                                sdOcidep= sd(OCI_dep),
                                                meanOciatt = mean(OCI_att),
                                                sdOciatt= sd(OCI_att),
                                                meanOciimp = mean(OCI_imp),
                                                sdOciimp= sd(OCI_imp));y



