#Murre Egg Location analysis 
library(tidyverse)
library(psych)
library(lme4)
library(lmerTest)
setwd("C:/Users/11arc/Dropbox/AmeliaBob/Murre Eggs")

#Set WD to the dropbox
setwd("C:/Users/11arc/Dropbox/AmeliaBob/Murre Eggs")


egg_spot <- read.csv("Maculation Data (QCed for photo quality).csv")

egg_col <- read.csv("Color Data.csv")


eggs <- full_join(egg_col, egg_spot)

loc <- read.csv("Murre Egg Location and Size Data.csv")[,1:8]
loc$female.id <- gsub("female.", "F", loc$female.id)
loc$egg.type <- recode(loc$egg.type, "first"="First", "replacement"="Replacement")



eggs<- full_join(loc, eggs, by=c("female.id"="Female", "lay.year"="Year", "egg.type"="EggNumber"))

eggs$EggID=paste(eggs$female.id, eggs$lay.year, eggs$egg.type, sep=" ")

names(eggs)[1:8]<- c("Photo", "Female", "Year", "EggNumber", "Site", "Length", "Width", "Mass")

View(eggs %>% filter(is.na(Hue))) #some of these listed eggs I don't have photos for. Get Jaimie to confirm. 


PCA_var <- principal(scale(eggs[!is.na(eggs$NumSpots_whole), 9:14]) , nfactors=2, rotate = "varimax", missing=F)
summary(PCA_var)
PCA_var

eggs$RC1[!is.na(eggs$NumSpots_whole)]<- predict(PCA_var, data=scale(eggs[!is.na(eggs$NumSpots_whole), 9:14]))[,1] #correlated to "spottiness"

eggs$RC2[!is.na(eggs$NumSpots_whole)]<- predict(PCA_var, data=scale(eggs[!is.na(eggs$NumSpots_whole), 9:14]))[,2] #correlated to spot shape




eggs2 <- eggs %>% filter(!is.na(Site))
eggs2$Site <- factor(eggs2$Site, levels=c("LLWB", "SPIT", "ULWB"))

eggs2$Female_hue<- factor(eggs2$Female, levels=unique(eggs2$Female[with(eggs2, order(Site, Hue)) ]))
eggs2$Female_RC1<- factor(eggs2$Female, levels=unique(eggs2$Female[with(eggs2, order(Site, RC1)) ]))
eggs2$Female_RC2<- factor(eggs2$Female, levels=unique(eggs2$Female[with(eggs2, order(Site, RC2)) ]))

ggplot(eggs2%>% filter(!is.na(Site)), aes(x=Female_hue, y=Hue, color=Site))+
  geom_point(size=3)+
  labs(y="Hue", color="Ledge")+
  theme_classic()+
  theme(axis.text.x = element_text(angle = -45, hjust=0))


ggplot(eggs2 %>% filter(!is.na(Site)), aes(x=Female_RC1, y=RC1, color=Site))+
  geom_point(size=3)+
  labs(y="Pattern RC1 (55%)", color="Ledge")+
  theme_classic()+
  theme(axis.text.x = element_text(angle = -45, hjust=0))


ggplot(eggs2 %>% filter(!is.na(Site)), aes(x=Female_RC2, y=RC2, color=Site))+
  geom_point(size=3)+
  labs(y="Pattern RC2 (17%)",  color="Ledge")+
  theme_classic()+
  theme(axis.text.x = element_text(angle = -45, hjust=0))


#All looks very very random. 

#As expected, no differeces between sites. 
mmod_hue <-  lmer(Hue ~   EggNumber + Site +Year + (1|Female), data=eggs2 %>% filter(EggNumber !="unknown"), REML = F)
summary(mmod_hue)
anova(mmod_hue)

mmod_RC1 <- lmer(RC1 ~   EggNumber + Site +Year + (1|Female), data=eggs2 %>% filter(EggNumber !="unknown"), REML = F)
summary(mmod_RC1)
anova(mmod_RC1)


mmod_RC2 <- lmer(RC2 ~   EggNumber + Site +Year + (1|Female), data=eggs2 %>% filter(EggNumber !="unknown"), REML = F)
summary(mmod_RC2)
anova(mmod_RC2)












eggs2_spot <- eggs2 %>% 
  filter(!is.na(RC1) & !is.na(RC2))


boot_spot <- as.data.frame(matrix(nrow=3000, ncol=9, NA))
names(boot_spot) <- c("Site", "LEgg1_RC1", "LEgg2_RC1", "LDif_RC1", "LessDif_RC1", "LEgg1_RC2", "LEgg2_RC2", "LDif_RC2", "LessDif_RC2")
boot_spot$Site <- rep(c("LLWB", "SPIT", "ULWB"))

#COMPARING two eggs from the same ledge, AGAINST A RANDOMLY CHOSEN EGG FROM THE POPULATION (regardless of ledge)

for(i in 1:nrow(boot_spot)){
  #Pick 2 eggs from the same ledge
  sam <- sample(x=eggs2_spot$EggID[eggs2_spot$Site==boot_spot$Site[i]], size=2, replace=F)
  
  
  boot_spot[i,2:3] <- eggs2_spot$RC1[which(eggs2_spot$EggID %in% sam)]
  boot_spot$LDif_RC1[i] <- boot_spot[i,2]-boot_spot[i,3]
  
  boot_spot[i,6:7] <- eggs2_spot$RC2[which(eggs2_spot$EggID %in% sam)]
  boot_spot$LDif_RC2[i] <- boot_spot[i,6]-boot_spot[i,7]
  
  #Randomly choose a another egg (with replacement) from the population
  pop_RC1 <- sample(eggs2_spot$RC1, 2000, replace=T)
  pop_RC2 <- sample(eggs2_spot$RC2, 2000, replace=T)
  bootdif_RC1 <- boot_spot[i,2]-pop_RC1
  bootdif_RC2 <- boot_spot[i,6]-pop_RC2
  
  #How many of those thousand egg pairs are Closer together than two eggs from the same ledge pair?
  boot_spot$LessDif_RC1[i]<- length(which(abs(bootdif_RC1) > abs(boot_spot$LDif_RC1[i])))
  boot_spot$LessDif_RC2[i]<- length(which(abs(bootdif_RC2) > abs(boot_spot$LDif_RC2[i])))
  
  rm(bootdif_RC1, bootdif_RC2, pop_RC1, pop_RC2, sam)
}

mean(boot_spot$LessDif_RC1)/2000

mean(boot_spot$LessDif_RC2)/2000
 #~50% of eggs on the same ledge are more similar than a random egg across the population. 