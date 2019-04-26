#Murre egg variation

library(tidyverse)
library(psych)
library(ggConvexHull)
library(lme4)
library(lmerTest)

#Set WD to the dropbox
setwd("C:/Users/11arc/Dropbox/AmeliaBob/Murre Eggs")

egg <- read.csv("Murre egg summarized color and pattern data.csv", as.is=T)

#select only those egg variables that are repeatable across different views of the egg
egg2 <- egg %>% 
  select(c(Female, Year, EggNumber, NumSpots_whole, meanRound_whole, ScaledPerim_whole, TotalPercSpot_whole, PercAreaofLargest_whole, RoundofLargest_whole, Hue))%>%
  mutate(Female=ifelse(Female=="LowerLWB8A", "LWB8A", Female), #Just shorted the one name so it fits nicely on the graph
         Year=factor(Year)) 


#Make a variable that will let us plot eggs
egg2$EggNumber2 <- recode(egg2$EggNumber, First=".0", Replacement=".5", unknown=".25")
egg2$YearEgg <- as.numeric(paste(egg2$Year, egg2$EggNumber2, sep=""))

egg_col <- egg2 %>% filter(!is.na(Hue)) 
 write.csv(egg_col, "file:///C:/Users/11arc/Dropbox/AmeliaBob/Murre Eggs/Color Data.csv", row.names = F, na="")


#A BUNCH OF EGG PHOTOS WERE POOR QUALITY. I REMOVED THEM FROM THE SPOT DATASET
#USING THIS CODE, AND YOU CAN LOAD THE QC'ED DATASET FROM DROPBOX
egg_spot <- egg2 %>% filter(!is.na(NumSpots_whole))

#SOme of the eggs were so dirty it's pretyt much impossible to get a maculation score.
QCegg <- read.csv("file:///C:/Users/11arc/OneDrive/Documents/Montogomerie Work/Eggs/Raw Data_Murre/Murre Eggs Measured.csv")
QCegg$Female2<- paste("F", QCegg$Female, sep="")
QCegg$ProcessingQuality[QCegg$ProcessingQuality=="good"] <- "Good"
QCegg$ProcessingQuality[QCegg$ProcessingQuality=="poor"] <- "Poor"
QCegg$ProcessingQuality[QCegg$ProcessingQuality=="mediocre"] <- "Mediocre"
QCegg$ProcessingQuality <- factor(QCegg$ProcessingQuality, levels=c("Poor", "Mediorcre", "Good"))

EggstoKeep <- QCegg %>% filter(ProcessingQuality!="Poor")

Keep <- paste(EggstoKeep$Female2, EggstoKeep$Year, EggstoKeep$Treatment)
egg_spot2 <- egg2 %>% na.omit() %>% mutate(EggID= paste(Female, Year, EggNumber)) %>% filter(EggID  %in% Keep)
write.csv(egg_spot2, "file:///C:/Users/11arc/Dropbox/AmeliaBob/Murre Eggs/Maculation Data (QCed for photo quality).csv", row.names = F, na="")

egg_spot2 <- read.csv("Maculation Data (QCed for photo quality).csv")

###########################################################################################################################

#Birds percieve color and pattern seperately, suggesting that they may be at
#least equally important, even though our data is heavily biased toward pattern
#in terms of number of parameters


#One way to deal with that would be to look at color and pattern seperately, and
#decide whether there was more with or between female variance in color and pattern seperately.

#COLOR
mmod_color <- lmer(Hue ~   EggNumber +Year + (1|Female), data=egg_col %>% filter(EggNumber !="unknown"), REML = F)
summary(mmod_color)
anova(mmod_color)

#r2glmm::r2beta(mmod_color, partial=T, method="nsj") 
#This cannot be doing what they say it is doing, because Nakagawa and
#Schilelzeth 2013 specifically says that you only get marginal and conditional
#R2 from their methods. To look at variance from fixed effects individually you
#must use PCV

MuMIn::r.squaredGLMM(mmod_color)
#Fixed effects (egg Number and Year) account for <1% of variance in hue. Female ID accounts for 91% of variance. 


#RESAMPLING TEST OF WHETHER FEMALE'S FIRST AND REPLACEMENT EGGS ARE MORE SIMILAR THAN RANDOM PAIR
#Now we need to test whether 2 eggs from the same female are more similar than 2 eggs from the general population

#Make a dataset of only females with a first and replacement egg in the same year

egg_boot <- egg2 %>% 
  filter(EggNumber !="unknown") %>% 
  group_by(Female, Year) %>% 
  filter(n()==2 ) %>%
  arrange(Year)

boot_col <- as.data.frame(matrix(nrow=length(unique(egg_boot$Female)), ncol=4, NA))
names(boot_col) <- c("FEgg1", "FEgg2", "FDif", "LessDif")

#COMPARING FEMALE'S FIRST AND REPLACEMENT EGG, AGAINST A RANDOMLY CHOSEN REPLACEMENT EGG FROM THE POPULATION
i=0
for(RFem in unique(egg_boot$Female)){
  i=i+1
  #Pick her first PAIR of eggs
  boot_col[i,1:2] <- egg_boot$Hue[which(egg_boot$Female==RFem)][1:2]
  boot_col$FDif[i] <- boot_col[i,1]-boot_col[i,2]

  bootdif <- rep(NA, 2000)
  for(j in 1:length(bootdif)){
    #Randomly choose a another female's replacement egg (with replacement)
    replacement <- sample(egg_boot$Hue[which(egg_boot$EggNumber=="Replacement")], 1, replace=T)
    bootdif[j] <- boot_col[i,1]-replacement
  }
  #How many of those thousand egg pairs are farther apart than the female's pair?
  boot_col$LessDif[i]<- length(which(abs(bootdif) > abs(boot_col$FDif[i])))
}

mean(boot_col$LessDif)/2000
#CONCLUSIONS: 78% of random eggs would be farther apart than female and her own egg based on hue. 


#COMPARING FEMALES OWN EGGS AGAINST 2 RANDOMLY CHOSEN EGGS FROM THE POPULATION
# i=0
# for(RFem in unique(egg_boot$Female)){
#   i=i+1
#   #Pick her first PAIR of eggs
#   boot_col[i,1:2] <- egg_boot$Hue[which(egg_boot$Female==RFem)][1:2]
#   boot_col$FDif[i] <- boot_col[i,1]-boot_col[i,2]
#   
#   bootdif <- rep(NA, 1000)
#   for(j in 1:length(bootdif)){
#     #Randomly choose another females first egg
#     first  <- sample(egg_boot$Hue[which(egg_boot$EggNumber=="First")], 1, replace=T)
#     #Randomly choose a another female's replacement egg (with replacement) 
#     replacement <- sample(egg_boot$Hue[which(egg_boot$EggNumber=="Replacement")], 1, replace=T)
#     bootdif[j] <- first-replacement
#   }
#   #How many of those thousand egg pairs are Closer together than the female's pair? 
#   boot_col$LessDif[i]<- length(which(bootdif < abs(boot_col$FDif[i])))
# }
# 
# mean(boot_col$LessDif)/1000
#CONCLUSIONS: No significant difference between coloration of first and
#replacement eggs of a female compared to what you'd expect from the population
#assorting randomly. Female's own eggs are less different than 60% of random
#pairs.


#Make some nice plots of egg hue
egg_col <- egg_col %>% group_by(Female) %>% mutate(MeanHue = mean(Hue), 
                                                   MinHue=min(Hue), 
                                                   MaxHue=max(Hue))  
egg_col$Female2 <-   factor(egg_col$Female, levels=unique(egg_col$Female[order(egg_col$MeanHue)]), ordered=TRUE)

#Plot egg hue for each female
ggplot(egg_col, aes(x=Female2, y=Hue))+
  stat_summary(fun.y="mean", shape="-", geom="point", size=7, color="black")+
  geom_segment(aes(x=Female2,xend=Female2, yend=MaxHue, y=MinHue))+
  geom_point(aes(x=Female2, y=Hue, shape=factor(Year), color=EggNumber))+  
  scale_color_manual(values=c("blue", "deeppink1", "gray40"))+
  labs(shape="Year", color="Egg type")+
  theme_classic()+
  theme(axis.text.x = element_text(angle = -45, hjust=0))
#ggsave("~/Montogomerie Work/Eggs/Plots_Murre/Variation in hue by female 2.jpeg", height=4, width=8, units="in")




#Plot egg hue through time for first and replacement eggs only. Only show females with more than one egg
ggplot(egg_col%>% 
         group_by(Female2) %>% 
         filter(EggNumber != 'unknown') %>% #only show first and replacement eggs
         filter(n()>1), #Only show females with more than one egg
       aes(x=YearEgg, y=Hue, color=Female2))+
  geom_vline(xintercept=c(2016.75, 2017.75))+
  geom_point(aes(shape=EggNumber), size=2, show.legend = F)  +
  geom_line( show.legend = F)+
  labs(x="Year")+
  theme_classic()+
  scale_x_continuous(breaks=c(2016, 2017, 2018))
#ggsave("~/Montogomerie Work/Eggs/Plots_Murre/Hue through time.jpeg", height=4, width=8, units="in")


#PATTERN

#First take all our egg maculation data and put them into a PCA
fit <- prcomp(scale(egg_spot2[, 4:9]))
summary(fit) # print variance accounted for
fit # pc loadings
plot(fit,type="lines") # scree plot
biplot(fit) 

#Varimax rotate the PCA into 2 axes.
PCA_var <- principal(scale(egg_spot2[, 4:9]) , nfactors=2, rotate = "varimax", missing=F)
summary(PCA_var)
PCA_var

egg_spot2$RC1<- predict(PCA_var, data=scale(egg_spot2[, 4:9]))[,1] #correlated to "spottiness"

egg_spot2$RC2<- predict(PCA_var, data=scale(egg_spot2[, 4:9]))[,2] #correlated to spot shape


mmod_RC1 <- lmer(RC1 ~   EggNumber +Year + (1|Female), data=egg_spot2 %>% filter(EggNumber !="unknown"), REML = F)
summary(mmod_RC1)
anova(mmod_RC1)

MuMIn::r.squaredGLMM(mmod_RC1)
#Fixed effects (egg Number and Year) account for ~1% of variance in spottiness (RC1) Female ID accounts for 58% of variance. 

mmod_RC2 <- lmer(RC2 ~   EggNumber +Year + (1|Female), data=egg_spot2 %>% filter(EggNumber !="unknown"), REML = F)
summary(mmod_RC2)
anova(mmod_RC2)

MuMIn::r.squaredGLMM(mmod_RC2)
#Neither fixed effects (egg number of year) nor female ID explain shape of egg spot. THis is really very unsurprising. 


#Is within female spottiness less random than expected?
egg_boot <- egg2 %>% 
  filter(EggNumber !="unknown") %>% 
  group_by(Female, Year) %>% 
  filter(n()==2 ) %>%
  arrange(Year)
egg_boot$RC1<- predict(PCA_var, data=scale(egg_boot[, 4:9]))[,1]

egg_boot$RC2<- predict(PCA_var, data=scale(egg_boot[, 4:9]))[,2]

boot_spot <- as.data.frame(matrix(nrow=length(unique(egg_boot$Female)), ncol=8, NA))
names(boot_spot) <- c("FEgg1_RC1", "FEgg2_RC1", "FDif_RC1", "LessDif_RC1", "FEgg1_RC2", "FEgg2_RC2", "FDif_RC2", "LessDif_RC2")


#COMPARING FEMALE'S FIRST AND REPLACEMENT EGG, AGAINST A RANDOMLY CHOSEN REPLACEMENT EGG FROM THE POPULATION

i=0
for(RFem in unique(egg_boot$Female)){
  i=i+1
  #Pick her first PAIR of eggs
  boot_spot[i,1:2] <- egg_boot$RC1[which(egg_boot$Female==RFem)][1:2]
  boot_spot$FDif_RC1[i] <- boot_spot[i,1]-boot_spot[i,2]
  
  boot_spot[i,5:6] <- egg_boot$RC2[which(egg_boot$Female==RFem)][1:2]
  boot_spot$FDif_RC2[i] <- boot_spot[i,5]-boot_spot[i,6]
  
  bootdif_RC1 <- rep(NA, 2000)
  bootdif_RC2 <- rep(NA, 2000)
  
  for(j in 1:length(bootdif_RC1)){
    
    #Randomly choose a another female's replacement egg (with replacement)
    replacement_RC1 <- sample(egg_boot$RC1[which(egg_boot$EggNumber=="Replacement")], 1)
    replacement_RC2 <- sample(egg_boot$RC2[which(egg_boot$EggNumber=="Replacement")], 1)
    bootdif_RC1[j] <- boot_spot[i,1]-replacement_RC1
    bootdif_RC2[j] <- boot_spot[i,5]-replacement_RC2
    
  }
  #How many of those thousand egg pairs are Closer together than the female's pair?
  boot_spot$LessDif_RC1[i]<- length(which(abs(bootdif_RC1) > abs(boot_spot$FDif_RC1[i])))
  boot_spot$LessDif_RC2[i]<- length(which(abs(bootdif_RC2) > abs(boot_spot$FDif_RC2[i])))
  
}
boot_spot <- boot_spot %>% filter(!is.na(FEgg1_RC1))

mean(boot_spot$LessDif_RC1)/nrow(boot_spot)

mean(boot_spot$LessDif_RC2)/nrow(boot_spot)

#CONCLUSIONS: In only about 30% of the cases is the female's own egg spots more
#similar than a random pairing (either RC1 or RC2).




# COMPARING A FEMALE'S FIRST PAIR TO HER FIRST EGG AND A RANDOM REPLACEMENT EGG
# i=0
# for(RFem in unique(egg_boot$Female)){
#   i=i+1
#   #Pick her first PAIR of eggs
#   boot_spot[i,1:2] <- egg_boot$RC1[which(egg_boot$Female==RFem)][1:2]
#   boot_spot$FDif_RC1[i] <- boot_spot[i,1]-boot_spot[i,2]
#   
#   boot_spot[i,5:6] <- egg_boot$RC2[which(egg_boot$Female==RFem)][1:2]
#   boot_spot$FDif_RC2[i] <- boot_spot[i,5]-boot_spot[i,6]
#   
#   bootdif_RC1 <- rep(NA, 2000)
#   bootdif_RC2 <- rep(NA, 2000)
#   
#   for(j in 1:length(bootdif)){
#     #Randomly choose another females first egg
#     first_RC1  <- sample(egg_boot$RC1[which(egg_boot$EggNumber=="First")], 1, replace=T)
#     first_RC2  <- sample(egg_boot$RC2[which(egg_boot$EggNumber=="First")], 1, replace=T)
#     
#     #Randomly choose a another female's replacement egg (with replacement) 
#     replacement_RC1 <- sample(egg_boot$RC1[which(egg_boot$EggNumber=="Replacement")], 1, replace=T)
#     replacement_RC2 <- sample(egg_boot$RC2[which(egg_boot$EggNumber=="Replacement")], 1, replace=T)
#     bootdif_RC1[j] <- first_RC1-replacement_RC1
#     bootdif_RC2[j] <- first_RC1-replacement_RC2
#     
#   }
#   #How many of those thousand egg pairs are Closer together than the female's pair? 
#   boot_spot$LessDif_RC1[i]<- length(which(bootdif_RC1 < abs(boot_spot$FDif_RC1[i])))
#   boot_spot$LessDif_RC2[i]<- length(which(bootdif_RC2 < abs(boot_spot$FDif_RC2[i])))
#   
# }


#Make some plots
egg_spot2 <- egg_spot2 %>% group_by(Female) %>% mutate(MeanRC1 = mean(RC1), 
                                                       MeanRC2 = mean(RC2), 
                                                       MaxRC1=max(RC1), 
                                                       MinRC1=min(RC1), 
                                                       MaxRC2=max(RC2), 
                                                       MinRC2=min(RC2))  
egg_spot2$Female1 <-   factor(egg_spot2$Female, levels=unique(egg_spot2$Female[order(egg_spot2$MeanRC1)]), ordered=TRUE)
egg_spot2$Female2 <-   factor(egg_spot2$Female, levels=unique(egg_spot2$Female[order(egg_spot2$MeanRC2)]), ordered=TRUE)


#plot RC1 vs RC2 to get a good idea of the spot world for the eggs
ggplot(egg_spot2, aes(x=RC1, y=RC2, fill=Female, color=Female) )+
  geom_point(show.legend = F)+
  labs(x="Pattern RC1 (55%)", y="Pattern RC2 (17%)")+
  geom_convexhull(alpha=0.2, show.legend = F)

#Plot Spottiness (RC1) for each female
ggplot(egg_spot2, aes(x=Female1, y=RC1))+
  stat_summary(fun.y="mean", shape="-", geom="point", size=7, color="black")+
  geom_segment(aes(x=Female1,xend=Female1, yend=MaxRC1, y=MinRC1))+
  geom_point(aes(x=Female1, y=RC1, shape=factor(Year), color=EggNumber))+
  #stat_summary(fun.y="mean", shape=4, geom="point", size=3, color="black")+
  scale_color_manual(values=c("blue", "deeppink1", "gray40"))+
  labs(y="Pattern RC1 (55%)", shape="Year", color="Egg type")+
  theme_classic()+
  theme(axis.text.x = element_text(angle = -45, hjust=0))
#ggsave("~/Montogomerie Work/Eggs/Plots_Murre/Variation in RC1 by female 2.jpeg", height=4, width=8, units="in")

#Spottiness though time (for only first and replacement eggs of females with multiple eggs)
ggplot(egg_spot2%>% 
         group_by(Female2) %>% 
         filter(EggNumber != 'unknown') %>% 
         filter(n()>1), 
       aes(x=YearEgg, y=RC1, color=Female2))+
  geom_vline(xintercept=c(2016.75, 2017.75))+
  geom_point(aes(shape=EggNumber), size=2, show.legend = F)  +
  geom_line( show.legend = F)+
  labs(x="Year")+
  theme_classic()+
  scale_x_continuous(breaks=c(2016, 2017, 2018))
#ggsave("~/Montogomerie Work/Eggs/Plots_Murre/RC1 through time.jpeg", height=4, width=8, units="in")

#Plot spot shape (RCr) for each female
ggplot(egg_spot2, aes(x=Female2, y=RC2))+
  stat_summary(fun.y="mean", shape="-", geom="point", size=7, color="black")+
  geom_segment(aes(x=Female1,xend=Female1, yend=MaxRC2, y=MinRC2))+
  geom_point(aes(x=Female1, y=RC2, shape=factor(Year), color=EggNumber))+  
  scale_color_manual(values=c("blue", "deeppink1", "gray40"))+
  labs(y="Pattern RC2 (17%)", shape="Year", color="Egg type")+
  theme_classic()+
  theme(axis.text.x = element_text(angle = 90))
#ggsave("~/Montogomerie Work/Eggs/Plots_Murre/Variation in RC2 by female 2.jpeg", height=4, width=8, units="in")

#Spot shape though time (again, only first and replacement eggs for females with multiple shown here.)
ggplot(egg_spot2%>% group_by(Female2) %>% filter(EggNumber != 'unknown') %>% filter(n()>1), aes(x=YearEgg, y=RC2, color=Female2))+
  geom_vline(xintercept=c(2016.75, 2017.75))+
  geom_point(aes(shape=EggNumber), size=2, show.legend = F)  +
  geom_line( show.legend = F)+
  labs(x="Year")+
  theme_classic()+
  scale_x_continuous(breaks=c(2016, 2017, 2018))
ggsave("~/Montogomerie Work/Eggs/Plots_Murre/RC2 through time.jpeg", height=4, width=8, units="in")




#Big picture plots
ggplot(egg_spot2, aes(x=Hue, y=RC1, fill=Female, color=Female) )+
  geom_point(show.legend = F)+
  labs(y="Pattern RC1 (55%)", x="Hue")+
  geom_convexhull(alpha=0.2, show.legend = F)+
  theme_classic()


ggplot(egg_spot2, aes(x=Hue, y=RC2, fill=Female, color=Female) )+
  geom_point(show.legend = F)+
  labs(y="Pattern RC2 (17%)", x="Hue")+
  geom_convexhull(alpha=0.2, show.legend = F)+
  theme_classic()

ggplot(egg_spot2, aes(x=RC1, y=RC2, fill=Female, color=Female) )+
  geom_point(show.legend = F)+
  labs(x="Pattern RC1 (55%)", y="Pattern RC2 (17%)")+
  geom_convexhull(alpha=0.2, show.legend = F)+
  theme_classic()



library(plotly)
plot_ly(egg_spot2, x=~Hue,y=~RC1,z=~RC2, color=~Female)


