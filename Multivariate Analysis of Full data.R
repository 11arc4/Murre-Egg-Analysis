#Murre egg variation

library(tidyverse)
library(psych)
#library(ggConvexHull)
#library(VCA)
library(lme4)


egg <- read.csv("~/Montogomerie Work/Eggs/Murre egg summarized color and pattern data.csv", as.is=T)

#select only those egg variables that are repeatable across different views of the egg


egg2 <- egg %>% 
  select(c(Female, Year, EggNumber, NumSpots_whole, meanRound_whole, ScaledPerim_whole, TotalPercSpot_whole, PercAreaofLargest_whole, RoundofLargest_whole, Hue))%>%
  mutate(Female=ifelse(Female=="LowerLWB8A", "LWB8A", Female), #Just shorted the one name so it fits nicely on the graph
         Year=factor(Year)) 


#Make a variable that will let us plot eggs
egg2$EggNumber2 <- recode(egg2$EggNumber, First=".0", Replacement=".5", unknown=".25")
egg2$YearEgg <- as.numeric(paste(egg2$Year, egg2$EggNumber2, sep=""))

#Some of these eggs were low quality photographs and should be used only for color, NOT for shape parameters.
#We will re-run without those bad eggs later on. 

egg_spot <- egg2 %>% filter(!is.na(NumSpots_whole)) 
egg_col <- egg2 %>% filter(!is.na(Hue)) 


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




#Birds percieve color and pattern seperately, suggesting that they may be at
#least equally important, even though our data is heavily biased toward pattern
#in terms of number of parameters


#One way to deal with that would be to look at color and pattern seperately, and
#decide whether there was more with or between female variance in color and pattern seperately.

#COLOR
mod_color <- lm(Hue~Female, data=egg_col)
anova(mod_color)

mmod_color <- lmer(Hue ~   EggNumber + (1|Female), data=egg_col, REML = F)
summary(mmod_color)
anova(mmod_color)

VarCorr(mmod_color, comp=c("Variance", "Std.Dev"))

r2glmm::r2beta(mmod_color)

#Now we need to test whether 2 eggs from the same female are more similar than 2 eggs from the general population

boot_col <- as.data.frame(matrix(nrow=1000, ncol=4, NA))
names(boot_col) <- c("FEgg1", "FEgg2", "PEgg1", "PEgg2")
for(i in 1:nrow(boot_col)){
  #Pick a random female
  RFem <-  sample(unique(egg_col$Female), 1)
  #Randomly choose 2 eggs from her set of eggs
  boot_col[i,1:2] <- sample(egg_col$Hue[which(egg_col$Female==RFem)], 2, replace=F)
   
  #Randomly choose 2 eggs from the general population
  boot_col[i,3:4]  <- sample(egg_col$Hue, 2, replace=F)
   
}

boot_col$FDif <- boot_col$FEgg1-boot_col$FEgg2
boot_col$PDif <- boot_col$PEgg1-boot_col$PEgg2

ggplot(data=boot_col)+
  geom_histogram(aes(FDif), alpha=0.2, fill="blue")+
  geom_histogram(aes(PDif), alpha=0.2, fill="red")



#Variance in hue between females=6563
#variance in hue within female=208

egg_col <- egg_col %>% group_by(Female) %>% mutate(MeanHue = mean(Hue), 
                                                   MinHue=min(Hue), 
                                                   MaxHue=max(Hue))  
egg_col$Female2 <-   factor(egg_col$Female, levels=unique(egg_col$Female[order(egg_col$MeanHue)]), ordered=TRUE)

ggplot(egg_col, aes(x=Female2, y=Hue))+
  stat_summary(fun.y="mean", shape="-", geom="point", size=7, color="black")+
  geom_segment(aes(x=Female2,xend=Female2, yend=MaxHue, y=MinHue))+
  geom_point(aes(x=Female2, y=Hue, shape=factor(Year), color=EggNumber))+  
  scale_color_manual(values=c("blue", "deeppink1", "gray40"))+
  labs(shape="Year", color="Egg type")+
  theme_classic()+
  theme(axis.text.x = element_text(angle = -45, hjust=0))
ggsave("~/Montogomerie Work/Eggs/Plots_Murre/Variation in hue by female 2.jpeg", height=4, width=8, units="in")





ggplot(egg_col%>% group_by(Female2) %>% filter(EggNumber != 'unknown') %>% filter(n()>1), aes(x=YearEgg, y=Hue, color=Female2))+
  geom_vline(xintercept=c(2016.75, 2017.75))+
  geom_point(aes(shape=EggNumber), size=2, show.legend = F)  +
  geom_line( show.legend = F)+
  labs(x="Year")+
  theme_classic()+
  scale_x_continuous(breaks=c(2016, 2017, 2018))
  #scale_color_brewer()
ggsave("~/Montogomerie Work/Eggs/Plots_Murre/Hue through time.jpeg", height=4, width=8, units="in")


#PATTERN

#First take all our egg maculation data and put them into a PCA
fit <- prcomp(scale(egg_spot2[, 4:9]))
summary(fit) # print variance accounted for
fit # pc loadings
plot(fit,type="lines") # scree plot
biplot(fit) 

PCA_var <- principal(scale(egg_spot2[, 4:9]) , nfactors=2, rotate = "varimax", missing=F)
summary(PCA_var)
PCA_var

egg_spot2$RC1<- predict(PCA_var, data=scale(egg_spot2[, 4:9]))[,1]

egg_spot2$RC2<- predict(PCA_var, data=scale(egg_spot2[, 4:9]))[,2]

egg_spot2 <- egg_spot2 %>% group_by(Female) %>% mutate(MeanRC1 = mean(RC1), 
                                                       MeanRC2 = mean(RC2), 
                                                       MaxRC1=max(RC1), 
                                                       MinRC1=min(RC1), 
                                                       MaxRC2=max(RC2), 
                                                       MinRC2=min(RC2))  
egg_spot2$Female1 <-   factor(egg_spot2$Female, levels=unique(egg_spot2$Female[order(egg_spot2$MeanRC1)]), ordered=TRUE)
egg_spot2$Female2 <-   factor(egg_spot2$Female, levels=unique(egg_spot2$Female[order(egg_spot2$MeanRC2)]), ordered=TRUE)



ggplot(egg_spot2, aes(x=RC1, y=RC2, fill=Female, color=Female) )+
  geom_point(show.legend = F)+
  labs(x="Pattern RC1 (55%)", y="Pattern RC2 (17%)")+
  geom_convexhull(alpha=0.2, show.legend = F)

ggplot(egg_spot2, aes(x=Female1, y=RC1))+
  stat_summary(fun.y="mean", shape="-", geom="point", size=7, color="black")+
  geom_segment(aes(x=Female1,xend=Female1, yend=MaxRC1, y=MinRC1))+
  geom_point(aes(x=Female1, y=RC1, shape=factor(Year), color=EggNumber))+
  #stat_summary(fun.y="mean", shape=4, geom="point", size=3, color="black")+
  scale_color_manual(values=c("blue", "deeppink1", "gray40"))+
  labs(y="Pattern RC1 (55%)", shape="Year", color="Egg type")+
  theme_classic()+
  theme(axis.text.x = element_text(angle = -45, hjust=0))
ggsave("~/Montogomerie Work/Eggs/Plots_Murre/Variation in RC1 by female 2.jpeg", height=4, width=8, units="in")

ggplot(egg_spot2%>% group_by(Female2) %>% filter(EggNumber != 'unknown') %>% filter(n()>1), aes(x=YearEgg, y=RC1, color=Female2))+
  geom_vline(xintercept=c(2016.75, 2017.75))+
  geom_point(aes(shape=EggNumber), size=2, show.legend = F)  +
  geom_line( show.legend = F)+
  labs(x="Year")+
  theme_classic()+
  scale_x_continuous(breaks=c(2016, 2017, 2018))
ggsave("~/Montogomerie Work/Eggs/Plots_Murre/RC1 through time.jpeg", height=4, width=8, units="in")


ggplot(egg_spot2, aes(x=Female2, y=RC2))+
  stat_summary(fun.y="mean", shape="-", geom="point", size=7, color="black")+
  geom_segment(aes(x=Female1,xend=Female1, yend=MaxRC2, y=MinRC2))+
  geom_point(aes(x=Female1, y=RC2, shape=factor(Year), color=EggNumber))+  
  scale_color_manual(values=c("blue", "deeppink1", "gray40"))+
  labs(y="Pattern RC2 (17%)", shape="Year", color="Egg type")+
  theme_classic()+
  theme(axis.text.x = element_text(angle = 90))
ggsave("~/Montogomerie Work/Eggs/Plots_Murre/Variation in RC2 by female 2.jpeg", height=4, width=8, units="in")

ggplot(egg_spot2%>% group_by(Female2) %>% filter(EggNumber != 'unknown') %>% filter(n()>1), aes(x=YearEgg, y=RC2, color=Female2))+
  geom_vline(xintercept=c(2016.75, 2017.75))+
  geom_point(aes(shape=EggNumber), size=2, show.legend = F)  +
  geom_line( show.legend = F)+
  labs(x="Year")+
  theme_classic()+
  scale_x_continuous(breaks=c(2016, 2017, 2018))
ggsave("~/Montogomerie Work/Eggs/Plots_Murre/RC2 through time.jpeg", height=4, width=8, units="in")



mod_RC1 <- lm(RC1 ~ Female, data=egg_spot)
mod_RC2 <- lm(RC2 ~ Female, data=egg_spot)

anova(mod_RC1)
anova(mod_RC2)

#Variance between females
#Rc1: 2.2
#RC2: .9
#variance within females
#RC1:0.4
#RC2: 0.9


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






boot_spotRC1 <- as.data.frame(matrix(nrow=1000, ncol=4, NA))
names(boot_spotRC1) <- c("FEgg1", "FEgg2", "PEgg1", "PEgg2")

boot_spotRC2 <- as.data.frame(matrix(nrow=1000, ncol=4, NA))
names(boot_spotRC2) <- c("FEgg1", "FEgg2", "PEgg1", "PEgg2")


egg_spot3 <- egg_spot2 %>% group_by(Female) %>% filter(n()>1) %>% group_by()

for(i in 1:nrow(boot_spotRC1)){
  #Pick a random female
  RFem <-  sample(unique(egg_spot3$Female), 1)
  #Randomly choose 2 eggs from her set of eggs

  boot_spotRC1[i,1:2] <- sample(egg_spot2$RC1[which(egg_spot2$Female==RFem)], 2, replace=F)
  boot_spotRC2[i,1:2] <- sample(egg_spot2$RC2[which(egg_spot2$Female==RFem)], 2, replace=F)
  
  #Randomly choose 2 eggs from the general population
  boot_spotRC1[i,3:4] <- sample(egg_spot2$RC1, 2, replace=F)
  boot_spotRC2[i,3:4] <- sample(egg_spot2$RC2, 2, replace=F)
  
}

boot_spotRC1$FDif <- boot_spotRC1$FEgg1 - boot_spotRC1$FEgg2
boot_spotRC1$PDif <- boot_spotRC1$PEgg1 - boot_spotRC1$PEgg2

ggplot(data=boot_spotRC1)+
  geom_histogram(aes(FDif), alpha=0.2, fill="blue")+
  geom_histogram(aes(PDif), alpha=0.2, fill="red")


boot_spotRC2$FDif <- boot_spotRC2$FEgg1 - boot_spotRC2$FEgg2
boot_spotRC2$PDif <- boot_spotRC2$PEgg1 - boot_spotRC2$PEgg2

ggplot(data=boot_spotRC2)+
  geom_histogram(aes(FDif), alpha=0.2, fill="blue")+
  geom_histogram(aes(PDif), alpha=0.2, fill="red")


























#Another way to deal with this would be to weight our one color variable 6x as
#heavily as each of our 6 spot variables and put them all together into a
#cluster analysis. 

egg_all <- egg2 %>% filter(!is.na(Hue) & !is.na(NumSpots_whole))%>% mutate(EggID= paste(Female, Year, EggNumber)) %>% filter(EggID  %in% Keep)

seggall<- as.data.frame(scale(egg_all[,4:10]))
seggall$Hue_6x<- seggall$Hue 
#since there is only one color variable compared to 6 pattern variables, I
#scaled hue so it has 6x the variance as the pattern variables.


seggDist<- dist(seggall[,-7], method="euclidean")

eggclust<- hclust(seggDist, method="ward.D2")



plot(eggclust, 
     labels=egg_all$Female, 
     main=NULL, 
     xlab=NULL, 
     hang=-1) 

egg_all$Group <- cutree(eggclust, k=2)


ggplot(egg_all, aes(x=Group, y=Hue))+
  geom_point()

ggplot(egg_all, aes(x=Group, y=NumSpots_whole))+
  geom_point()

#partitioning around medoids
library(fpc)
library(cluster)
pamk.best <- pamk(seggDist) #2 is the best number of clusters-- just like when we do DTW. 
cat("number of clusters estimated by optimum average silhouette width:", pamk.best$nc, "\n")

pam.best <- pam(seggDist, pamk.best$nc)
plot(pam.best) # the PST clusters much better than the RST-- this is expected 

clusplot(pam.best, main = "Cluster plot, k = 2", 
         color = TRUE)

egg_all$Group_pam <- factor(pam.best$clustering)




write.csv(egg_spot2, "file:///C:/Users/11arc/Dropbox/AmeliaBob/Murre Eggs/Maculation Data (QCed for photo quality).csv", row.names = F, na="")
