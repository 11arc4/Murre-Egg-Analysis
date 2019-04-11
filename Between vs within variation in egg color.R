

library(tidyverse)

egg <- read.csv("~/Montogomerie Work/Eggs/Murre egg summarized color and pattern data.csv")

#Quality of photos document--We should remove all the eggs with poor quality photos from the analysis. 
QCegg <- read.csv("~/Montogomerie Work/Eggs/Raw Data_Murre/Murre Eggs Measured.csv")
QCegg$Female2<- paste("F", QCegg$Female, sep="")
QCegg$ProcessingQuality[QCegg$ProcessingQuality=="good"] <- "Good"
QCegg$ProcessingQuality[QCegg$ProcessingQuality=="poor"] <- "Poor"
QCegg$ProcessingQuality[QCegg$ProcessingQuality=="mediocre"] <- "Mediocre"
QCegg$ProcessingQuality <- factor(QCegg$ProcessingQuality, levels=c("Poor", "Mediorcre", "Good"))

EggstoKeep <- QCegg %>% filter(ProcessingQuality!="Poor")

Keep <- paste(EggstoKeep$Female2, EggstoKeep$Year, EggstoKeep$Treatment)

#remove the couple of eggs where their photos were too bad to take good measurements
egg2 <- egg %>% na.omit() %>% mutate(EggID= paste(Female, Year, EggNumber)) %>% filter(EggID  %in% Keep)

#Make a data set including only eggs where we know what eggnumber they were
egg3 <- egg2 %>% filter(EggNumber !="unknown") %>% mutate(Year2=factor(Year))

#Make datasets (with and without unknown egg number eggs) where only the
#important variables are retained, and all other variables are mean centered and
#scaled so SD=1
scaledegg2 <- cbind(egg2[1:3],scale(egg2[,c(4:5,9:14)])) %>% mutate(Year2 = factor(Year))
scaledegg3 <- cbind(egg3[1:3],scale(egg3[,c(4:5,9:14)])) %>% mutate(Year2 = factor(Year))


#clean up workspace
rm( QCegg, EggstoKeep, Keep)


#How much variation is within vs between females?

#for color measurements: we will use egg because this includes all eggs, even the dirty ones, which should be fine 
mod_h <- lm(Hue ~Female, data=egg)
anova(mod_h)
ggplot(egg, aes(x=Female, y=Hue))+
  geom_boxplot()+
  geom_point()

#Between female variance in hue is much much higher than the within female variance (about 12x higher)


mod_s <- lm(Saturation ~Female, data=egg)
anova(mod_s)
ggplot(egg, aes(x=Female, y=Saturation))+
  geom_boxplot()+
  geom_point()

#Between female variance in saturation is higher than within female variance




mod_l <- lm(Lightness ~Female, data=egg)
anova(mod_l)
ggplot(egg, aes(x=Female, y=Lightness))+
  geom_boxplot()+
  geom_point()

#Between female variance in lightness is much is higher than within female variance


#For maculations we will use egg 2 because this includes all eggs that were of decent quality 
