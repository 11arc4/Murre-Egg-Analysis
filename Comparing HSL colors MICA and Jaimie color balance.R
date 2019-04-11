#Color Methods validation


library(tidyverse)

MICA <- read.csv("file:///C:/Users/11arc/OneDrive/Pictures/RAWmurreeggs/Image Analysis Results None.csv", as.is = T)


MICA2 <- MICA %>% 
  separate(col=Label, into=c("Female", "EggNumber", "Year", "Patch"), sep="_") %>%
  select(-Patch) %>%
  group_by(Female, EggNumber, Year) %>%
  summarise_all(mean) %>%
  mutate(R= v.R.NormalisedMean/65535* 256, 
         G=v.G.NormalisedMean/65535* 256, 
         B=v.B.NormalisedMean/65535* 256) 

MICA2$EggNumber[MICA2$EggNumber=="first"] <- "First"
MICA2$EggNumber[MICA2$EggNumber=="replacement"] <- "Replacement"
MICA2$Year <- as.integer(MICA2$Year)

RGB <- as.matrix(rbind(MICA2$R, MICA2$G,MICA2$B))

HSL <- plotwidgets::rgb2hsl(rgb=RGB)

MICA2$Hue <- HSL[1,]
MICA2$Saturation <- HSL[2,]
MICA2$Lightness <- HSL[3,]







#all the data that I'd like to use
ImageJ <- read.csv("file:///C:/Users/11arc/Dropbox/AmeliaBob/Murre Eggs/Murre egg summarized color and pattern data.csv")

ImageJ2 <- ImageJ %>% select(c(Female, EggNumber, Year, Hue, Saturation, Lightness))
MICA3 <- MICA2 %>% select(c(Female, EggNumber, Year, Hue, Saturation, Lightness))


compare<- inner_join(MICA3, ImageJ2, by = c("Female", "EggNumber", "Year"))
#.x= from MICA toolbox. .y= from ImageJ


PA <- ggplot(compare, aes(x=Hue.x, y=Hue.y))+
  geom_point()+
  labs(x="Hue (color balanced in MICA)", y="Hue (color balanced in Lightroom)")+
  geom_smooth(method="lm")

PB <- ggplot(compare, aes(x=Saturation.x, y=Saturation.y))+
  geom_point()+
  labs(x="Saturation (color balanced in MICA)", y="Saturation (color balanced in Lightroom)")+
    geom_smooth(method="lm")


PC <- ggplot(compare, aes(x=Lightness.x, y=Lightness.y))+
  geom_point()+
  labs(x="Lightness (color balanced in MICA)", y="Lightness (color balanced in Lightroom)")+
  geom_smooth(method="lm")


mod_hue <- lm(Hue.y ~ Hue.x+Saturation.x, data=compare)
summary(mod_hue) 
anova(mod_hue)
#Hue is comparable, but not 1 to 1 (MICA is consistantly slightly higher than Jaimies calibration)



mod_sat <- lm(Saturation.y ~ Saturation.x, data=compare)
summary(mod_sat)
#Saturation is not comparable between the two methods. 


mod_light <- lm(Lightness.y ~ Lightness.x, data=compare)
summary(mod_light)
#Lightness is sort of comparable between the two methods, but not very tight relationship. 



cowplot::plot_grid(PA, PB, PC, labels="AUTO")
ggsave("Color")
