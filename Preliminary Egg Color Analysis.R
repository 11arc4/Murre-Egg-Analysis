
library(tidyverse)
library(lme4)
library(MuMIn)
library(lmerTest)

library(MASS)
library(caret)# for the LDA


#################
#Based on means of modal color values

color1 <- read.csv( "file:///C:/Users/11arc/Documents/Montogomerie Work/Eggs/Murre egg colors.csv")
#remove eggs of unknown egg number and those where only one egg was measured within the year
color2 <- color1 %>% filter (EggNumber!="unknown") %>% group_by(Year, Female) %>% filter(length(EggNumber)>1)
color2$Year <- as.factor(color2$Year)


########################MIXED MODEL ANALYSIS OF HUE, SATURATION, AND LIGHTNESS OF FIRST VS REPLACEMENT EGGS

#HUE
huemod <- lmer(Hue ~ EggNumber + (1|Female) +(1|Year), data=color2, REML=F)
plot(huemod)
hist(resid(huemod))
plot(resid(huemod)~color2$EggNumber)
plot(resid(huemod)~color2$Female)
plot(resid(huemod)~color2$Year)
summary(huemod)

huemod2 <- lmer(Hue ~ EggNumber + (1|Female)  , data=color2, REML=F)
huemod3 <- lmer(Hue ~ EggNumber + (1|Year) , data=color2, REML=F)
huemod4 <- lm(Hue ~ EggNumber  , data=color2)

AICc(huemod, huemod2, huemod3, huemod4)
#As expected, don't need a year random effect, but do need a female
anova(huemod2)
#EggNumber is significant (based on lmerTest)
summary(huemod2)
# Hue 89.4 when First, Hue 81.2 when Replacement (Getting less blue but it's only marginal)




#SATURATION
saturationmod <- lmer(Saturation ~ EggNumber + (1|Female) +(1|Year) , data=color2, REML=F)
plot(saturationmod)
hist(resid(saturationmod))
plot(resid(saturationmod)~color2$EggNumber)
plot(resid(saturationmod)~color2$Female)
plot(resid(saturationmod)~color2$Year)
summary(saturationmod)

saturationmod2 <- lmer(Saturation ~ EggNumber + (1|Female)  , data=color2, REML=F)
saturationmod3 <- lmer(Saturation ~ EggNumber + (1|Year) , data=color2, REML=F)
saturationmod4 <- lm(Saturation ~ EggNumber  , data=color2)

AICc(saturationmod, saturationmod2, saturationmod3, saturationmod4)
#As expected, don't need a year or female random effect
anova(saturationmod4)
#EggNumber is not significant (based on lmerTest)
summary(saturationmod4)
#mean saturation is 0.14


#LIGHTNESS
lightnessmod <- lmer(Lightness ~ EggNumber + (1|Female) +(1|Year) , data=color2, REML=F)
plot(lightnessmod)
hist(resid(lightnessmod))
plot(resid(lightnessmod)~color2$EggNumber)
plot(resid(lightnessmod)~color2$Female)
plot(resid(lightnessmod)~color2$Year)
summary(lightnessmod)

lightnessmod2 <- lmer(Lightness ~ EggNumber + (1|Female)  , data=color2, REML=F)
lightnessmod3 <- lmer(Lightness ~ EggNumber + (1|Year) , data=color2, REML=F)
lightnessmod4 <- lm(Lightness ~ EggNumber  , data=color2)

AICc(lightnessmod, lightnessmod2, lightnessmod3, lightnessmod4)
#retain both random effects is about equal to dropping Female random effect so I will go with mod 3
anova(lightnessmod3)
#EggNumber is not significant (based on lmerTest)
summary(lightnessmod3)
#mean lightness is 0.67


ggplot(color2, aes(x=EggNumber, y=Hue))+
  geom_boxplot()+
  geom_jitter()+
  labs(x="Egg")+
  theme_classic(base_size = 16, base_family = "serif")






#####################
#Assessing clustering of egg color based on female using linear discriminant analysis
library(MASS)
library(lattice)
# library(devtools)
# install_github("fawda123/ggord")
library(ggord)
#following http://geog.uoregon.edu/bartlein/courses/geog495/lec17.html
#https://stackoverflow.com/questions/31893423/r-plotting-posterior-classification-probabilities-of-a-linear-discriminant-anal

color1$Hue2 <- scale(color1$Hue)
color1$Saturation2 <- scale(color1$Saturation)
color1$Lightness2 <- scale(color1$Lightness)

#A linear discriminant analysis assumes:
# normality
# homogeneity of variance (if violated use quadratic discriminant analysis)
# no colinearity
# independence

#Check homogeneity of variance
plot <- list()
box_variables <- c("Hue", "Saturation", "Lightness")
for(i in box_variables) {
  plot[[i]] <- ggplot(color1, aes_string(x = "Female", y = i, col = "Female", fill = "Female")) + 
    geom_boxplot(alpha = 0.2) + 
    theme(legend.position = "none")+
    geom_jitter()
}

do.call(gridExtra::grid.arrange, c(plot, nrow = 1))
#looks like there is some homogeneity of variance so we should use QDA instead of LDA. 

# Graphical Assessment of Multivariate Normality
x <- as.matrix(color1[,c(12:14)]) # n x p numeric matrix
center <- colMeans(x) # centroid
n <- nrow(x); p <- ncol(x); cov <- cov(x);
d <- mahalanobis(x,center,cov) # distances
qqplot(qchisq(ppoints(n),df=p),d,
       main="QQ Plot Assessing Multivariate Normality",
       ylab="Mahalanobis D2")
abline(a=0,b=1) 
#That looks OK!



#For the QDA you need groups to be LARGE so that's out for us. Luckily out
#heterogenous variance isn't TOO bad so I will just carry on.
lda1 <- lda(Female ~ Hue2 + Saturation2 + Lightness2, data=color1)
plot(lda1)

loadings <- cor(color1[,12:14])
loadings


# Assess the accuracy of the prediction
# percent correct for each category of G
#must redo this with CV=T (now won't run anything else because it's a list not a lda object)
lda2 <- lda(Female ~ Hue2 + Saturation2 + Lightness2, data=color1, method="moment", CV=T)
ct <- table(color1$Female, lda2$class)
diag(prop.table(ct, 1))
# total percent correct
sum(diag(prop.table(ct)))

#Make a plot
ggord(lda1,color1$Female, 
      alpha_el=0.2)+
  theme_classic( base_size = 16)
#THIS LOOKS SO GOOD


