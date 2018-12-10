#Multivariate Murre Egg Analysis
library(tidyverse)
library(lme4)
library(lmerTest)
library(MASS)

egg <- read.csv("file:///C:/Users/11arc/Documents/Montogomerie Work/Eggs/Murre egg summarized color and pattern data.csv")

#Quality of photos document--We should remove all the eggs with poor quality photos from the analysis. 
QCegg <- read.csv("file:///C:/Users/11arc/Documents/Montogomerie Work/Eggs/Raw Data_Murre/Murre Eggs Measured.csv")
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

#Check assumptions

plot <- list()
box_variables <- names(scaledegg)[4:11]
for(i in box_variables) {
  plot[[i]] <- ggplot(scaledegg, aes_string(x = "Female", y = i, col = "Female", fill = "Female")) + 
    geom_boxplot(alpha = 0.2) + 
    theme(legend.position = "none")+
    geom_jitter()
}

do.call(gridExtra::grid.arrange, c(plot, nrow = 4))
#Variance looks totally fine. 


#MANOVA to assess whether eggs differ by female
#Drop skew, kurtosis and SD of roundness because not repeatable.
#Scale everything so homogeneity of variance assumptions are met. 
scaledegg <- cbind(egg2[1:3],scale(egg2[,c(4:5,9:14)]))
MANOVA <- manova( as.matrix(scaledegg[,4:11])~Female, data=scaledegg)
summary(MANOVA, test="Pillai") 
# Pillai test is most robust, so we will use that. R automatically transforms
# the Pillai trace into an F stat to get P values since disctbution of Pillai is
# unknown
#Females are different from each other.


summary.aov(MANOVA)
#summary.aov-ing this does all the univariate analyses-- not excatly a
#multivariate appraoch but is advocated by some people. Probably would need a
#bonferoni correction

#Females appear to differ with respect to the number of
#spots, mean roundness of spots scaled perimeter, total % spot, % area of
#largest spot, Hue and Saturation-- basically they differ on pretty much
#everything, except Roundness of the largest spot


#We could also do this analyses as an LDA. 


#For now we will carry on as is
lda1 <- lda(Female~as.matrix(scaledegg[,4:11]), data=scaledegg, CV=F)
lda2 <- lda(Female~as.matrix(scaledegg[,4:11]), data=scaledegg, CV=T)
diag(prop.table(table(scaledegg$Female, lda2$class), 1))
#We are really really bad at correctly classyiing eggs-- not enough eggs, too many females. 

#Hue is totally the 

result <- mvn(data=scaledegg[4:11], mvnTest=c("mardia"), multivariatePlot="qq")
result$multivariateNormality #problems!

result <- mvn(data=scaledegg[  4:11], mvnTest=c("hz"), multivariatePlot="qq")
result$multivariateNormality #problems!
result <- mvn(data=scaledegg[4:11], mvnTest=c("royston"), multivariatePlot="qq")
result$multivariateNormality #problems!

#Looks like we have multivariate colinearity which I suppose we knew....

mvn(data = scaledegg,multivariatePlot="qq")
slda <- train(flda, data = scaledegg,
              method = "stepLDA",
              trControl = trainControl(method = "cv"))
slda$finalModel
mam_whole <- lda(Female~ NumSpots_whole, data=scaledegg)

#Maybe good enough? 
plot_whole <- ggord::ggord(lda1, scaledegg$Female, 
                    alpha_el=0.2, 
                    show.legend=F)+
  theme_classic( base_size = 16)+
  ggtitle("Whole egg")









######PCA USING COLOR AND SHAPE VARIABLES
#Drop skew, kurtosis and SD of roundness because not repeatable. 
PCA <- prcomp( egg3[ , c(4:5,9:14)], 
               center=T, 
               scale=T, 
               retx=T)

plot(PCA, type="lines")
ncomp<-2
rawLoadings     <- PCA$rotation[,1:ncomp] %*% diag(PCA$sdev, ncomp, ncomp)
rotatedLoadings <- varimax(rawLoadings)$loadings
invLoadings     <- t(pracma::pinv(rotatedLoadings))
rownames(invLoadings) <- colnames(egg3[ , c(4:5,9:14)])

loadingsAll[[i]] <- invLoadings
PCAsummaries[[i]] <- rotatedLoadings
scores <- scale(egg3[ , c(4:5,9:14)]) %*% invLoadings

####Calculate PCs for Top of Egg
egg3$PC1 <-  scores[,1]
egg3$PC2 <-  scores[,2]

#This PCA really isn't all that good. ONly PC1 explains a lot of variance, so
#using 2 PCs we only get to 56% explained. Need 5 PCs to get to 91% variance explained. 



#Does PC1 differ between first and replacement eggs? 
mod <- lmer(PC1~EggNumber + (1|Year2) + (1|Female), data=egg3, REML=F)
plot(mod)
#Appear not to need random effect of year-it explains NOTHING
#Female explains something at least but the SE is too large to be worthwhile probably. 
summary(mod)

mod2 <- lm(PC1~EggNumber , data=egg3)
anova(mod2)
summary(mod)
#No difference in PC1 based on egg number (basically maculations aren't
#different since loadings for PC1 are mainly maculations)

ggplot(egg3, aes(x=factor(Year), y=PC1, fill=EggNumber))+
  geom_boxplot()

#Does PC2 differ between first and replacement eggs? 
mod <- lmer(PC2~EggNumber + (1|Year2) + (1|Female), data=egg3, REML=F)
plot(mod)
#Appear not to need random effect of year-it explains very little
#Female explains something at least but the SE is again too large to be worthwhile probably. 
summary(mod)
anova(mod)

mod2 <- lm(PC2~EggNumber , data=egg3)
anova(mod2)
summary(mod)
#No significant difference in PC2 based on egg number (basically color isn't
#different since loadings for PC2 are mainly color) There is a tendency for
#Replacement egg to have higher PC2, but it's slight and there's so much
#variation....Also perhaps heteroskedasticity

ggplot(egg3, aes(x=factor(Year), y=PC2, fill=EggNumber))+
  geom_boxplot()
ggplot(egg3, aes(x=EggNumber, y=PC2))+
  geom_boxplot()




centroids <- egg3 %>% 
  group_by(Female) %>% 
  filter(n()>2)%>%
  summarise(PC1 = mean(PC1), 
            PC2 = mean(PC2)) 
#BASED ON COLOR AND SHAPE PCA#Calculate centroids of all females with more than 2 eggs measured
ggplot()+
  geom_point(data=egg3, aes(x=PC1, y=PC2, color=Female), show.legend = F)+
  geom_point(data=centroids, aes(x=PC1, y=PC2, color=Female), shape=4, show.legend = F ,size=2)+ #add centroid as an x
  theme_classic()+
  ggConvexHull::geom_convexhull(data=egg3, aes(x=PC1, y=PC2, fill=Female), alpha=0.2)




#I really don't think the PCs are a great way to show what's going on. They
#don't seperate the eggs enough and they don't explain enough variation.
#Instead, lets try the LDA. For females where we don't have enough data, we can
#project LD values based on the axis that the other eggs decide on

