#Murre egg discriminant analysis
library(tidyverse)
library(MASS)
library(lattice)
library(ggord)
library(MVN)
library(caret)
#Based loosely on 
#http://geog.uoregon.edu/bartlein/courses/geog495/lec17.html
#https://stackoverflow.com/questions/31893423/r-plotting-posterior-classification-probabilities-of-a-linear-discriminant-anal


#Read in all the processed egg data. 
egg <- read.csv("file:///C:/Users/11arc/Documents/Montogomerie Work/Eggs/Murre egg processed color and pattern data.csv", as.is=T)

#Scale all the variables you'd like to put into your discriminant analysis. 
scaledegg <- cbind(egg[1:3],scale(egg[,4:42]))

#Right now we have some missing values so remove those
scaledegg2 <- scaledegg %>% group_by(Female) %>% filter(!is.na(meanRound_1) & length(EggNumber)>2)


#A linear discriminant analysis assumes:
# normality
# homogeneity of variance (if violated use quadratic discriminant analysis)
# no colinearity
# independence

#Check homogeneity of variance
plot <- list()
box_variables <- names(scaledegg2)[4:39]
for(i in box_variables) {
  plot[[i]] <- ggplot(scaledegg2, aes_string(x = "Female", y = i, col = "Female", fill = "Female")) + 
    geom_boxplot(alpha = 0.2) + 
    theme(legend.position = "none")+
    geom_jitter()
}

do.call(gridExtra::grid.arrange, c(plot, nrow = 6))
#looks like there is some heterogeneity of variance but on the whole it's really not bad. 


#For now we will carry on as is
flda <- as.formula(paste("Female ~ ", paste(names(scaledegg2[4:42 ]), collapse= "+")))
lda1 <- lda(formula=flda, data=scaledegg2)
#Colinearity!

# # Assess the accuracy of the prediction
# # percent correct for each category of G
# #must redo this with CV=T (now won't run anything else because it's a list not a lda object)
# 
# lda2 <- lda(formula=flda, data=scaledegg2, method="moment", CV=T)
# ct <- table(scaledegg2$Female, lda2$class)
# diag(prop.table(ct, 1))
# # total percent correct
# sum(diag(prop.table(ct)))
# 
# #Make a plot
# ggord(lda1,scaledegg2$Female, 
#       alpha_el=0.2)+
#   theme_classic( base_size = 16)


#############################################################
##########REMOVE COLINEARITY TO GET A BETTER PICTURE

#Whole
corDF = cor(scaledegg2[,c(4:6, 34:42 )])
dissimilarity <- 1 - abs(corDF)
distance <- as.dist(dissimilarity)
hc <- hclust(distance)
clusterV = cutree(hc,h=0.3)
clusterV
#NumbSpots and Scaled Perimeter
#%Area of largest and Total%spots 


#Bottom
corDF = cor(scaledegg2[c(4:6, seq(9, 33, 3)) ])
#NumbSpots and Scaled Perimeter correlate as do %Area of largest and Total%spots 

#Middle
corDF = cor(scaledegg2[c(4:6, seq(8, 33, 3)) ])
#PercArea of largest + round of largest
#Kurtosis and SD round
#Scaled Perim + Total Perc Spot

#Top
corDF = cor(scaledegg2[c(4:6, seq(7, 33, 3)) ])
#Hue and Roundof largest
#mean round and number of spots
#kurtosis and SD round
#PercArea of largest & Total Perc Spot





#I would suggest using Hue, Saturation, LIghtness (MAYBE), Number of spots, mean
#roundness, Total Percent Spots, kurtosis, skew


#####TOP THIRD ONLY
flda_top <- as.formula(Female ~ Hue + Saturation + NumSpots_1 + meanRound_1 +  +kurtosisRound_1 + PercAreaofLargest_1 + RoundofLargest_1+SDround_1)
lda_top <- lda(formula=flda_top, data=scaledegg2)


slda <- train(flda_top, data = scaledegg2,
              method = "stepLDA",
              trControl = trainControl(method = "cv"))
slda$finalModel
mam_top <- lda(Female~ Hue + Saturation+ NumSpots_1, data=scaledegg2)


#Maybe good enough? 
plot_top <- ggord(mam_top,scaledegg2$Female, 
                  alpha_el=0.2)+
  theme_classic( base_size = 16)+
  ggtitle("Top third of egg")




#####MIDDLE THIRD ONLY
flda_middle <- as.formula(Female ~ Hue + Saturation + NumSpots_2 + meanRound_2 +  +kurtosisRound_2 + PercAreaofLargest_2 + RoundofLargest_2+SDround_2)
lda_middle <- lda(formula=flda_middle, data=scaledegg2)

slda <- train(flda_middle, data = scaledegg2,
              method = "stepLDA",
              trControl = trainControl(method = "cv"))
slda$finalModel
mam_middle <- lda(Female~ Hue +Saturation+ NumSpots_2, data=scaledegg2)



plot_middle <- ggord(mam_middle,scaledegg2$Female, 
                     alpha_el=0.2)+
  theme_classic( base_size = 16)+
  ggtitle("Middle third of egg")

####BOTTOM THIRD ONLY
flda_bottom <- as.formula(Female ~ Hue + Saturation + NumSpots_3 + meanRound_3 +  +kurtosisRound_3 + PercAreaofLargest_3 + RoundofLargest_3+SDround_3)
lda_bottom <- lda(formula=flda_bottom, data=scaledegg2)

slda <- train(flda_bottom, data = scaledegg2,
              method = "stepLDA",
              trControl = trainControl(method = "cv"))
slda$finalModel
mam_bottom <- lda(Female~ Hue + Saturation+Num+kurtosisRound_3, data=scaledegg2)

#Maybe good enough? 
plot_bottom <- ggord(lda_bottom,scaledegg2$Female, 
                     alpha_el=0.2)+
  theme_classic( base_size = 16)+
  ggtitle("Bottom third of egg")


####WHOLE EGG
flda_whole <- as.formula(Female~Hue + Saturation + Lightness + NumSpots_whole + meanRound_whole + TotalPercSpot_whole +kurtosisRound_whole + skewRound_whole)
lda_whole <- lda(formula=flda_whole, data=scaledegg2)

result <- mvn(data=scaledegg2[,c(4:6, 25:30 )][-c(6,8)], mvnTest=c("mardia"), multivariatePlot="qq")
result$multivariateNormality #all good! normal 


result <- mvn(data=scaledegg2[,c(4:6, 25:30 )][-c(6,8)], mvnTest="hz")
result$multivariateNormality #normal

result <- mvn(data = scaledegg2[,c(4:6, 25:30 )][-c(6,8)], mvnTest = "royston")
result$multivariateNormality #not normal

mvn(data = scaledegg2[,c(4:6, 25:30 )][-c(6,8)],multivariatePlot="qq")
slda <- train(flda_whole, data = scaledegg2,
              method = "stepLDA",
              trControl = trainControl(method = "cv"))
slda$finalModel
mam_whole <- lda(Female~ meanRound_whole, data=scaledegg2)

#Maybe good enough? 
plot_whole <- ggord(lda_whole,scaledegg2$Female, 
                    alpha_el=0.2, 
                    show.legend=F)+
  theme_classic( base_size = 16)+
  ggtitle("Whole egg")



cowplot::plot_grid(plot_top, plot_middle, plot_bottom, plot_whole,  nrow=2, ncol=2)
