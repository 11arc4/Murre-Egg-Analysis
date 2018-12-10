#Check to see if PCA of whole egg and 0 are the same (whole egg being calculated from
#the three thirds, 0 from a picture with the entire egg)
library(tidyverse)
library(stats)

#Read in all the processed egg data. 
egg <- read.csv("file:///C:/Users/11arc/Documents/Montogomerie Work/Eggs/Murre egg shapes.csv", as.is=T)


mShape_whole <- egg %>% 
  filter(Third !=0)%>%
  group_by(Female, EggNumber, Year) %>% 
  summarise(NumSpots_whole=length(Area), 
            meanRound_whole= mean(Round),
            SDround_whole= sd(Round),
            skewRound_whole= moments::skewness(Round),
            kurtosisRound_whole=moments::kurtosis(Round),
            ScaledPerim_whole=sum(Perim)/(first(TotalArea[which(Third==1)])+first(TotalArea[which(Third==2)])+ first(TotalArea[which(Third==3)]) ),
            TotalPercSpot_whole=sum(Area)/(first(TotalArea[which(Third==1)])+first(TotalArea[which(Third==2)])+ first(TotalArea[which(Third==3)]) ),
            PercAreaofLargest_whole=100*Area[which.max(PercArea)]/(first(TotalArea[which(Third==1)])+first(TotalArea[which(Third==2)])+ first(TotalArea[which(Third==3)]) ),
            RoundofLargest_whole=Round[which.max(PercArea)], 
            Method="AddThird"
  )

mShape_0 <- egg %>% 
  filter(Third ==0)%>%
  group_by(Female, EggNumber, Year) %>% 
  summarise(NumSpots_whole=length(Area), 
            meanRound_whole= mean(Round),
            SDround_whole= sd(Round),
            skewRound_whole= moments::skewness(Round),
            kurtosisRound_whole=moments::kurtosis(Round),
            ScaledPerim_whole=sum(Perim)/first(TotalArea),
            TotalPercSpot_whole=sum(Area)/ first(TotalArea),
            PercAreaofLargest_whole=100*Area[which.max(PercArea)]/first(TotalArea),
            RoundofLargest_whole=Round[which.max(PercArea)], 
            Method="Photo"
  )


#Right now we have some missing values so remove those
egg2 <- rbind(mShape_0, mShape_whole)

color1 <- read.csv( "file:///C:/Users/11arc/Documents/Montogomerie Work/Eggs/Murre egg colors.csv")[,c(1:3, 9:10)]
egg3 <- merge(color1, egg2, all.x=T) %>% filter(!is.na(meanRound_whole))

rm(egg, color1, egg2, mShape_0, mShape_whole)


WholeEggPCA <- prcomp( egg3[,4:14], 
                       center=T, 
                       scale=T, 
                       retx=T)

plot(WholeEggPCA, type="lines")
summary(WholeEggPCA)

ncomp<-2
rawLoadings     <- WholeEggPCA$rotation[,1:ncomp] %*% diag(WholeEggPCA$sdev, ncomp, ncomp)
rotatedLoadings <- varimax(rawLoadings)$loadings
invLoadings     <- t(pracma::pinv(rotatedLoadings))
rownames(invLoadings) <- colnames(egg3[4:14])
scores          <- scale(egg3[4:14]) %*% invLoadings

egg3$PC1 <-  scores[,1]
egg3$PC2 <-  scores[,2]


egg4 <- egg3 %>% group_by(Female, Year, EggNumber) %>% filter(length(Method)>1)




ggplot(egg4 , aes(x=PC1, y=PC2, color=Female, shape=Method))+
  geom_point(size=2)+
  labs(x="PC1 (34.5%)", y="PC2 (18%)")
#F6 is dirty, F1 2017 replacement egg has one giant spot in the middle that's
#being cut in half.



egg4 %>% group_by(Female, Egg, Year) %>% summarise()
