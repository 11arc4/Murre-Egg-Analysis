#Combine shape files for all eggs


library(tidyverse)
library(plotwidgets)


#Set directory
dir <- "~/Montogomerie Work/Eggs/Murre Egg Spot Size Data"

#Combine all files from said directory into one master color datasheet. 
filenames<- list.files(dir)

#I think including 100 rows for each file should be enough (probably excessive) but if it isn't we can increase that. 
r=100*length(filenames)

mShape <- data.frame(Area=rep(NA,r),
                     Perim=rep(NA,r),
                     Major=rep(NA,r),
                     Minor=rep(NA,r),
                     Angle=rep(NA,r),
                     Circ=rep(NA,r), 
                     XArea=rep(NA,r), 
                     AR=rep(NA,r),
                     Round=rep(NA,r), 
                     Solidity=rep(NA,r),
                     TotalBackground=rep(NA,r),
                     TotalArea=rep(NA,r),
                     PercArea=rep(NA,r),
                     Female=rep(NA,r),
                     Year=rep(NA,r),
                     EggNumber=rep(NA,r), 
                     Third=rep(NA,r))


n  <- 1
for (i in 1:length(filenames)){
  file <- filenames[i]
  #Load in file
  shape0 <- read.csv(paste(dir, file, sep="/"), as.is = T, na.strings = "")
  #sometimes it saves with all the labels, sometimes not, unclear when so I've written the code to know that's a difference
  if(any(names(shape0)=="Label")){
    shape1.1 <- shape0[,-c(1,2)]
  } else {
    shape1.1 <- shape0[,-c(1)]
    
  }
  
  if(any(names(shape0)=="XM")){
    shape <- shape1.1[ , -which(names(shape1.1) %in% c("X","Y", "XM", "YM"))]
  } else {
    if(any(names(shape0)=="X")){
      #shape <- shape1.1 %>% select(-c("X", "Y"))
      shape <- shape1.1[ , -which(names(shape1.1) %in% c("X","Y"))]
    }
    
  }
  
  #Calculate Total Area of the space
  
  if(any("TotalBackground"==names(shape))){
    shape$TotalArea <- sum(shape$Area)+shape$TotalBackground[1] #adding the area of all the black splotches and the background
  } else {
    if(any("BackgroundArea"==names(shape))){
    shape$TotalArea <- sum(shape$Area)+shape$BackgroundArea[1] #adding the area of all the black splotches and the background
    } else {
      message(file, "has wrong background area name")
    }
  }
 
    shape$PercArea <- shape$Area/shape$TotalArea*100
 
  
  #Pull important identifying info out of the file name
  female <- strsplit(file, "_")[[1]][1]
  year <- strsplit(file, "_")[[1]][2]
  eggNumber <- strsplit(file, "_")[[1]][3]
  third <-  substring(strsplit(file, "_")[[1]][4],1,  nchar(strsplit(file, "_")[[1]][4])-4)
  
  shape$Female <- female
  shape$Year <- year
  shape$EggNumber <- eggNumber
  shape$Third <- third
  
  
  if(ncol(shape)!=ncol(mShape)){
    message("check initial file for ", file, ". There is something wrong.")
    #if you get this message, there's something wrong with your initial file. 
    } else {
        mShape[n:(n+nrow(shape)-1),]<- shape
        n<- n+nrow(shape)
        
  }
}

#Truncate file to hold only the proper stuff
mShape1 <- mShape[1:n-1,-11]

write.csv(mShape1, "file:///C:/Users/11arc/Documents/Montogomerie Work/Eggs/Raw murre egg maculations.csv", na="", row.names = F)



#Tidy up
rm(mShape, shape, filenames, file, eggNumber, female, i, n, r, third, year, shape0, shape1.1)



#############################
#Take the large file of egg patterns, and extract informative information at the egg third level and whole egg level
mShape_Thirds <- mShape1 %>% 
  filter(Third!=0)%>%
  group_by(Female, EggNumber, Year, Third) %>% 
  summarise(NumSpots=length(Area), 
            meanRound= mean(Round),
            SDround= sd(Round),
            skewRound= moments::skewness(Round),
            kurtosisRound=moments::kurtosis(Round),
            ScaledPerim=sum(Perim)/sqrt(TotalArea[1]),
            TotalPercSpot=sum(Area)/TotalArea[1],
            PercAreaofLargest=max(PercArea),
            RoundofLargest=Round[which.max(PercArea)[1]]
  ) %>% 
  gather(variable, value, -c(Female, EggNumber, Year, Third)) %>%
  unite(temp, variable, Third) %>%
  spread(temp, value)

#make thirds whid instead of long.   
 
##Recalculate Perc. Area based on the area of the total Egg.  
# mShape_whole <- mShape1 %>% 
#   filter(Third!=0)%>%
#   group_by(Female, EggNumber, Year) %>% 
#   summarise(NumSpots_whole=length(Area), 
#             meanRound_whole= mean(Round),
#             SDround_whole= sd(Round),
#             skewRound_whole= moments::skewness(Round),
#             kurtosisRound_whole=moments::kurtosis(Round),
#             ScaledPerim_whole=sum(Perim)/(first(TotalArea[which(Third==1)])+first(TotalArea[which(Third==2)])+ first(TotalArea[which(Third==3)]) ),
#             TotalPercSpot_whole=sum(Area)/(first(TotalArea[which(Third==1)])+first(TotalArea[which(Third==2)])+ first(TotalArea[which(Third==3)]) ),
#             PercAreaofLargest_whole=100*Area[which.max(PercArea)]/(first(TotalArea[which(Third==1)])+first(TotalArea[which(Third==2)])+ first(TotalArea[which(Third==3)]) ),
#             RoundofLargest_whole=Round[which.max(PercArea)])
            
#Calculate for whole egg
mShape_0 <- mShape1 %>% 
  filter(Third ==0)%>%
  group_by(Female, EggNumber, Year) %>% 
  summarise(NumSpots_whole=n(), 
            meanRound_whole= mean(Round),
            SDround_whole= sd(Round),
            skewRound_whole= moments::skewness(Round),
            kurtosisRound_whole=moments::kurtosis(Round),
            ScaledPerim_whole=sum(Perim)/sqrt(first(TotalArea)), #NOT SCALE INDEPENDENT
            TotalPercSpot_whole=sum(Area)/ first(TotalArea),
            PercAreaofLargest_whole=100*Area[which.max(PercArea)]/first(TotalArea),
            RoundofLargest_whole=Round[which.max(PercArea)]
  )


color1 <- read.csv( "file:///C:/Users/11arc/Documents/Montogomerie Work/Eggs/Murre egg colors.csv")[,c(1:3, 9:11)]


EggData <- merge(color1, mShape_0, all.x=T)

write.csv(EggData,"file:///C:/Users/11arc/Documents/Montogomerie Work/Eggs/Murre egg summarized color and pattern data.csv", row.names=F, na="" )

