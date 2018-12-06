#Maculation repeatability analysis. 



library(tidyverse)

##############
#PROCESS THE DATA EXACTLY HOW THE FIRST MEASUREMENT WAS PROCESSED. 
#Set directory
dir <- "~/Montogomerie Work/Eggs/Maculation Repeatability"

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

mShape1 <- mShape[1:n-1,-11] %>% mutate(Measurement=2)



#Read in all the original measurements. 

mShape2 <- read.csv( "file:///C:/Users/11arc/Documents/Montogomerie Work/Eggs/Murre egg shapes.csv") %>% 
  mutate(Measurement=1)


Shapes <- rbind(mShape2, mShape1) %>% filter(Third %in% c('R', "0"))


PShapes <- Shapes %>% 
  filter(Area>0.002)%>%
  group_by(Female, EggNumber, Year, Measurement) %>% 
  summarise(NumSpots_whole=length(Area), 
            meanRound_whole= mean(Round),
            SDround_whole= sd(Round),
            skewRound_whole= moments::skewness(Round),
            kurtosisRound_whole=moments::kurtosis(Round),
            ScaledPerim_whole=sum(Perim)/first(TotalArea),
            TotalPercSpot_whole=sum(Area)/ first(TotalArea)*100,
            PercAreaofLargest_whole=100*Area[which.max(PercArea)]/first(TotalArea),
            RoundofLargest_whole=Round[which.max(PercArea)]
  ) %>% 
  unite(col=EggID , Female, Year, EggNumber) %>% 
  group_by(EggID) %>% filter( n() > 1 )


library(rptR)
Repeatability <- data.frame(Variables=names(PShapes)[3:11], 
                            Repeatability=NA, 
                            se=NA, 
                            LCL=NA, 
                            UCL=NA
                            )

var <- names(PShapes)[3:11]


for (i in 1:nrow(Repeatability)){
  rpt1 <- rpt( as.formula(paste(Repeatability$Variables[i], "~ (1|EggID)")), data=PShapes, grname = "EggID", datatype="Gaussian",
               nboot = 1000, npermut = 0)
   Repeatability$Repeatability[i]<- rpt1$R[1,]
   Repeatability$se[i]<- rpt1$se[1,]
   Repeatability[i,4:5]<- rpt1$CI_emp[1,]
}

ThresholdRepeatability$Cutoff <- 0
Repeatability$Cutoff <- 0.002
ThresholdRepeatability <- rbind(ThresholdRepeatability, Repeatability)
#Changing the spots included doesn't really help much. 


write.csv(Repeatability,"~/Montogomerie Work/Eggs/Repeatability Estimates.csv" , row.names = F)





