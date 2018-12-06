#Combine color files for all eggs and convert RGB to HSL values.


library(tidyverse)
library(plotwidgets)


#Set directory
dir <- "~/Montogomerie Work/Eggs/Egg Color Data"

#Combine all files from said directory into one master color datasheet. 
filenames<- list.files(dir)

r=50*length(filenames)

mColor <- data.frame(X=rep(NA,r), 
                     Label=rep(NA,r), 
                     Area=rep(NA,r), 
                     Mean=rep(NA,r), 
                     Mode=rep(NA,r), 
                     Min=rep(NA,r),
                     Max=rep(NA,r),
                     Female=rep(NA,r),
                     Year=rep(NA,r),
                     EggNumber=rep(NA,r))


n  <- 1
for (i in 1:length(filenames)){
  file <- filenames[i]
  
  #Load in file
  color <- read.csv(paste(dir, file, sep="/"), as.is=T, na.strings = "")
  
  #Pull important identifying info out of the file name
  female <- strsplit(file, "_")[[1]][2]
  year <- strsplit(file, "_")[[1]][3]
  eggNumber <- substring(strsplit(file, "_")[[1]][4],1,  nchar(strsplit(file, "_")[[1]][4])-4)
  
  color$Female <- female
  color$Year <- year
  color$EggNumber <- eggNumber
  
  if(nrow(color)==50){
    mColor[n:(n+49),]<- color
    n<- n+50
  } else {
    message("Check ", file, ": too many rows")
  }
  
}


#Transform from a WIDE data sheet to a long data sheet
#Only need to report the mode for each RGB

mColor2 <- mColor %>% group_by(Label, Female, Year, EggNumber) %>% summarise(Mean=mean(Mode)) %>% spread(Label, Mean)
RGB <- as.matrix(rbind(mColor2$Red, mColor2$Green, mColor2$Blue))
HSL <- rgb2hsl(rgb=RGB)

mColor2$Hue <- HSL[1,]
mColor2$Saturation <- HSL[2,]
mColor2$Lightness <- HSL[3,]

#create a new masterfile that includes everything. 
write.csv(mColor2 %>% arrange(Female), "file:///C:/Users/11arc/Documents/Montogomerie Work/Eggs/Murre egg colors.csv", na="", row.names = F)
