#Comparre the color and maculation of two photos of the same egg from different sides. 


library(tidyverse)



#Which maculation variables are repeatable across multiple angles of photographs?

duplicates_spot <- read.csv("~/Montogomerie Work/Eggs/Duplicate summarized murre egg pattern data.csv")
original <- read.csv("file:///C:/Users/11arc/OneDrive/Documents/Montogomerie Work/Eggs/Murre egg summarized color and pattern data.csv")



spots <- left_join(duplicates_spot, original, by=c("Year", "EggNumber", "Female"))


sumspots <- data.frame(Variable=rep(NA, 10), 
                       m=rep(NA, 10), 
                       sd =rep(NA, 10), 
                       p= rep(NA, 10), 
                       R2 =rep(NA, 10))

for(i in 1:9){
  ggplot(data=spots, aes_string(x=names(spots)[i+3] , y=names(spots)[i+12])) +
    geom_point()
  ggsave(paste("~/Montogomerie Work/Eggs/Plots_Murre/Repeatability based on egg rotation/", i,  names(spots)[i+3], ".jpeg", sep=""), 
         width=4, height=3, units = "in")
 
   mod <- lm(paste(names(spots)[i+12],   names(spots)[i+3], sep="~" ), data=spots)
  sumspots$Variable[i] <- names(spots)[i+3]
  sumspots$m[i] <- summary(mod)$coefficients[2,1]
  sumspots$sd[i] <- summary(mod)$coefficients[2,2]
  sumspots$p[i] <- summary(mod)$coefficients[2,4]
    sumspots$R2[i] <- summary(mod)[[8]]
  
  
}

write.csv(sumspots,"~/Montogomerie Work/Eggs/Repeatable variables across sides.csv", row.names=F, na="" )


###########################
duplicated_color <- read.csv("~/Montogomerie Work/Eggs/Duplicate summarized murre egg color data.csv")

ggplot(data=duplicated_color, aes(x=Hue , y=Hue.1)) +
  geom_point()
ggsave("~/Montogomerie Work/Eggs/Plots_Murre/Repeatability based on egg rotation/Hue.jpeg", 
       width=4, height=3, units = "in")


mod <- lm(Hue.1 ~ Hue.1, data=spots)
sumspots$Variable[10] <- "Hue"
sumspots$m[10] <- summary(mod)$coefficients[2,1]
sumspots$sd[10] <- summary(mod)$coefficients[2,2]
sumspots$p[10] <- summary(mod)$coefficients[2,4]
sumspots$R2[10] <- summary(mod)[[8]]




######################
#Make the duplicate_spot dataset

#Combine shape files for all eggs

#Set directory
dir <- "~/Montogomerie Work/Eggs/Raw Data_Murre/Maculation Repeatability Data"

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
                     EggNumber=rep(NA,r)
                  )


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
  #third <-  substring(strsplit(file, "_")[[1]][4],1,  nchar(strsplit(file, "_")[[1]][4])-4)
  
  shape$Female <- female
  shape$Year <- year
  shape$EggNumber <- eggNumber
  #shape$Third <- third
  
  
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

write.csv(mShape1, "~/Montogomerie Work/Eggs/Duplicate photos raw murre egg maculations.csv", na="", row.names = F)



#Tidy up
rm(mShape, shape, filenames, file, eggNumber, female, i, n, r, third, year, shape0, shape1.1)

#Calculate for whole egg
mShape <- mShape1 %>% 
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


mShape <- mShape1 %>% 
 # filter(PercArea>0.005)%>%
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


write.csv(mShape,"~/Montogomerie Work/Eggs/Duplicate summarized murre egg pattern data.csv", row.names=F, na="" )




###############################################
MICA <- read.csv("file:///C:/Users/11arc/OneDrive/Pictures/RAWmurreeggs/Image Analysis Results None.csv", as.is = T)

MICA$Rep <- substring(MICA$Label, 1,2)
MICA$Label2 <- ifelse(MICA$Rep=="2_", substring(MICA$Label, 3, nchar(MICA$Label)), MICA$Label)


MICA2 <- MICA %>% 
  separate(col=Label2, into=c("Female", "EggNumber", "Year", "Patch"), sep="_") %>%
  select(-Patch, -Label) %>%
  group_by(Female, EggNumber, Year, Rep) %>%
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


MICA3 <- MICA2 %>% 
  select(c(Female, EggNumber, Year, Rep, Hue, Saturation, Lightness)) %>% 
  mutate(Rep=ifelse(Rep=="2_", 2, 1)) %>% 
 arrange(Rep)

duplicated_color <- data.frame(MICA3[1:8,],
      MICA3[9:16,5:7])

write.csv(duplicated_color,"~/Montogomerie Work/Eggs/Duplicate summarized murre egg color data.csv", row.names=F, na="" )



