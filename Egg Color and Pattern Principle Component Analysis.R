library(stats)
library(corrplot)
library(cowplot)
#devtools::install_github("cmartin/ggConvexHull")
library(ggConvexHull)
library(tidyverse)

#Principle component analysis of egg maculation and color

#Read in all the processed egg data. 
#Right now we have some missing values so remove those
egg <- read.csv("file:///C:/Users/11arc/Documents/Montogomerie Work/Eggs/Murre egg summarized color and pattern data.csv", as.is=T)



dats <- list(TopEgg=egg[,c(1:5, seq(10, 24, 3), 31 )], 
             MiddleEgg=egg[,c(1:5, seq(11, 24, 3), 32 ) ],
             BottomEgg=egg[,c(1:5, seq(12, 24, 3), 33 )], 
             WholeEgg=egg[,c(1:5, seq(12, 24, 3), 33 )])


#Roundness wasn't repeatable, so we won't use it in the PCA
loadingsAll <- list(Top=NA, Middle=NA, Bottom=NA, Whole=NA)
plots_color <- list(Top=NA, Middle=NA, Bottom=NA, Whole=NA)
PCAsummaries <- list(Top=NA, Middle=NA, Bottom=NA, Whole=NA)

#Files for ONLY shape (no Hue or Saturation values included)
loadingsAll_shape <- list(Top=NA, Middle=NA, Bottom=NA, Whole=NA)
plots_shape <- list(Top=NA, Middle=NA, Bottom=NA, Whole=NA)
PCAsummaries_shape <- list(Top=NA, Middle=NA, Bottom=NA, Whole=NA)

distPlots <- list(Top=NA, Middle=NA, Bottom=NA, Whole=NA)


PlotTitles <- c("Top", "Middle", "Bottom", "Whole")

for (i in 1:length(dats)){
  dat <- dats[[i]]
  
  ######PCA USING COLOR AND SHAPE VARIABLES
  PCA <- prcomp( dat[4:11], 
                 center=T, 
                 scale=T, 
                 retx=T)
  
  #plot(TopEggPCA, type="lines")
  ncomp<-2
  rawLoadings     <- PCA$rotation[,1:ncomp] %*% diag(PCA$sdev, ncomp, ncomp)
  rotatedLoadings <- varimax(rawLoadings)$loadings
  invLoadings     <- t(pracma::pinv(rotatedLoadings))
  rownames(invLoadings) <- colnames(dat[4:11])
  
  loadingsAll[[i]] <- invLoadings
  PCAsummaries[[i]] <- rotatedLoadings
  scores <- scale(dat[4:11]) %*% invLoadings
  
  ####Calculate PCs for Top of Egg
  dat$PC1 <-  scores[,1]
  dat$PC2 <-  scores[,2]
  
  #calculate euclidean distance between all points
  #dist(dat[, 12:13]) #euclidean is the default but therea are lots of other options. 
  
  
  ########PCA USING ONLY SHAPE VARIABLES
  PCA2 <- prcomp( dat[6:11], 
                  center=T, 
                  scale=T, 
                  retx=T)
  
  #plot(PCA1, type="lines")
  ncomp<-2
  rawLoadings     <- PCA2$rotation[,1:ncomp] %*% diag(PCA2$sdev, ncomp, ncomp)
  rotatedLoadings <- varimax(rawLoadings)$loadings
  invLoadings     <- t(pracma::pinv(rotatedLoadings))
  rownames(invLoadings) <- colnames(dat[6:11])
  scores          <- scale(dat[6:11]) %*% invLoadings
  
  loadingsAll_shape[[i]] <- invLoadings
  PCAsummaries_shape[[i]] <- rotatedLoadings
  scores_shape <- scale(dat[6:11]) %*% invLoadings
  
  dat$PC1_2 <-  scores[,1]
  dat$PC2_2 <-  scores[,2]
  
  
  ######MAKE PLOTS 
  centroids <- dat %>% 
    group_by(Female) %>% 
    filter(n()>2)%>%
    summarise(PC1 = mean(PC1), 
              PC2 = mean(PC2), 
              PC1_2=mean(PC1_2), 
              PC2_2= mean(PC1_2))
  #BASED ON COLOR AND SHAPE PCA#Calculate centroids of all females with more than 2 eggs measured
  V <- ggplot()+
    geom_point(data=dat, aes(x=PC1, y=PC2, color=Female), show.legend = F)+
    #geom_point(data=centroids, aes(x=PC1, y=PC2, color=Female), shape=4, show.legend = F ,size=2)+ #add centroid as an x
    theme_classic()+
    geom_convexhull(data=dat, aes(x=PC1, y=PC2, fill=Female), alpha=0.2)+
    ggtitle(paste(PlotTitles[i]," of egg (shape and color)"))
  plots_color[[i]] <- V
  
  #BASED ON SHAPE ONLY PCA
  s <- ggplot()+
    geom_point(data=dat, aes(x=PC1_2, y=PC2_2, color=Female), show.legend = F)+
    #geom_point(data=centroids, aes(x=PC1_2, y=PC2_2, color=Female), shape=4, show.legend = F ,size=2)+ #add centroid as an x
    theme_classic()+
    geom_convexhull(data=dat, aes(x=PC1_2, y=PC2_2, fill=Female), alpha=0.2)+
    ggtitle(paste(PlotTitles[i]," of egg (shape only)"))+
    labs(x="PC1", y="PC2")
  plots_shape[[i]] <- s
  
  ################EUCLIDEAN DISTANCES WITHIN VS BETWEEN FEMALES
  dat2 <-  dat[,c(1,2,3,12:15)] %>% mutate(k = 1)
  
  datDist <- dat2 %>% 
    full_join(dat2, by = "k")%>% 
    mutate(dist = sqrt((PC1.x - PC1.y)^2 + (PC2.x - PC2.y)^2),
           dist_2 = sqrt((PC1_2.x - PC1_2.y)^2 + (PC2_2.x - PC2_2.y)^2)) %>%
    select(-k) %>% 
    group_by(Female.x) %>% 
    summarise(WithinDist= mean(dist[Female.x == Female.y & (EggNumber.x !=EggNumber.y | Year.x !=Year.y)]), 
              BetweenDist=mean(dist[Female.x !=Female.y]), 
              WithinDist_2= mean(dist_2[Female.x == Female.y & (EggNumber.x !=EggNumber.y | Year.x !=Year.y)]), 
              BetweenDist_2=mean(dist_2[Female.x !=Female.y])) %>%
    mutate(Ratio_both=WithinDist/BetweenDist, 
           Ratio_shape=WithinDist_2/BetweenDist_2) %>%
    gather_(key="Method", value="Dist", c("Ratio_both", "Ratio_shape"))
  R <- ggplot(datDist, aes(x=Method, y=Dist))+
    geom_boxplot()+
    geom_jitter()+
    labs(y="Distance within/amoung females")+
    ggtitle(paste(PlotTitles[i], " of egg"))
  distPlots[[i]] <- R
  
}


cowplot::plot_grid(plotlist=plots_color, ncol=2, nrow=2)
ggsave("~/Montogomerie Work/Eggs/Egg rotated PCA plot_noroundshapeandcolor.jpeg", device="jpeg", width=6, height=5, unit="in")


cowplot::plot_grid(plotlist=plots_shape, ncol=2, nrow=2)
ggsave("~/Montogomerie Work/Eggs/Egg rotated PCA plot_noroundnocolor.jpeg", device="jpeg", width=6, height=5, unit="in")

cowplot::plot_grid(plotlist=distPlots, ncol=2, nrow=2)
ggsave("~/Montogomerie Work/Eggs/Ratio plots_noround.jpeg", device="jpeg", width=6, height=5, unit="in")


