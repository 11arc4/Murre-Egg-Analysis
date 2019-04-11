

#Check whether the two color balancing methods are equivalent. 
library(tidyverse)
library(colorspace)

dat <- read.csv("file:///C:/Users/11arc/OneDrive/Documents/Montogomerie Work/Eggs/Validation of color balancing/Color balancing Validation from Jaime/dataframes created/RGB.scoring_withandwithoutcolorchecker.csv")


dat2 <- dat%>% select("eggname", "mean.colorchecker.R.score", "mean.colorchecker.G.score", "mean.colorchecker.B.score", "mean.withoutcolorchecker.R.score", "mean.withoutcolorchecker.G.score", "mean.withoutcolorchecker.B.score")


RGB <- as.matrix(rbind(dat2$mean.colorchecker.R.score, dat2$mean.colorchecker.G.score, dat2$mean.colorchecker.B.score))
HSL <- plotwidgets::rgb2hsl(rgb=RGB)


RGB_without <- as.matrix(rbind(dat2$mean.withoutcolorchecker.R.score, dat2$mean.withoutcolorchecker.G.score, dat2$mean.withoutcolorchecker.B.score))
HSL_without <- plotwidgets::rgb2hsl(rgb=RGB_without)



dat3 <- data.frame(egg=dat2$eggname, 
                   H_with=HSL[1,],
                   S_with=HSL[2,],
                   L_with=HSL[3,],
                   H_wo=HSL_without[1,],
                   S_wo=HSL_without[2,],
                   L_wo=HSL_without[3,]
                   )




#Hue is sort of repeatable-- they are related on a log scale. 
ggplot(data=dat3, aes(x=H_with, y=H_wo))+
  geom_point()+
  geom_smooth(method="lm", formula=y~log(x))+
  geom_abline(slope=1, intercept = 0, color="blue")

mod_hue <- lm(H_wo ~ H_with, data=dat3)
mod_hue <- lm(H_wo ~ log(H_with), data=dat3)
summary(mod_hue)
plot(mod_hue)
#R2= 0.98

#Saturation is not particularly repeatable
ggplot(data=dat3, aes(x=S_with, y=S_wo))+
  geom_point()+
  geom_smooth(method="lm")+
  geom_abline(slope=1, intercept = 0, color="blue")

mod_sat <- lm(S_wo ~ S_with, data=dat3)
summary(mod_sat)
plot(mod_sat)
#R2= 0.62

#lightness is not particularly repeatable
ggplot(data=dat3, aes(x=L_with, y=L_wo))+
  geom_point()+
  geom_smooth(method="lm")+
  geom_abline(slope=1, intercept = 0, color="blue")

mod_light <- lm(L_wo ~ L_with, data=dat3)
summary(mod_light)
plot(mod_light)

#R2=0.51




#Conclude that only Hue is repeatable across the two methods, and therefore only Hue is particularly useful. 





