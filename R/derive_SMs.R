library(lidR)
library(raster)

#read all las files and put in 1 place
# file.copy(list.files("input/Regions", 
#            pattern=".las$", 
#            recursive=TRUE, 
#            full.names = TRUE), 
#           "input/Regions")

#read the las ctg
ctg<-readLAScatalog(list.files("input/las", full.names = TRUE, pattern="PR"))

#Create CHM and DTM
chm<-grid_canopy(ctg, 2, p2r())
dem<-grid_terrain(ctg, 2, knnidw(k = 6L, p = 2))

#Limit analysis to areas more than 1 m elevation
chm[dem<1]<-NA

#Topographic position index
TPI<-terrain(dem, option="TPI")

#Create a DF for anaylsis
df<-data.frame(DEM=getValues(dem),
           CHM = getValues(chm),
           TPI = getValues(TPI))

#Bin the TPI for visualization
df$TPI_bin<-floor(df$TPI*10)/10

#make a figure
library(ggplot2)
library(viridis)
ggplot(na.omit(df), aes(y=CHM, x=as.factor(TPI_bin), group=as.factor(TPI_bin)))+
  geom_boxplot()+
  # geom_hex(bins=1000)+
  # scale_fill_viridis(option="plasma")+
  theme_bw()+
  xlab('Topographic Position Index')+
  ylab('Canopy Height (m)')
  # stat_smooth()
  


