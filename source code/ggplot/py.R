library("ggmap")
library("animation")



tables <- read.table("el.csv",header=T, sep=",")

data <- tables[c('scale','lan','lon')]

names(data) <- c('scale','lan','lon')

data$lan <- as.numeric(data$lan)
data$lon <- as.numeric(data$lon)

#data$date <- as.Date(data$date,"%y-%m-%d")

data$scale <- as.numeric(data$scale)

plotfunc <- function(x) {
    df <- subset(data,scale <= x)
    df$lan <- as.numeric(df$lan)
    df$lon <- as.numeric(df$lon)
    p <- ggmap(get_googlemap(center = 'taiwan', zoom=8,maptype='terrain'),,extent='device')+
        geom_point(data=df,aes(x=lon,y=lan),colour = 'red',alpha=0.7)
}


time <- sort(unique(data$scale))

dir.create("examples")
setwd("examples")

png(file="example%02d.png", width=600, height=800)
  for( i in time) print(plotfunc(i))
  
dev.off()

#ani.options(convert = "C:/Program Files/ImageMagick-6.9.2-Q16/convert.exe")
#saveGIF({
#for( i in time) print(plotfunc(i))
#})

#system("convert -delay 80 *.png example_1.gif")

setwd("C:/Users/L/Documents")