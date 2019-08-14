library(ggplot2)
library(dplyr) # data munging
library(scales) # nicer axis scale labels
library(reshape2)
library(srtingi)
library(recommenderlab)
library(data.table)
library(png)
library(cowplot)
library(here) #just to set the working directory

here('ratings.csv') #just to set the working directory

ratings <- read.csv('ratings.csv')
ratings$rating <- round(ratings$rating)
ratings$rating <- as.factor(ratings$rating)

g <- ggplot(ratings, aes(x=1, fill=rating)) + 
  geom_bar() +
  coord_polar("y")+
  #scale_fill_manual(values = rating) +
  xlim(-0.5, 1.5)

img <-  readPNG("newfile.png")
ger <- grid::rasterGrob(img, interpolate=TRUE)
h <- ggdraw(g)
h + draw_grob(ger, 0.4, 0.5, 0.15, 0.07, scale = 2.4)

  
