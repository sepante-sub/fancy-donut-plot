library(ggplot2)
library(dplyr) # data munging
library(scales) # nicer axis scale labels
library(reshape2)
library(srtingi)
library(recommenderlab)
library(data.table)
library(png)
library(cowplot)
setwd("~/Downloads/ml-latest-small")


movies <- read.csv('movies.csv')
ratings <- read.csv('ratings.csv')

head(movies)
head(ratings)
#drop <- c("timestamp")
#ratings <- ratings[,!(names(ratings) %in% drop)]

ratings$rating <- round(ratings$rating)

ratings$rating <- as.factor(ratings$rating)


head(ratings)

#choice 1
ggplot(ratings, aes(x=rating, fill=rating)) + 
  geom_bar(width = 1) +
  coord_polar("y", start=0) #+
  #scale_y_log10()   + labs(y="Logarithmic Scale of Counts") 
  
#choice 2
g <- ggplot(ratings, aes(x=1, fill=rating)) + 
  geom_bar() +
  coord_polar("y")+
  #scale_fill_manual(values = rating) +
  xlim(-0.5, 1.5)

img <-  readPNG("newfile.png")
ger <- grid::rasterGrob(img, interpolate=TRUE)
h <- ggdraw(g)
h + draw_grob(ger, 0.4, 0.5, 0.15, 0.07, scale = 2.4)

  

ggplot(ratings, aes(x = 2, fill = rating)) +
  #geom_bar(width = 1, stat = "identity", color = "white") +
  geom_bar()+
  coord_polar("y", start = 0)+
  #geom_text(aes(y = lab.ypos, label = prop), color = "white")+
  #scale_fill_manual(values = mycols) +
  theme_void()

  
  
ggplot(ratings, aes(x = factor(1), y=FR,fill=rating) ) 
+ geom_bar(width = 1,stat="identity")+coord_polar(theta = "y") 
  
  
ggplot(subset(ratings), aes(x=movieId, y=rating))


two_people <- subset(ratings,userId < 20 && userId > 18)

ggplot(two_people, aes(x=timestamp, y=rating)) + geom_point(aes(color=userId))


ratingmat <- dcast(ratings, userId~movieId, value.var = "rating", na.rm = False)
ratingmat <- as.matrix( ratingmat[,-1] )
ratingmat <- as(ratingmat, "realRatingMatrix")

ratingmat <- normalize(ratingmat)

rec_mod = Recommender(ratingmat, method = "UBCF", param = list(method="Cosine", nn = 10))
"""

top5 = predict(rec_mod, ratingmat[2], n = 5)
top5 = as(top5, "list")

top5_vector = as.integer(unlist(top5))
print(top5_vector)



top5_all = predict(rec_mod, ratingmat, n = 5)
top5_all = as(top5_all, "list")
top5_all_vector <- as.integer (unlist(top5_all))


ggplot(subset(ratings), aes(x=movieId, y=rating))

movies$movieId <- as.factor(movies$movieId)

topmovies <- subset(movies, movieId %in% top5_vector)
View( topmovies$title )

top5_df <- as.data.frame(table(top5_all_vector))


top5_df <- top5_df[order(top5_df$Freq,decreasing = TRUE),]

head(top5_df)
top5_df$ID <- seq.int(nrow(top5_df))


top_overall_movies <- subset(movies, movieId %in% top5_df[1:5,]$top5_all_vector)
"""

movgen <- as.data.frame( movies$genres, stringsAsFactors = False)

movgen2 <- as.data.frame( tstrsplit(movgen[,1], '[|]', type.convert = TRUE), stringsAsFactors = FALSE )

colnames(movgen2) <- c(1:7)
View(movgen2)

names(movgen_list) <- c ("Action", "Adventure", "Animation", "Children", "Comedy", "Crime", "Documentary", "Drama", "Fantasy", "Film-Noir", "Horror", "Musical", "Mystery", "Romance", "Sci-Fi", "Thriller", "War", "Western")


movgen_matrix <- as.data.frame (matrix(0, nrow = dim(movgen2)[1], ncol = 18))
colnames(movgen_matrix) <- movgen_list
View(movgen_matrix)

for (i in 1:nrow(movgen2)) {
  #print(i)
  for (c in 1:ncol(movgen2)) {
    #genmat_col = which(movgen_matrix[1,] == movgen2[i,c])
    genmat_col = which(names(movgen_matrix[1,]) == movgen2[i,c])
    movgen_matrix[i+1,genmat_col] <- 1
  }
}

View(movgen_matrix)



binary_ratings <- ratings
binary_ratings$rating <- as.numeric(binary_ratings$rating)

for (i in 1:nrow(binary_ratings)){
  #print(binary_ratings[i,3])
  if ( binary_ratings[i,3] > 3){
    binary_ratings[i,3] <- 1
  }
  else{
    binary_ratings[i,3] <- -1
  }
}


drop <- c("timestamp", "movieIds")
binary_ratings <- binary_ratings[,!(names(ratings) %in% drop)]
View(binary_ratings)



binary_ratings2 <- dcast(binary_ratings, movieId~userId, value.var = "rating", na.rm=FALSE)
for (i in 1:ncol(binary_ratings2)){
  binary_ratings2[which(is.na(binary_ratings2[,i]) == TRUE),i] <- 0
}
binary_ratings2 = binary_ratings2[,-1] #remove movieIds col. Rows are movieIds, cols are userIds
View(binary_ratings2)

unique_movieIds <- unique(movies$movieId) #9125
unique_ratings <- unique(ratings$movieId) #9066
movies2 <- movies[which((unique_movieIds %in% unique_ratings) == TRUE),]
movies2 <- movies[which((unique_ratings %in% unique_movieIds) == TRUE),]
rownames(movies2) <- NULL

movgen_matrix2 <- as.data.frame(movgen_matrix[-1,], stringsAsFactors=FALSE) #remove first row, which was the genre list
for (c in 1:ncol(movgen_matrix2)) {
  movgen_matrix2[,c] <- as.integer(movgen_matrix2[,c])
} #convert from characters to integers


#Remove rows that are not rated from movgen_matrix2
movgen_matrix3 <- movgen_matrix2[-which((unique_movieIds %in% unique_ratings) == FALSE),]
rownames(movgen_matrix3) <- NULL
movgen_matrix3 <- movgen_matrix3[-9725,]
View(movgen_matrix3)

mat1 <- as.matrix(movgen_matrix3)
mat2 <- as.matrix(binary_ratings2) 

View(result)

result <- t(mat1) %*% mat2
#result <- t(result)

result[result < 0 ] <- 0
result[result > 0 ] <- 1
View(result)

