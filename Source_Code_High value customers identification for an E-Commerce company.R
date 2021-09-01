##### High value customers identification for an E-Commerce company #####

getwd()

setwd("C:/Users/intel/Desktop/Simplilearn")

e_commerce <- read.csv("Ecommerce.csv")

View(e_commerce)

e_commerce$X <- NULL ## removing column X - NA values

str(e_commerce)

summary(e_commerce)

e_commerce <- na.omit(e_commerce) ## removing na  values

e_commerce_data <- e_commerce[,c(4,6,7)]

View(e_commerce_data)

e_commerce_data <- scale(e_commerce_data)

View(e_commerce_data)

### Computing the kmeans clustering 

result <- kmeans(e_commerce_data,centers = 3, iter.max = 10)

str(result)

result$cluster

e_commerce_data$cluster <- result$cluster

View(e_commerce)

## compute the optimum cluster value 

wssplot <- function(data, nc , seed ){
  
  #wss <- (nrow (data) -1) * sum (apply(data,2,var))
  wss <- sum(kmeans(data,centers = 1)$withinss)
  #print(length(wss))
  
  #print(wss)
  for (i in 2:nc){
    set.seed(seed)
    wss[i] <- sum(kmeans(data,centers = i)$withinss)}
  #print(length(wss))
  #print(wss)
  #plot(wss)
  plot(1:nc,wss,type='b',xlab="number of clusters",ylab="within groups sum")
  
  
}


wssplot(e_commerce[,c(4,6,7)],16,123)

