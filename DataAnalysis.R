library(stats)
library(BBmisc)

hourlyDF <- read.csv("hourlyDF.csv", header=TRUE, row.names = 1) #, as.is=TRUE, na.strings="-1")
hourlyDF_org <- read.csv("hourlyDF.csv", header=TRUE, row.names = 1) #, as.is=TRUE, na.strings="-1")
sum(is.na(hourlyDF))

######### For Teacher's Tool ############
#hourlyDF$date <- NULL
#hourlyDF <- apply(hourlyDF,2,as.integer)
#write.csv2(t(as.matrix(hourlyDF)),"todos.csv", col.names = FALSE, row.names = FALSE)


################# GRAPHICS ####################
for (col in 2:length(hourlyDF)){
  #To save the images:
  #png(file=paste("ts",colnames(hourlyDF)[col],".png", sep=""),width=1200, height=500)

  plot(ts(hourlyDF[col], start = c(2015,1),frequency = 365), 
       main = paste(colnames(hourlyDF)[col], 'Mean Daily Energy Consumption'),
       xlab='Years', ylab ='MW')
  
  #dev.off()
}

################ NORMALIZE DATA ################
dateCol <- hourlyDF$date
hourlyDF$date <- NULL
hourlyDFnormalized <- as.data.frame(normalize(hourlyDF, method="range", range=c(0,1), margin=2))

# To check if the normalized data is equal to the original data
# for (col in 2:length(hourlyDFnormalized)){
#   plot(ts(hourlyDF[col], start = c(2015,1),frequency = 365),
#        main = paste(colnames(hourlyDF)[col], 'Mean Daily Energy Consumption'),
#        xlab='Years', ylab ='MW')
#   plot(ts(hourlyDFnormalized[col], start = c(2015,1),frequency = 365),
#        main = paste(colnames(hourlyDFnormalized)[col], 'Mean Daily Energy Consumption'),
#        xlab='Years', ylab ='MW')
# }


############### FOURIER TRANSFORM ##############
# We have seen with the teacher's tool that the 50 coefficients is sufficient
coefFourier <- apply(hourlyDFnormalized,2,function(x) fft(as.numeric(x)))
coefFourier <- as.data.frame(coefFourier[1:50,])
# We split the complex number in to real and imaginary part to apply clustering techniques
coefFourier <- apply(coefFourier,2,function(x) c(Re(x),Im(x)))
coefFourier <- as.data.frame(t(as.matrix(coefFourier)))

################# TASKS ####################
# Cluster the TS to see similarities among countries
# NUMBER OF CLUSTERS
# Elbow method: within-cluster sum of square (WSS)
library(NbClust)
library(factoextra)
fviz_nbclust(coefFourier, kmeans, method = "wss") +
  geom_vline(xintercept = 6, linetype = 2)+
  labs(subtitle = "Elbow method")
nClusters <- 6

#k-means is very simple; you need to call the kmeans() function and specify
#the number of clusters. As usual, it is best to scale the features first.
set.seed(1)
clusters <- kmeans(coefFourier, nClusters)
groups <- as.data.frame(clusters[1])
centers <- as.data.frame(clusters[2])


ts.plot(hourlyDFnormalized[,rownames(subset(groups, subset=cluster==2))], gpars = list(col = c("black", "red")))
ts.plot(hourlyDFnormalized[,rownames(subset(groups, subset=cluster==3))], gpars = list(col = c("black", "red")))
ts.plot(hourlyDFnormalized[,rownames(subset(groups, subset=cluster==4))], gpars = list(col = c("black", "red", "blue", "green")))
ts.plot(hourlyDFnormalized[,rownames(subset(groups, subset=cluster==5))], gpars = list(col = c("black", "red", "blue")))



# Select a representative TS of each cluster and compare those TS
# Function to given the center of the cluster and the ts of that cluster, identify which is the nearest one
# Euclidean distance
for (i in 1:nClusters){
  print(i)
  print(apply(coefFourier[rownames(subset(groups, subset=cluster==i)),],
              1, function(x) sqrt(sum((x - centers[i,]) ^ 2))))
}


rownames(hourlyDF_org) <- hourlyDF_org$date
representative = hourlyDF_org[,c("NO", "HR", "LV", "PL", "ES","FI")]  # returns a data.frame


countries <- names(representative)
counter = 1
for (country in countries){
  ts = ts(representative[country], frequency = 365.25, start = c(2015,1))
  #plot.ts(ts, main = country)
  dec <- decompose(ts, "additive")
  plot(dec, xlab = paste("Cluster ", as.character(counter), "(",  country, ")"))
  counter=counter+1
}


