
### Run this script and call k_means_cluster(foldername) and pass it the name of the folder containing all files datasets)
### Please remove all "*" characters from file using a curl command line before using this script.

######################################################################################################################
##          PROJECT LIBRARIES AND PACKAGES
##########################################################################

# NOTE: install packages if you don't have them, before loading
library(class)
library(factoextra)
library(dplyr)

######################################################################################################################



######################################################################################################################
##           Part 1:  CLUSTERING PROBLEM 
#########################################################################


##
## This function takes the folder name containing weather station datasets
## calls another function to clean and prepare the data for clustering
## perform a kmeans clustering algorithm on the data and provides summary of clusters
## and returns a dataframe containing cluster numbers for each station and a plot of the clusters
##
k_means_cluster<-function(folderName){
  
  files<-list.files(folderName) #get all the files from the dir folderName
  cleanedData <-prepareDatasets(files)
  originalData<- cleanedData #Just to keep this copy unchanged
  print(originalData)
  
  #remove row names
  rownames(cleanedData)<-NULL
  
  #scale (normalise) data to improve convergence for kmeans clustering algorithm
  cleanedData.scaled<-scale(cleanedData)
  
  #To determine the optimal number of clusters to use for kmeans, the fviz_nbclust function from factoextra library is used with the elbow method
  print("plot1- optimal cluster number")
  plot1<-fviz_nbclust(cleanedData.scaled, kmeans, method = "wss") + geom_vline(xintercept = 3, linetype = 2)
  
  
  #run the kmeans algorithm with optimum number of clusters = 3
  results<-kmeans(cleanedData.scaled,3, nstart = 10)
  print("K-means result")
  print(results) #print kmeasn results
  
  
  # computing the mean of each variable by clusters using the original data
    
  aggregate(originalData, by=list(cluster=results$cluster), mean)
  
  #add the cluster number for each weather station to the original dataframe
  stations <- cbind(originalData, cluster = results$cluster)
  head(stations)
  
  #cluster size
  print("cluster size")
  results$size
  
  # See Cluster number for each of the observations
  print("Cluster number for each of the observations")
  results$cluster
  
  # Cluster means
  print("Cluster means")
  results$centers
  
  #K-means clustering visualization 
  print("plot2: k-means visualization")
  plot2<-fviz_cluster(results, cleanedData, ellipse.type = "norm")
  
  #resurn original data with rownames and cluster numbers assigned, plot1, and plot2 in a list
  
  return_list<-list(stations,plot2)
  
  print("Original data with cluster numbers returned ")
  return(return_list)
  
  
}

##
## This function takes the list of files - weather station datasets
## then loops through the file, calling another function to clean the data
## then prepares the cleaned data and packages it into a dataframe that can be used by the k_means_clustering function
## returns a cleaned and well labelled dataframe containing all mean values for temperature and rainfall for each station
## 
prepareDatasets<-function(files){
  
  final_frame <- data.frame() #initialize an empty dataframe to take the final data frame
  rNames<-c()
  
  for (file in files){
    fileDir = paste("dataset1/",file,sep = "")
    fileData <- read.csv(fileDir, header = FALSE, sep="",dec=".") #read each file into a csv to ease cleaning
    
    cleanedData <- cleanData(fileData) #pass the dataset to be cleaned by the cleanDatafunction
    print(cleanedData)
    
    #take the cleaned dataframe, get the mean values for each column and add it to the final data frame to be used for clustering
    datasetName <- strsplit(file,".",fixed = TRUE)[[1]][1] #get the name of the station
    rNames<-c(rNames,datasetName) #create a ist of row names
    
    datasetName <- getMeanOfColumns(cleanedData)
    final_frame <- rbind(final_frame, datasetName )
    
    
    
  }
  print(final_frame)
  names(final_frame)<-c("mean_maxT_degC", "mean_minT_degC", "mean_rain_mm")
  
  rownames(final_frame)<-rNames
  final_frame
  
  #return the dataframe containing only the mean values for temp and rainfall for all stations
  
  return(final_frame)
  
}

##
## This function takes a dataframe and calculates the mean values for all numeric columns in the dataframe
## then returns a dataframe with only the mean values for each column
## 
getMeanOfColumns<-function(cleanDataFrame){
  
  cmeans<-colMeans(cleanDataFrame,na.rm=TRUE)
  
  #mean for temp and rainfall data for this dataset
  
  return(cmeans)
}

##
## This function takes in the raw dataset for each weather station
## then  removes all extra information not needed, removes rows with  estimated values in the dataset represented by "---"
## removes all unnecessary rows and columns, leaving only columns for temperature and rainfall, renamed
## converts all column values into numeric type
## and returns a dataframe with cleaned and consistent data to be used for further processing and analysis
##
cleanData<-function(stationData){
  
  # remove extra info on first 7 lines and remove airfrost and sunshine data, and month, year
  data_rightRowsAndColumns<-stationData[-c(1:7),c(3,4,6)] 
  
  #rename the columns. 
  names(data_rightRowsAndColumns)<-c("maxT_degC", "minT_degC", "rain_mm")
  
  
  #remove rows with missing records (denoted by ---)
  dataWithNoMissingValues <- data_rightRowsAndColumns[!(data_rightRowsAndColumns$maxT_degC =="---" |
                                                          data_rightRowsAndColumns$minT_degC =="---" |
                                                          data_rightRowsAndColumns$rain_mm =="---"), ]
  
  #view
  cat ("missing data removed " ,"\n")
  print(dataWithNoMissingValues)
  print("\n")
  
  
  #Make all columns numeric
  indx <- sapply(dataWithNoMissingValues, is.factor)
  dataWithNoMissingValues[indx] <- lapply(dataWithNoMissingValues[indx], function(x) as.numeric(as.character(x)))
  
  dataFrames<-na.omit(dataWithNoMissingValues)  #do final check and remove any NA values
  
  #view
  cat ("fully cleaned numeric columns " ,"\n")
  print(dataFrames)
  print("\n")
  
  #Estimated data will be used in this project. Therefore, the * has to be removed from it. This was already done manually in VScode
  
  return(dataFrames)
  
}
######################################################################################################################



######################################################################################################################
##  FUNCTION CALLS
##################

kmeans_output<-k_means_cluster("dataset1")
#contains the data frame clusters added and a PLOT of the clusters
kmeans_output 

#####################################################################################################################



#####################################################################################################################
##           Part 2:  CLASSIFICATION PROBLEM
#########################################################################



#Cleaned dataset was already returned in the clustering problem, so we just get the cleaned data from part 1
data_set<- kmeans_output[[1]]

# Determine the label for the region that each station false in, using the stations latittudes

lats<-c(52.139,54.352,55.181,53.813,57.006,50.218,52.245,51.488,51.089,52.358,56.451,54.768,50.762,55.311,51.479,
        50.779,60.139,56.377,52.483,51.346,57.593,54.670,51.761,55.846,53.356,51.911,52.794,53.381,50.898,58.214,52.833,
        56.500,53.252,53.175,54.481,58.454,51.006)

#southernly lat = 49.9
#northernly lat = 60.9
#middle lat = 55.4

#get the region that each station's latittude is closest to
region<-c()

for (i in lats) {
  
  #calculate difference between lats of different stations and pick region closest to its lat
  nx <- abs(i-49.9)
  mx<- abs(i-55.4)
  sx<- abs(i-60.9)
  
  smallest_x <- min(nx, mx, sx)
  if(smallest_x==nx){
    region<- c(region, "northern_third")
  }else if(smallest_x==mx){
    region<- c(region, "middle_third")
    
  }else{
    region<- c(region, "southern_third")
    
  }
  
}

############################################################
##      PREPARE DATA FOR USE WITH KNN() MODEL
###########################################################

#let's drop the cluster column and add the region column for the regions in which each station falls
stations.data<-data_set[ , c("mean_maxT_degC","mean_minT_degC","mean_rain_mm")]
stations.data$region <-region
stations.data


rownames(stations.data)<-NULL  ##remove rownames

#Normalise the numeric columns in the data to remove bais and transform data to similar scale
normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x))) }

stations.data.normalized <- as.data.frame(lapply(stations.data[1:3], normalize))

# Split dataset into training set (first 32 stations) and test set (last 5 stations)
train_set<-stations.data.normalized[1:32,]
train_set_labels<-stations.data[1:32,4]

test_set<-stations.data.normalized[33:37,]
test_set_labels<-stations.data[33:37,4]

# Just to check the proportions of each label (number of stations per region) in the entire dataset
round(prop.table(table(stations.data$region)) * 100, digits = 1) # 51.4%: north, 40.5%: middle and 8.1%: south

##################################################
##      TRAINING AND CLASSIFYING
#################################################

# Train a knn() model on the data. Use the euclidian distance between neighbors
# k (the number of neighbors to consider) is determined by taking the square root of the number of stations (37) -> k=6
stations.train6 <- knn(train = train_set, test = test_set,cl = train_set_labels, k=6)

# Also trying k=5 and k=7 to see which model would have a higher accuracy in classifying the stations
stations.train5 <- knn(train = train_set, test = test_set,cl = train_set_labels, k=5) 
stations.train7 <- knn(train = train_set, test = test_set,cl = train_set_labels, k=7)

# Evaluate each model by calculating the accuracy of the prediction -> k=6 or 7
acc6 <- 100*sum(test_set_labels==stations.train6)/NROW(test_set_labels) # accuracy = 60%
acc5 <- 100*sum(test_set_labels==stations.train5)/NROW(test_set_labels) # accuracy = 40%
acc7 <- 100*sum(test_set_labels==stations.train7)/NROW(test_set_labels) # accuracy = 60%

acc5
acc6
acc7
# view confusion matrix for various models. All are similar, so only one is shown below
table(stations.train6,test_set_labels)
######################################################################################################################


#####################################################################################################################
##        Part 3: WEATHER AND HAPPPINESS CORRELATION
#####################################################################################################################

# get the weather data used before and add the latitude to each station's
dataset_c<-data_set[,-4]
dataset_withLat<-dataset_c
dataset_withLat$latitude<-lats
dataset_withLat

#Load the census data with mean happiness ratings added
census.happiness<-read.csv("dataset2/censusdata.txt", header = FALSE, sep=",",dec=".")

# Get only the only the latitude and happiness ratings column
census.happiness<-census.happiness[,c(3,5)]
names(census.happiness)<-c("latitude","happiness-ratings")
census.happiness

census.area.latitude<- census.happiness$latitude
census.area.happiness<- census.happiness$`happiness-ratings`
census.area.latitude
census.area.happiness
# determine the happiness ratings for each station using a band of latitudes 
# each station is assigned the happiness of the area to each their latittudes are closest
stations.happiness<-c()

for (lat in lats) {
  dff1<-abs(lat-census.area.latitude[1])
  dff2<-abs(lat-census.area.latitude[2])
  dff3<-abs(lat-census.area.latitude[3])
  dff4<-abs(lat-census.area.latitude[4])
  dff5<-abs(lat-census.area.latitude[5])
  dff6<-abs(lat-census.area.latitude[6])
  dff7<-abs(lat-census.area.latitude[7])
  dff8<-abs(lat-census.area.latitude[8])
  dff9<-abs(lat-census.area.latitude[9])
  dff10<-abs(lat-census.area.latitude[10])
  dff11<-abs(lat-census.area.latitude[11])
  dff12<-abs(lat-census.area.latitude[12])
  
  diffs<-c(dff1,dff2,dff3,dff4,dff5,dff6,dff7,dff8,dff9,dff10,dff11,dff12)
  
  min_dff<-min(dff1,dff2,dff3,dff4,dff5,dff6,dff7,dff8,dff9,dff10,dff11,dff12)
  
  indx<-match(min_dff,diffs)
  hapi_r<-census.area.happiness[indx]
  
  stations.happiness<-c(stations.happiness,hapi_r)
  
  
}

stations.happiness

# add the happiness rations for each station to the main dataset
dataset_withLat$happiness<-stations.happiness

dataset2.ready<-dataset_withLat[,-4]
dataset2.ready


##############################################################
## CHECKING CORRELATION BTN WEATHER INSTRUMENTS AND HAPPINESS
##############################################################

# peek into the dataset to explore using a scatter plot

plot(dataset2.ready$happiness, dataset2.ready$mean_maxT_degC, main="meanMax Temperature vs Happinnes plot", 
     xlab="happiness ", ylab="meanMax Temperature (degC) ", pch=19)
plot(dataset2.ready$happiness, dataset2.ready$mean_minT_degC,  main="meanMin Temperature vs Happinnes plot", 
     xlab="happiness ", ylab="meanMin Temperature (degC) ", pch=19)
plot(dataset2.ready$happiness, dataset2.ready$mean_rain_mm, main="mean rainfall vs Happinnes plot", xlab="happiness ", 
     ylab="mean rainfall (mmHg) ", pch=19)

# There are no linear covariation between happiness and any of the weather instrument. There we cannot use pearson coeff

# using the Shapiro-Wilk normality test to check is the data is normally distributed
shapiro.test(dataset2.ready$happiness) #not normally distributed
shapiro.test(dataset2.ready$mean_maxT_degC)

# Therefore, using a rank-based measure of asscoiation i.e spearman rho
cor.test(dataset2.ready$mean_maxT_degC, dataset2.ready$happiness, method=c("spearman"))
cor.test(dataset2.ready$mean_minT_degC, dataset2.ready$happiness, method=c("spearman"))
cor.test(dataset2.ready$mean_rain_mm, dataset2.ready$happiness, method=c("spearman"))



#########################################################################################################################
##        THE END!
#########################################################################################################################