library('sf')
library('spatial')
library('stats')
library('graphics')
library('raster')
library('rgdal')
library('sp')
library('ncdf4')
library('dplyr')
library('ggplot2')
library('plotly')
library('stringr')
library('gdata')
library('lubridate')
library('TSstudio')
library('rts')
library('PROJ')
library('dplyr')
library('VIM')
library('data.table')
library('ggmap')
library('DT')
library('factoextra') # clustering algorithms & visualization
library('tidyverse')  # data manipulation
library('cluster')    # clustering algorithms
library('TSstudio')

####SET WORKING DIRECTORY TO THE LOCATION OF THE CLIPPED RAINFALL FILES###
getwd()  
list.ras <- list.files(paste(getwd(), sep = ""), full.names = F, all.files = T, 
                       pattern = ".tif")
# read the rainfall files as a raster stack
rain <- stack(list.ras) 
# scale the rainfall values by multiplying with 0.1. Details for this are in documentation for GPM-IMERG 
rain_sc <- (rain * 0.1) 
plot(rain_sc,9:12)  #optional but you can plot them if you like


###CONVERT TO DATE-TIME##
#Date and time details are in the names of the rasters
raindate <- substr(names(rain_sc),27,42)
erikadate <-ymd_hms(raindate)
erikadates <- as_datetime(erikadate)
class(erikadates)#class should be date and time ("POSIXct" "POSIXt")


###DATA FRAME WITH COORDINATES###
#read the raster stack into a dataframe with coordinates
dfcord <- raster::as.data.frame(rain_sc,xy=TRUE)
#remove N/A values
row.has.na <- apply(dfcord , 1, function(x){any(is.na(x))})
dfcord.filtered <- dfcord [!row.has.na,]


###DATA FRAME WITHOUT COORDINATES###
# convert to a data.frame
precipitation <- as.data.frame(rain_sc)
# read column names as date-time
names(precipitation) <- erikadates 
plot(precipitation$`2015-08-28 03:30:00`) #optional but you can plot any of them if you as a check
#remove rows with N/A values
row.has.na <- apply(precipitation, 1, function(x){any(is.na(x))})
final.filtered <- precipitation[!row.has.na,] # data id now to extent of the buffer boundary
# rename the new data frame (with out N/A)
Tr <- final.filtered
plot(Tr$`2015-08-28 03:30:00`) #optional but you can plot any of them if you as a check


#########ELBOW METHOD CHOOSING OPTIMAL CLUSTERS (K)###############
set.seed(123)
# function to compute total within-cluster sum of square 
wss <- function(k) {
  kmeans(Tr, k, nstart = 10 )$tot.withinss
}
# Compute and plot wss for K = 1 to K = 15
k.values <- 1:15
#plot total within-cluster sum of square against the varying K values
wss_values <- map_dbl(k.values, wss)
plot(k.values, wss_values,
     type="b", pch = 19, frame = FALSE, 
     xlab="Number of clusters K",
     ylab="Total within-clusters sum of squares")
###Elbow method in one function
set.seed(123)
fviz_nbclust(Tr, kmeans, method = "wss") 


#########CLUSTERING USING EUCLIDEAN DISTANCE###############
set.seed(123)
#set K value  to 9
clusterK <- kmeans(Tr, centers = 9, nstart = 25)
# show details of the clustering output in the console
clusterK 
# check size of output clusters
clusterK$size
# cluster membership for each of the observations (rows)
clusterK$cluster 
# Cluster membership  for the first four observations
head(clusterK$cluster,4) 

# plot to show which cluster each pixel belongs, index x axis is row number
plot(Tr$`2015-08-28 06:30:00`, col = clusterK$cluster) 
#add the point classifications to the original data 
dd <- cbind(Tr, cluster = clusterK$cluster) #the point classifications are in now the last column of the dataframe dd

#####PLOT CLUSTERS##############
dd$x= dfcord.filtered$x ###add x coordinate
dd$y= dfcord.filtered$y ### add y coordinate
All_dd <- select(filter(dd, cluster >= 1), c(146,147,145))
dfr <- rasterFromXYZ(All_dd )
crs(dfr) <-  "+init=EPSG:4326"
dfr 
plot(dfr, main = "Clusters")

#####ADDING VECTOR FILES####
# set to the folder vector files
study <- readOGR(dsn=path.expand("C:/MGEO YEAR 2/MSC THESIS/DataAnalysisR/ClippedRainfall/StudyBuffer"), layer="Dominica_500km")
dominica <- readOGR(dsn=path.expand("C:/MGEO YEAR 2/MSC THESIS/DataAnalysisR/ClippedRainfall/StudyBuffer"), layer="Dominica")
airport <- readOGR(dsn=path.expand("C:/MGEO YEAR 2/MSC THESIS/DataAnalysisR/ClippedRainfall/StudyBuffer"), layer="Airports")
erika_track <- readOGR(dsn=path.expand("C:/MGEO YEAR 2/MSC THESIS/DataAnalysisR/ClippedRainfall/StudyBuffer"), layer="Erika_Study")
plot(study,add = TRUE)
plot(dominica,add = TRUE)
plot(airport,add = TRUE)
plot(erika_track,add = TRUE)

##AIRPORTS###
DC <- cellFromXY(dfr, c(-61.300759,15.546122)) #Douglas Charles Airport
DC
CF <- cellFromXY(dfr, c(-61.391948,15.336980)) #Cane Field Airport
CF

############SUBSETING THE DATAFRAME######
cluster1 <- dd[(dd$cluster == "1"),]
cluster2 <- dd[(dd$cluster == "2"),]
cluster3 <- dd[(dd$cluster == "3"),]
cluster4 <- dd[(dd$cluster == "4"),]
cluster5 <- dd[(dd$cluster == "5"),]
cluster6 <- dd[(dd$cluster == "6"),]
cluster7 <- dd[(dd$cluster == "7"),]
cluster8 <- dd[(dd$cluster == "8"),]
cluster9 <- dd[(dd$cluster == "9"),]

#########EXPLORING THE CLUSTERS############
#distance matrix between the rows of a data matrix
distance <- get_dist(cluster3) 
#visualizing a distance matrix
fviz_dist(distance, gradient = list(low = "#00AFBB", mid = "white", high = "#FC4E07"))

#computation of cluster statistics
cluster1[, "Q0"] <- apply(cluster1[, 1:144], 1,FUN=quantile, probs=0)
cluster1[, "Q1"] <- apply(cluster1[, 1:144], 1,FUN=quantile, probs=0.25)
cluster1[, "Q2"] <- apply(cluster1[, 1:144], 1,FUN=quantile, probs=0.5)
cluster1[, "Q3"] <- apply(cluster1[, 1:144], 1,FUN=quantile, probs=0.75)
cluster1[, "Q4"] <- apply(cluster1[, 1:144], 1,FUN=quantile, probs=0.9)
cluster1[, "Q5"] <- apply(cluster1[, 1:144], 1,FUN=quantile, probs=1)
cluster1[, "Mean"] <- apply(cluster1[, 1:144], 1, mean) #max, mean, min, 
cluster1[, "STD"] <- apply(cluster1[, 1:144], 1, sd) 
cluster1$rain_tot = rowSums(cluster1[1:144],c(-1))## total rainfall in a pixel

cluster2[, "Q0"] <- apply(cluster2[, 1:144], 1,FUN=quantile, probs=0)
cluster2[, "Q1"] <- apply(cluster2[, 1:144], 1,FUN=quantile, probs=0.25)
cluster2[, "Q2"] <- apply(cluster2[, 1:144], 1,FUN=quantile, probs=0.5)
cluster2[, "Q3"] <- apply(cluster2[, 1:144], 1,FUN=quantile, probs=0.75)
cluster2[, "Q4"] <- apply(cluster2[, 1:144], 1,FUN=quantile, probs=0.9)
cluster2[, "Q5"] <- apply(cluster2[, 1:144], 1,FUN=quantile, probs=1)
cluster2[, "Mean"] <- apply(cluster2[, 1:144], 1, mean) #max, mean, min, 
cluster2[, "STD"] <- apply(cluster2[, 1:144], 1, sd) 
cluster2$rain_tot = rowSums(cluster2[1:144],c(-1))## total rainfall in a pixel

cluster3[, "Q0"] <- apply(cluster3[, 1:144], 1,FUN=quantile, probs=0)
cluster3[, "Q1"] <- apply(cluster3[, 1:144], 1,FUN=quantile, probs=0.25)
cluster3[, "Q2"] <- apply(cluster3[, 1:144], 1,FUN=quantile, probs=0.5)
cluster3[, "Q3"] <- apply(cluster3[, 1:144], 1,FUN=quantile, probs=0.75)
cluster3[, "Q4"] <- apply(cluster3[, 1:144], 1,FUN=quantile, probs=0.9)
cluster3[, "Q5"] <- apply(cluster3[, 1:144], 1,FUN=quantile, probs=1)
cluster3[, "Mean"] <- apply(cluster3[, 1:144], 1, mean) #max, mean, min, 
cluster3[, "STD"] <- apply(cluster3[, 1:144], 1, sd) 
cluster3$rain_tot = rowSums(cluster3[1:144],c(-1))## total rainfall in a pixel

cluster4[, "Q0"] <- apply(cluster4[, 1:144], 1,FUN=quantile, probs=0)
cluster4[, "Q1"] <- apply(cluster4[, 1:144], 1,FUN=quantile, probs=0.25)
cluster4[, "Q2"] <- apply(cluster4[, 1:144], 1,FUN=quantile, probs=0.5)
cluster4[, "Q3"] <- apply(cluster4[, 1:144], 1,FUN=quantile, probs=0.75)
cluster4[, "Q4"] <- apply(cluster4[, 1:144], 1,FUN=quantile, probs=0.9)
cluster4[, "Q5"] <- apply(cluster4[, 1:144], 1,FUN=quantile, probs=1)
cluster4[, "Mean"] <- apply(cluster4[, 1:144], 1, mean) #max, mean, min, 
cluster4[, "STD"] <- apply(cluster4[, 1:144], 1, sd) 
cluster4$rain_tot = rowSums(cluster4[1:144],c(-1))## total rainfall in a pixel

cluster5[, "Q0"] <- apply(cluster5[, 1:144], 1,FUN=quantile, probs=0)
cluster5[, "Q1"] <- apply(cluster5[, 1:144], 1,FUN=quantile, probs=0.25)
cluster5[, "Q2"] <- apply(cluster5[, 1:144], 1,FUN=quantile, probs=0.5)
cluster5[, "Q3"] <- apply(cluster5[, 1:144], 1,FUN=quantile, probs=0.75)
cluster5[, "Q4"] <- apply(cluster5[, 1:144], 1,FUN=quantile, probs=0.9)
cluster5[, "Q5"] <- apply(cluster5[, 1:144], 1,FUN=quantile, probs=1)
cluster5[, "Mean"] <- apply(cluster5[, 1:144], 1, mean) #max, mean, min, 
cluster5[, "STD"] <- apply(cluster5[, 1:144], 1, sd) 
cluster5$rain_tot = rowSums(cluster5[1:144],c(-1))## total rainfall in a pixel

cluster6[, "Q0"] <- apply(cluster6[, 1:144], 1,FUN=quantile, probs=0)
cluster6[, "Q1"] <- apply(cluster6[, 1:144], 1,FUN=quantile, probs=0.25)
cluster6[, "Q2"] <- apply(cluster6[, 1:144], 1,FUN=quantile, probs=0.5)
cluster6[, "Q3"] <- apply(cluster6[, 1:144], 1,FUN=quantile, probs=0.75)
cluster6[, "Q4"] <- apply(cluster6[, 1:144], 1,FUN=quantile, probs=0.9)
cluster6[, "Q5"] <- apply(cluster6[, 1:144], 1,FUN=quantile, probs=1)
cluster6[, "Mean"] <- apply(cluster6[, 1:144], 1, mean) #max, mean, min, 
cluster6[, "STD"] <- apply(cluster6[, 1:144], 1, sd) 
cluster6$rain_tot = rowSums(cluster6[1:144],c(-1))## total rainfall in a pixel

cluster7[, "Q0"] <- apply(cluster7[, 1:144], 1,FUN=quantile, probs=0)
cluster7[, "Q1"] <- apply(cluster7[, 1:144], 1,FUN=quantile, probs=0.25)
cluster7[, "Q2"] <- apply(cluster7[, 1:144], 1,FUN=quantile, probs=0.5)
cluster7[, "Q3"] <- apply(cluster7[, 1:144], 1,FUN=quantile, probs=0.75)
cluster7[, "Q4"] <- apply(cluster7[, 1:144], 1,FUN=quantile, probs=0.9)
cluster7[, "Q5"] <- apply(cluster7[, 1:144], 1,FUN=quantile, probs=1)
cluster7[, "Mean"] <- apply(cluster7[, 1:144], 1, mean) #max, mean, min, 
cluster7[, "STD"] <- apply(cluster7[, 1:144], 1, sd) 
cluster7$rain_tot = rowSums(cluster7[1:144],c(-1))## total rainfall in a pixel

cluster8[, "Q0"] <- apply(cluster8[, 1:144], 1,FUN=quantile, probs=0)
cluster8[, "Q1"] <- apply(cluster8[, 1:144], 1,FUN=quantile, probs=0.25)
cluster8[, "Q2"] <- apply(cluster8[, 1:144], 1,FUN=quantile, probs=0.5)
cluster8[, "Q3"] <- apply(cluster8[, 1:144], 1,FUN=quantile, probs=0.75)
cluster8[, "Q4"] <- apply(cluster8[, 1:144], 1,FUN=quantile, probs=0.9)
cluster8[, "Q5"] <- apply(cluster8[, 1:144], 1,FUN=quantile, probs=1)
cluster8[, "Mean"] <- apply(cluster8[, 1:144], 1, mean) #max, mean, min, 
cluster8[, "STD"] <- apply(cluster8[, 1:144], 1, sd) 
cluster8$rain_tot = rowSums(cluster8[1:144],c(-1))## total rainfall in a pixel

cluster9[, "Q0"] <- apply(cluster9[, 1:144], 1,FUN=quantile, probs=0)
cluster9[, "Q1"] <- apply(cluster9[, 1:144], 1,FUN=quantile, probs=0.25)
cluster9[, "Q2"] <- apply(cluster9[, 1:144], 1,FUN=quantile, probs=0.5)
cluster9[, "Q3"] <- apply(cluster9[, 1:144], 1,FUN=quantile, probs=0.75)
cluster9[, "Q4"] <- apply(cluster9[, 1:144], 1,FUN=quantile, probs=0.9)
cluster9[, "Q5"] <- apply(cluster9[, 1:144], 1,FUN=quantile, probs=1)
cluster9[, "Mean"] <- apply(cluster9[, 1:144], 1, mean) #max, mean, min, 
cluster9[, "STD"] <- apply(cluster9[, 1:144], 1, sd) 
cluster9$rain_tot = rowSums(cluster9[1:144],c(-1))## total rainfall in a pixel

mean(cluster1$rain_tot)
mean(cluster2$rain_tot)
mean(cluster3$rain_tot)
mean(cluster4$rain_tot)
mean(cluster5$rain_tot)
mean(cluster6$rain_tot)
mean(cluster7$rain_tot)
mean(cluster8$rain_tot)
mean(cluster9$rain_tot)


#########PLOTTING CLUSTER TIME SERIES ############
######### Transpose dataframe and drop the column 'cluster'
c1_Trans <-as_tibble (t(cluster1[1:144]))
c1_date <- dplyr::as_data_frame(c1_Trans, rownames = "Date")
c1_date $Date <- erikadates
c1_date 
ts_plot(c1_date , line.mode = "lines", width = 2, dash = NULL, color = NULL , slider = FALSE,
        type = "single", Xtitle ="DATE",
        Ytitle = " PRECIPITATION (MM)", title = "CLUSTER 1",
        Xgrid = FALSE, Ygrid = FALSE)

c2_Trans <-as_tibble (t(cluster2[1:144]))
c2_date <- dplyr::as_data_frame(c2_Trans, rownames = "Date")
c2_date $Date <- erikadates
c2_date 
ts_plot(c2_date , line.mode = "lines", width = 2, dash = NULL, color = NULL , slider = FALSE,
        type = "single", Xtitle ="DATE",
        Ytitle = " PRECIPITATION (MM)", title = "CLUSTER 2",
        Xgrid = FALSE, Ygrid = FALSE)

c3_Trans <-as_tibble (t(cluster3[1:144]))
c3_date <- dplyr::as_data_frame(c3_Trans, rownames = "Date")
c3_date $Date <- erikadates
c3_date 
ts_plot(c3_date , line.mode = "lines", width = 2, dash = NULL, color = NULL , slider = FALSE,
        type = "single", Xtitle ="DATE",
        Ytitle = " PRECIPITATION (MM)", title = "CLUSTER 3",
        Xgrid = FALSE, Ygrid = FALSE)

c4_Trans <-as_tibble (t(cluster4[1:144]))
c4_date <- dplyr::as_data_frame(c4_Trans, rownames = "Date")
c4_date $Date <- erikadates
c4_date 
ts_plot(c4_date , line.mode = "lines", width = 2, dash = NULL, color = NULL , slider = FALSE,
        type = "single", Xtitle ="DATE",
        Ytitle = " PRECIPITATION (MM)", title = "CLUSTER 4",
        Xgrid = FALSE, Ygrid = FALSE)

c5_Trans <-as_tibble (t(cluster5[1:144]))
c5_date <- dplyr::as_data_frame(c5_Trans, rownames = "Date")
c5_date $Date <- erikadates
c5_date 
ts_plot(c5_date , line.mode = "lines", width = 2, dash = NULL, color = NULL , slider = FALSE,
        type = "single", Xtitle ="DATE",
        Ytitle = " PRECIPITATION (MM)", title = "CLUSTER 5",
        Xgrid = FALSE, Ygrid = FALSE)

c6_Trans <-as_tibble (t(cluster6[1:144]))
c6_date <- dplyr::as_data_frame(c6_Trans, rownames = "Date")
c6_date $Date <- erikadates
c6_date 
ts_plot(c6_date , line.mode = "lines", width = 2, dash = NULL, color = NULL , slider = FALSE,
        type = "single", Xtitle ="DATE",
        Ytitle = " PRECIPITATION (MM)", title = "CLUSTER 6",
        Xgrid = FALSE, Ygrid = FALSE)

c7_Trans <-as_tibble (t(cluster7[1:144]))
c7_date <- dplyr::as_data_frame(c7_Trans, rownames = "Date")
c7_date $Date <- erikadates
c7_date 
ts_plot(c7_date , line.mode = "lines", width = 2, dash = NULL, color = NULL , slider = FALSE,
        type = "single", Xtitle ="DATE",
        Ytitle = " PRECIPITATION (MM)", title = "CLUSTER 7",
        Xgrid = FALSE, Ygrid = FALSE)

c8_Trans <-as_tibble (t(cluster8[1:144]))
c8_date <- dplyr::as_data_frame(c8_Trans, rownames = "Date")
c8_date $Date <- erikadates
c8_date 
ts_plot(c8_date , line.mode = "lines", width = 2, dash = NULL, color = NULL , slider = FALSE,
        type = "single", Xtitle ="DATE",
        Ytitle = " PRECIPITATION (MM)", title = "CLUSTER 8",
        Xgrid = FALSE, Ygrid = FALSE)

c9_Trans <-as_tibble (t(cluster9[1:144]))
c9_date <- dplyr::as_data_frame(c9_Trans, rownames = "Date")
c9_date $Date <- erikadates
c9_date 
ts_plot(c9_date , line.mode = "lines", width = 2, dash = NULL, color = NULL , slider = FALSE,
        type = "single", Xtitle ="DATE",
        Ytitle = " PRECIPITATION (MM)", title = "CLUSTER 9",
        Xgrid = FALSE, Ygrid = FALSE)


###########EXPORTING OUTPUT##########
# set output folder to your choice
write.csv(cluster1[,145:156],"D:/Study/MSC THESIS/R_output/Spatial Clustering\\Cluster1.csv", row.names = FALSE)
writeRaster(dfr,"D:/Study/MSC THESIS/R_output/Spatial Clustering\\ClustersA.tif", row.names = FALSE)
write.csv(All_dd,"D:/Study/MSC THESIS/R_output/Spatial Clustering\\All_dd.csv", row.names = FALSE)
writeRaster(dfr,"D:/Study/MSC THESIS/R_output/Spatial Clustering\\Clusters9.tif", row.names = FALSE)
