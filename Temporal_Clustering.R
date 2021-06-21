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
library('plyr')
library('VIM')
library('data.table')
library('ggmap')
library('DT')
library('factoextra') # clustering algorithms & visualization
library('tidyverse')  # data manipulation
library('cluster')    # clustering algorithms
library('matrixStats') # matrix statistics
library('pracma') # find peaks
library('spatstat') # reversing CUMULATIVE sum

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
##calculate statistics for each row (pixel)
Tr[, "Q0"] <- apply(Tr[, 1:144], 1,FUN=quantile, probs=0)
Tr[, "Q1"] <- apply(Tr[, 1:144], 1,FUN=quantile, probs=0.25)
Tr[, "Q2"] <- apply(Tr[, 1:144], 1,FUN=quantile, probs=0.5)
Tr[, "Q3"] <- apply(Tr[, 1:144], 1,FUN=quantile, probs=0.75)
Tr[, "Q4"] <- apply(Tr[, 1:144], 1,FUN=quantile, probs=0.9)
Tr[, "Q5"] <- apply(Tr[, 1:144], 1,FUN=quantile, probs=1)
Tr[, "Mean"] <- apply(Tr[, 1:144], 1, mean) #max, mean, min, 
Tr[, "STD"] <- apply(Tr[, 1:144], 1, sd) 
Tr$rain_tot = rowSums(Tr[1:144],c(-1))## total rainfall in a pixel
Tr$x= dfcord.filtered$x #add x coordinate
Tr$y= dfcord.filtered$y #add y coordinate

#only select statistics to be used for temporal clustering 
#x and y included in Tr1 only for plotting purposes
Tr1 <- select(filter(Tr, Q2 == 0 ), c(148:155)) 
# statistics used for temporal clustering are scaled
Tr2 <- scale(Tr1[1:6])


#########ELBOW METHOD CHOOSSING K###############
set.seed(123)
# function to compute total within-cluster sum of square 
wss <- function(k) {
  kmeans(Tr2, k, nstart = 10 )$tot.withinss
}
# Compute and plot wss for k = 1 to k = 15
k.values <- 1:15
#plot total within-cluster sum of square against the varying K values
wss_values <- map_dbl(k.values, wss)
plot(k.values, wss_values,
     type="b", pch = 19, frame = FALSE, 
     xlab="Number of clusters K",
     ylab="Total within-clusters sum of squares")
###Elbow method in one function
set.seed(123)
fviz_nbclust(Tr2, kmeans, method = "wss") 



#########CLUSTERING USING EUCLIDEAN DISTANCE###############
set.seed(123)
#set K value  to 5
clusterK <- kmeans(Tr2, centers = 5, nstart = 25)
# show details of the clustering output in the console
clusterK 
# check size of output clusters
clusterK$size
# cluster membership for each of the observations (rows)
clusterK$cluster 
# Cluster membership  for the first four observations
head(clusterK$cluster,4) 
#visualization of the clusters 
fviz_cluster(clusterK, geom = "point",  data = Tr2) + ggtitle("k = 5")
#add the point classifications to the original data 
dd <- as.data.frame(cbind(Tr2, cluster = clusterK$cluster)) #the point classifications are in now the last column of the dataframe dd
Tr1$cluster= dd$cluster
Tr$cluster= Tr1$cluster

#####PLOT CLUSTERS##############

#plot cluster distribution
All_dd <- select(filter(Tr1, cluster >= 1), c(7,8,9))
dfr <- rasterFromXYZ(All_dd )
crs(dfr) <-  "+init=EPSG:4326"
plot(dfr, main = "Clusters")
dfr 
writeRaster(dfr,"D:/Study/MSC THESIS/R_output/Temporal Clustering\\dfr.tif", row.names = FALSE)

#add vector files 
study <- readOGR(dsn=path.expand("D:/Study/MSC THESIS/DataAnalysisR/ClippedRainfall/StudyBuffer"), layer="Dominica_500km")
dominica <- readOGR(dsn=path.expand("D:/Study/MSC THESIS/DataAnalysisR/ClippedRainfall/StudyBuffer"), layer="Dominica")
airport <- readOGR(dsn=path.expand("D:/Study/MSC THESIS/DataAnalysisR/ClippedRainfall/StudyBuffer"), layer="Airports")
erika_track <- readOGR(dsn=path.expand("D:/Study/MSC THESIS/DataAnalysisR/ClippedRainfall/TC Tracks"), layer="Erika_Study")
plot(study,add = TRUE)
plot(dominica,add = TRUE)
plot(airport,add = TRUE)
plot(erika_track,add = TRUE)

#plot distribution of pixel cumulative rainfall
All_total <- select(filter(Tr1, cluster >= 1), c(7,8,6)) 
dfr_total <- rasterFromXYZ(All_total )
crs(dfr_total) <-  "+init=EPSG:4326"
plot(dfr_total, main = "Total rain")
writeRaster(dfr_total,"D:/Study/MSC THESIS/R_output/Temporal Clustering\\dfr_total.tif", row.names = FALSE)

#plot distribution of pixel maximum rainfall intensity
All_intensity <- select(filter(Tr1, cluster >= 1), c(7,8,3)) 
dfr_intensity <- rasterFromXYZ(All_intensity )
crs(dfr_intensity ) <-  "+init=EPSG:4326"
plot(dfr_intensity , main = "Maximum Intensity")
writeRaster(dfr_intensity,"D:/Study/MSC THESIS/R_output/Temporal Clustering\\dfr_intensity.tif", row.names = FALSE)

############SUBSETING THE DATAFRAME######
cluster1 <- Tr[(Tr$cluster == "1"),]
cluster2 <- Tr[(Tr$cluster == "2"),]
cluster3 <- Tr[(Tr$cluster == "3"),]
cluster4 <- Tr[(Tr$cluster == "4"),]
cluster5 <- Tr[(Tr$cluster == "5"),]


distance <- get_dist(cluster2[1:144]) #distance matrix between the rows of a data matrix
fviz_dist(distance, gradient = list(low = "#00AFBB", mid = "white", high = "#FC4E07"))#visualizing a distance matrix
boxplot(cluster1$rain_tot)

#########EXPLORING THE CLUSTERS############
#distance matrix between the rows of a data matrix
distance <- get_dist(cluster1[1:144])
#visualizing a distance matrix
fviz_dist(distance, gradient = list(low = "#00AFBB", mid = "white", high = "#FC4E07"))
boxplot(cluster1$rain_tot) #box plot

#########AIRPORTS###############
DC <- cellFromXY(dfr, c(-61.300759,15.546122)) #Douglas Charles Airport C5
DC
CF <- cellFromXY(dfr, c(-61.391948,15.336980)) #Cane Field Airport C3
CF

#########PLOTTING CLUSTER TIME SERIES ############
##CLUSTER 1##
# Transpose data frame and drop the column 'cluster'
c1_trans <-as_tibble (t(cluster1[1:144]))
c1_date <- dplyr::as_data_frame(c1_trans, rownames = "Date")
c1_date $Date <- erikadates
ts_plot(c1_date , line.mode = "lines", width = 2, dash = NULL , slider = FALSE,
        type = "single", Xtitle ="DATE AND TIME",
        Ytitle = " PRECIPITATION (MM)", title = "CLUSTER 1",
        Xgrid = FALSE, Ygrid = FALSE)
cumm1 <- data.frame(c1_trans, c=cumsum(c1_trans[1:132]))
c1_cumm <- cumm1[,-c(1:132)] 
c1_cumm_date <- dplyr::as_data_frame(c1_cumm, rownames = "Date")
c1_cumm_date $Date <- erikadates
ts_plot(c1_cumm_date , line.mode = "lines", width = 2, dash = NULL , slider = FALSE,
        type = "single", Xtitle ="DATE AND TIME",
        Ytitle = " CUMULATIVE PRECIPITATION (MM)", title = "CLUSTER 1",
        Xgrid = FALSE, Ygrid = FALSE)

##CLUSTER 2##
# Transpose data frame and drop the column 'cluster'
c2_trans <-as_tibble (t(cluster2[1:144]))
c2_date <- dplyr::as_data_frame(c2_trans, rownames = "Date")
c2_date $Date <- erikadates
ts_plot(c2_date , line.mode = "lines", width = 2, dash = NULL , slider = FALSE,
        type = "single", Xtitle ="DATE AND TIME",
        Ytitle = " PRECIPITATION (MM)", title = "CLUSTER 2",
        Xgrid = FALSE, Ygrid = FALSE)
cumm2 <- data.frame(c2_trans, c=cumsum(c2_trans[1:798]))
c2_cumm <- cumm2[,-c(1:798)] 
c2_cumm_date <- dplyr::as_data_frame(c2_cumm, rownames = "Date")
c2_cumm_date $Date <- erikadates
ts_plot(c2_cumm_date , line.mode = "lines", width = 2, dash = NULL , slider = FALSE,
        type = "single", Xtitle ="DATE AND TIME",
        Ytitle = " CUMULATIVE PRECIPITATION (MM)", title = "CLUSTER 2",
        Xgrid = FALSE, Ygrid = FALSE)

##CLUSTER 3##
# Transpose data frame and drop the column 'cluster'
c3_trans <-as_tibble (t(cluster3[1:144]))
c3_date <- dplyr::as_data_frame(c3_trans, rownames = "Date")
c3_date $Date <- erikadates
ts_plot(c3_date , line.mode = "lines", width = 2, dash = NULL , slider = FALSE,
        type = "single", Xtitle ="DATE AND TIME",
        Ytitle = " PRECIPITATION (MM)", title = "CLUSTER 3",
        Xgrid = FALSE, Ygrid = FALSE)
cumm3 <- data.frame(c3_trans, c=cumsum(c3_trans[1:433]))
c3_cumm <- cumm3[,-c(1:433)] 
c3_cumm_date <- dplyr::as_data_frame(c3_cumm, rownames = "Date")
c3_cumm_date $Date <- erikadates
ts_plot(c3_cumm_date , line.mode = "lines", width = 2, dash = NULL , slider = FALSE,
        type = "single", Xtitle ="DATE AND TIME",
        Ytitle = " CUMULATIVE PRECIPITATION (MM)", title = "CLUSTER 3",
        Xgrid = FALSE, Ygrid = FALSE)

##CLUSTER 4##
# Transpose data frame and drop the column 'cluster'
c4_trans <-as_tibble (t(cluster4[1:144]))
c4_date <- dplyr::as_data_frame(c4_trans, rownames = "Date")
c4_date $Date <- erikadates
ts_plot(c4_date , line.mode = "lines", width = 2, dash = NULL , slider = FALSE,
        type = "single", Xtitle ="DATE AND TIME",
        Ytitle = " PRECIPITATION (MM)", title = "CLUSTER 4",
        Xgrid = FALSE, Ygrid = FALSE)
cumm4 <- data.frame(c4_trans, c=cumsum(c4_trans[1:399]))
c4_cumm <- cumm4[,-c(1:399)] 
c4_cumm_date <- dplyr::as_data_frame(c4_cumm, rownames = "Date")
c4_cumm_date $Date <- erikadates
ts_plot(c4_cumm_date , line.mode = "lines", width = 2, dash = NULL , slider = FALSE,
        type = "single", Xtitle ="DATE AND TIME",
        Ytitle = " CUMULATIVE PRECIPITATION (MM)", title = "CLUSTER 4",
        Xgrid = FALSE, Ygrid = FALSE)

##CLUSTER 5##
# Transpose data frame and drop the column 'cluster'
c5_trans <-as_tibble (t(cluster5[1:144]))
c5_date <- dplyr::as_data_frame(c5_trans, rownames = "Date")
c5_date $Date <- erikadates
ts_plot(c5_date , line.mode = "lines", width = 2, dash = NULL , slider = FALSE,
        type = "single", Xtitle ="DATE AND TIME",
        Ytitle = " PRECIPITATION (MM)", title = "CLUSTER 5",
        Xgrid = FALSE, Ygrid = FALSE)
cumm5 <- data.frame(c5_trans, c=cumsum(c5_trans[1:149]))
c5_cumm <- cumm5[,-c(1:149)] 
c5_cumm_date <- dplyr::as_data_frame(c5_cumm, rownames = "Date")
c5_cumm_date $Date <- erikadates
ts_plot(c5_cumm_date , line.mode = "lines", width = 2, dash = NULL , slider = FALSE,
        type = "single", Xtitle ="DATE AND TIME",
        Ytitle = " CUMULATIVE PRECIPITATION (MM)", title = "CLUSTER 5",
        Xgrid = FALSE, Ygrid = FALSE)

mean(cluster1$rain_tot)
mean(cluster2$rain_tot)
mean(cluster3$rain_tot)
mean(cluster4$rain_tot)
mean(cluster5$rain_tot)

###########EXPORTING OUTPUT##########
write.csv(c1_date[,2:133],"D:/Study/MSC THESIS/Thesis_r_project\\c1_date.csv", row.names = FALSE)
write.csv(c3_date[,2:434],"D:/Study/MSC THESIS/Thesis_r_project\\c3_date.csv", row.names = FALSE)
write.csv(c4_date[,2:400],"D:/Study/MSC THESIS/Thesis_r_project\\c4_date.csv", row.names = FALSE)
write.csv(c5_date[,2:150],"D:/Study/MSC THESIS/Thesis_r_project\\c5_date.csv", row.names = FALSE)













