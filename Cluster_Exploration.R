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

getwd()
list.ras <- list.files(paste(getwd(), sep = ""), full.names = F, all.files = T, 
                       pattern = ".tif")
#list.ras 
rain <- stack(list.ras) #Raster stack
#plot(rain)
rain_sc <- (rain * 0.1) # Rainfall value scaling
#rain_sc
#plot(rain_sc,66:70)

###CONVERT TO DATE-TIME##

raindate <- substr(names(rain_sc),27,42)
erikadate <-ymd_hms(raindate)
erikadates <- as_datetime(erikadate)
#class(erikadates)
#str(erikadates)
#names(rain_sc) <- erikadates
#plot(subset(rain_sc, 60:80))
#animate(subset(rain_sc, 1:144), pause = 0.2, n = 1)

###DATA FRAME WITH COORDINATES###
dfcord <- raster::as.data.frame(rain_sc,xy=TRUE)## raster to dataframe with coordinates
row.has.na <- apply(dfcord , 1, function(x){any(is.na(x))})
dfcord.filtered <- dfcord [!row.has.na,]
#sum(row.has.na)
#row.has.na



###DATA FRAME WITHOUT COORDINATES###
precipitation <- as.data.frame(rain_sc) # convert to a data.frame
#precipitation
#crs(rain_sc)

names(precipitation) <- erikadates # read column names as date-time
#head(precipitation)
#precipitation
#plot(precipitation$`2015-08-28 03:30:00` ) # check


######DROP ROWS WITH N/A####
row.has.na <- apply(precipitation, 1, function(x){any(is.na(x))})
final.filtered <- precipitation[!row.has.na,]

#sum(row.has.na)
#row.has.na
#Tr <- t(final.filtered)# Transpose dataframe
Tr <- final.filtered
#plot(Tr$`2015-08-28 03:30:00` )
### add y coordinate
##Add raw name as part of the data##
#Tr <- data.table(Tr, keep.rownames=TRUE, check.names=FALSE, key=NULL, stringsAsFactors= FALSE )

#y <- Tr[1] # read first row ## To be clarified
#y1 <- Tr[,1]# read first column


#Tr1 <-Tr[apply(Tr,1,max) > 5,] ### drop rows with a maximum received rain less than 5mm, row max for maximum, sd for standard deviation quantile for quantile, min for minimum
##########EXPLORING CLUSTERS###
#cluster4[, "Maximum"] <- apply(cluster4[, 1:144], 1, max ) #max, mean, min, 
# Q0- Q5 =0, 0.25, 0.5,0.75, 0.9, 1
Tr[, "Q0"] <- apply(Tr[, 1:144], 1,FUN=quantile, probs=0)
Tr[, "Q1"] <- apply(Tr[, 1:144], 1,FUN=quantile, probs=0.25)
Tr[, "Q2"] <- apply(Tr[, 1:144], 1,FUN=quantile, probs=0.5)
Tr[, "Q3"] <- apply(Tr[, 1:144], 1,FUN=quantile, probs=0.75)
Tr[, "Q4"] <- apply(Tr[, 1:144], 1,FUN=quantile, probs=0.9)
Tr[, "Q5"] <- apply(Tr[, 1:144], 1,FUN=quantile, probs=1)
Tr[, "Mean"] <- apply(Tr[, 1:144], 1, mean) #max, mean, min, 
Tr[, "STD"] <- apply(Tr[, 1:144], 1, sd) 
Tr$rain_tot = rowSums(Tr[1:144],c(-1))## total rainfall in a pixel
Tr$x= dfcord.filtered$x ###add x coordinate
Tr$y= dfcord.filtered$y 

Tr1 <- select(filter(Tr, Q2 == 0 ), c(148:155))
Tr2 <- scale(Tr1[1:6])
#########CHOOSING K###############
set.seed(123)
k <- list()
for (i in 1:10){
  k[[i]] <- kmeans(Tr2,i)
}

k

betweenss_totss <- list()
for (i in 1:10){
  betweenss_totss[[i]] <- k[[i]]$betweenss/k[[i]]$totss
}
# A shoulder in the plot is an indication of the optimal number of clusters to select.
plot(1:10,betweenss_totss, type = "b",
     ylab = " Between SS / Total SS", xlab = "Clusters (k)")

for (i in 1:4) {
  plot (Tr$`2015-08-26 12:00:00`, col = k[[i]]$cluster)
}
#########ELBOW METHOD CHOOSSING K###############
set.seed(123)

# function to compute total within-cluster sum of square 
wss <- function(k) {
  kmeans(Tr2, k, nstart = 10 )$tot.withinss
}

# Compute and plot wss for k = 1 to k = 15
k.values <- 1:15

# extract wss for 2-15 clusters
wss_values <- map_dbl(k.values, wss)

plot(k.values, wss_values,
     type="b", pch = 19, frame = FALSE, 
     xlab="Number of clusters K",
     ylab="Total within-clusters sum of squares")

set.seed(123)
fviz_nbclust(Tr2, kmeans, method = "wss") ###Elbow method in one function

####AVERAGE SLHOUETTE METHOD FOR CHOOSING K#####
set.seed(123)
avg_sil <- function(k) {
  km.res <- kmeans(Tr2, centers = k, nstart = 25)
  ss <- silhouette(km.res$cluster, dist(Tr))
  mean(ss[, 3])
}

# Compute and plot wss for k = 2 to k = 15
k.values <- 2:15

# extract avg silhouette for 2-15 clusters
avg_sil_values <- map_dbl(k.values, avg_sil)

plot(k.values, avg_sil_values,
     type = "b", pch = 19, frame = FALSE, 
     xlab = "Number of clusters K",
     ylab = "Average Silhouettes")


set.seed(123)
fviz_nbclust(Tr2, kmeans, method = "silhouette") #average silhoutte method in one function

####GAP STATISTIC METHOD FOR CHOOSING K#####
set.seed(123)
gap_stat <- clusGap(Tr, FUN = kmeans, nstart = 25,
                    K.max = 10, B = 50)
print(gap_stat, method = "firstmax") # Print the result

fviz_gap_stat(gap_stat) #gap statistic method in one function

#########CLUSTERING USING EUCLIDEAN DISTANCE###############
set.seed(123)
#clusterK <- kmeans(Tr, 6)
clusterK <- kmeans(Tr2, centers = 5, nstart = 25)
fviz_cluster(clusterK, geom = "point",  data = Tr2) + ggtitle("k = 5")
clusterK$size # Cluster size
dd <- as.data.frame(cbind(Tr2, cluster = clusterK$cluster)) ##add the point classifications to the original data
Tr1$cluster= dd$cluster
Tr$cluster= Tr1$cluster

k6 <- kmeans(Tr2, centers = 6, nstart = 25)
fviz_cluster(k6, geom = "point",  data = Tr2) + ggtitle("k = 6")



#####CLUSTER STATISTICS##############

Tr %>%
  as_tibble() %>%
  mutate(cluster = clusterK$cluster,
         state = row.names(Tr)) %>%
  ggplot(aes(`2015-08-26 12:00:00`, `2015-08-27 12:00:00`, color = factor(cluster), label = state)) +
  geom_text()


#clusterK
#plot(Tr$`2015-08-28 06:30:00`, col = clusterK$cluster) ##Plot to show which cluster each pixel belongs, index x axis is row number
#aggregate(Tr, by=list(cluster=clusterK$cluster), mean) #compute the mean of each variables by clusters using the original data

#head(dd)
#dd$row_sum = rowSums(Tr[,c(-1)]) # add column row sum after clustering, total rainfall received in a pixel for the storm period
clusterK$cluster # Cluster number for each of the observations
head(clusterK$cluster,4) # Cluster number for the first four observations

clusters$centers # Cluster means
#####PLOT CLUSTERS##############
dd$x= dfcord.filtered$x ###add x coordinate
dd$y= dfcord.filtered$y ### add y coordinate
#c1 <- select(filter(dd, cluster == 1 ), c(146,147,145))
#c2 <- select(filter(dd, cluster == 2), c(146,147,145))
#c3 <- select(filter(dd, cluster == 3), c(146,147,145))
#c4 <- select(filter(dd, cluster == 4), c(146,147,145))
#c5 <- select(filter(dd, cluster == 5), c(146,147,145))
#c6 <- select(filter(dd, cluster == 6), c(146,147,145))
All_dd <- select(filter(Tr1, cluster >= 1), c(7,8,9))
dfr <- rasterFromXYZ(All_dd )
crs(dfr) <-  "+init=EPSG:4326"
plot(dfr, main = "Clusters")
dfr 

############SUBSETING THE DATAFRAME######
cluster1 <- Tr[(Tr$cluster == "1"),]
cluster2 <- Tr[(Tr$cluster == "2"),]
cluster3 <- Tr[(Tr$cluster == "3"),]
cluster4 <- Tr[(Tr$cluster == "4"),]
cluster5 <- Tr[(Tr$cluster == "5"),]


distance <- get_dist(cluster2[1:144]) #distance matrix between the rows of a data matrix
fviz_dist(distance, gradient = list(low = "#00AFBB", mid = "white", high = "#FC4E07"))#visualizing a distance matrix
boxplot(cluster1$rain_tot)

###########ADDING VECTOR FILES##########
study <- readOGR(dsn=path.expand("D:/Study/MSC THESIS/DataAnalysisR/ClippedRainfall/StudyBuffer"), layer="Dominica_500km")
dominica <- readOGR(dsn=path.expand("D:/Study/MSC THESIS/DataAnalysisR/ClippedRainfall/StudyBuffer"), layer="Dominica")
airport <- readOGR(dsn=path.expand("D:/Study/MSC THESIS/DataAnalysisR/ClippedRainfall/StudyBuffer"), layer="Airports")
erika_track <- readOGR(dsn=path.expand("D:/Study/MSC THESIS/DataAnalysisR/ClippedRainfall/TC Tracks"), layer="Erika_Study")
plot(study,add = TRUE)
plot(dominica,add = TRUE)
plot(airport,add = TRUE)
plot(erika_track,add = TRUE)

#########AIRPORTS###############

DC <- cellFromXY(dfr, c(-61.300759,15.546122)) #Douglas Charles Airport
DC
CF <- cellFromXY(dfr, c(-61.391948,15.336980)) #Cane Field Airport
CF
rain_sc[cell]
hist(rain_sc[], main=NULL)

###PLOT For all pixels in a cluster####
##############CLUSTER 1############
######### Transpose dataframe and drop the column cluster
c1_trans <-as_tibble (t(cluster1[1:144]))
c1_date <- dplyr::as_data_frame(c1_trans, rownames = "Date")
c1_date $Date <- erikadates
ts_plot(c1_date , line.mode = "lines", width = 2, dash = NULL , slider = FALSE,
        type = "single", Xtitle ="Date",
        Ytitle = " Precipitation (mm)", title = "CLUSTER 1",
        Xgrid = FALSE, Ygrid = FALSE)
cumm1 <- data.frame(c1_trans, c=cumsum(c1_trans[1:132]))
c1_cumm <- cumm1[,-c(1:132)] 
c1_cumm_date <- dplyr::as_data_frame(c1_cumm, rownames = "Date")
c1_cumm_date $Date <- erikadates
ts_plot(c1_cumm_date , line.mode = "lines", width = 2, dash = NULL , slider = FALSE,
        type = "single", Xtitle ="Date",
        Ytitle = " Cummulative Precipitation (mm)", title = "CLUSTER 1- CUMMULATIVE",
        Xgrid = FALSE, Ygrid = FALSE)
###plot quantiles ###
quantile(cluster1$rain_tot, c(0,0.25, 0.5, 0.75,0.9,1))
c1_quantiles <- c(1219, 1058,1158,1016,1066,1161)
ggplot(data=c1_cumm_date) + 
  geom_line(aes(x=Date,y=c.1219,color = "Q0")) + 
  geom_line(aes(x=Date,y=c.1058,color = "Q1")) + 
  geom_line(aes(x=Date,y=c.1158,color = "Q2")) + 
  geom_line (aes(x=Date,y=c.1016,color = "Q3")) +
  geom_line(aes(x=Date,y=c.1066,color = "Q4")) + 
  geom_line (aes(x=Date,y=c.1161,color = "Q5")) +
  labs(x = "Date",
       y = " Cummulative Precipitation (mm)",
       title = "Erika 2015",
       subtitle = "Cluster 1 Quantiles")
ggplot(data=c1_date) +
  geom_line(aes(x=Date,y=`1219`,color = "Q0")) + 
  geom_line(aes(x=Date,y=`1058`,color = "Q1")) + 
  geom_line(aes(x=Date,y=`1158`,color = "Q2")) + 
  geom_line (aes(x=Date,y=`1016`,color = "Q3")) +
  geom_line(aes(x=Date,y=`1066`,color = "Q4")) + 
  geom_line (aes(x=Date,y=`1161`,color = "Q5")) +
  labs(x = "Date",
       y = " Precipitation (mm)",
       title = "Erika 2015",
       subtitle = "Cluster 1 Quantiles")
c1_quat <- as.data.frame (c1_date[,c("Date","1219","1058","1158","1016","1066","1161")])

##############CLUSTER 2############
######### Transpose dataframe and drop the column cluster
c2_trans <-as_tibble (t(cluster2[1:144]))
c2_date <- dplyr::as_data_frame(c2_trans, rownames = "Date")
c2_date $Date <- erikadates
ts_plot(c2_date , line.mode = "lines", width = 2, dash = NULL , slider = FALSE,
        type = "single", Xtitle ="Date",
        Ytitle = " Precipitation (mm)", title = "CLUSTER 2",
        Xgrid = FALSE, Ygrid = FALSE)
cumm2 <- data.frame(c2_trans, c=cumsum(c2_trans[1:798]))
c2_cumm <- cumm2[,-c(1:798)] 
c2_cumm_date <- dplyr::as_data_frame(c2_cumm, rownames = "Date")
c2_cumm_date $Date <- erikadates
ts_plot(c2_cumm_date , line.mode = "lines", width = 2, dash = NULL , slider = FALSE,
        type = "single", Xtitle ="Date",
        Ytitle = " Cummulative Precipitation (mm)", title = "CLUSTER 2- CUMMULATIVE",
        Xgrid = FALSE, Ygrid = FALSE)
###plot quantiles ###
quantile(cluster2$rain_tot, c(0,0.25, 0.5, 0.75,0.9,1))
c2_quantiles <- c(2016, 2279,179,428, 1712,1640)
ggplot(data=c2_cumm_date) + 
  geom_line(aes(x=Date,y=c.2016,color = "Q0")) + 
  geom_line(aes(x=Date,y=c.2279,color = "Q1")) + 
  geom_line(aes(x=Date,y=c.179,color = "Q2")) + 
  geom_line (aes(x=Date,y=c.428,color = "Q3")) +
  geom_line(aes(x=Date,y=c.1712,color = "Q4")) + 
  geom_line (aes(x=Date,y=c.1640,color = "Q5")) +
  labs(x = "Date",
       y = " Cummulative Precipitation (mm)",
       title = "Erika 2015",
       subtitle = "Cluster 2 Quantiles")
ggplot(data=c2_date) +
  geom_line(aes(x=Date,y=`2016`,color = "Q0")) + 
  geom_line(aes(x=Date,y=`2279`,color = "Q1")) + 
  geom_line(aes(x=Date,y=`179`,color = "Q2")) + 
  geom_line (aes(x=Date,y=`428`,color = "Q3")) +
  geom_line(aes(x=Date,y=`1712`,color = "Q4")) + 
  geom_line (aes(x=Date,y=`1640`,color = "Q5")) +
  labs(x = "Date",
       y = " Precipitation (mm)",
       title = "Erika 2015",
       subtitle = "Cluster 2 Quantiles")
c2_quat <- as.data.frame (c2_date[,c("Date","2016","2279","179","428","1712","1640")])
##############CLUSTER 3############
######### Transpose dataframe and drop the column cluster
c3_trans <-as_tibble (t(cluster3[1:144]))
c3_date <- dplyr::as_data_frame(c3_trans, rownames = "Date")
c3_date $Date <- erikadates
ts_plot(c3_date , line.mode = "lines", width = 2, dash = NULL , slider = FALSE,
        type = "single", Xtitle ="Date",
        Ytitle = " Precipitation (mm)", title = "CLUSTER 3",
        Xgrid = FALSE, Ygrid = FALSE)
cumm3 <- data.frame(c3_trans, c=cumsum(c3_trans[1:433]))
c3_cumm <- cumm3[,-c(1:433)] 
c3_cumm_date <- dplyr::as_data_frame(c3_cumm, rownames = "Date")
c3_cumm_date $Date <- erikadates
ts_plot(c3_cumm_date , line.mode = "lines", width = 2, dash = NULL , slider = FALSE,
        type = "single", Xtitle ="Date",
        Ytitle = " Cummulative Precipitation (mm)", title = "CLUSTER 3- CUMMULATIVE",
        Xgrid = FALSE, Ygrid = FALSE)

quantile(cluster3$rain_tot, c(0,0.25, 0.5, 0.75,0.9,1))
c3_quantiles <- c(363, 1083,1352,500, 552,699)
ggplot(data=c3_cumm_date) + 
  geom_line(aes(x=Date,y=c.363,color = "Q0")) + 
  geom_line(aes(x=Date,y=c.1083,color = "Q1")) + 
  geom_line(aes(x=Date,y=c.1352,color = "Q2")) + 
  geom_line (aes(x=Date,y=c.500,color = "Q3")) +
  geom_line(aes(x=Date,y=c.552,color = "Q4")) + 
  geom_line (aes(x=Date,y=c.699,color = "Q5")) +
  labs(x = "Date",
       y = " Cummulative Precipitation (mm)",
       title = "Erika 2015",
       subtitle = "Cluster 3 Quantiles")
ggplot(data=c3_date) +
  geom_line(aes(x=Date,y=`363`,color = "Q0")) + 
  geom_line(aes(x=Date,y=`1083`,color = "Q1")) + 
  geom_line(aes(x=Date,y=`1352`,color = "Q2")) + 
  geom_line (aes(x=Date,y=`500`,color = "Q3")) +
  geom_line(aes(x=Date,y=`552`,color = "Q4")) + 
  geom_line (aes(x=Date,y=`699`,color = "Q5")) +
  labs(x = "Date",
       y = " Precipitation (mm)",
       title = "Erika 2015",
       subtitle = "Cluster 3 Quantiles")
c3_quat <- as.data.frame (c3_date[,c("Date","363","1083","1352","500","552","699")])
##############CLUSTER 4############
######### Transpose dataframe and drop the column cluster
c4_trans <-as_tibble (t(cluster4[1:144]))
c4_date <- dplyr::as_data_frame(c4_trans, rownames = "Date")
c4_date $Date <- erikadates
ts_plot(c4_date , line.mode = "lines", width = 2, dash = NULL , slider = FALSE,
        type = "single", Xtitle ="Date",
        Ytitle = " Precipitation (mm)", title = "CLUSTER 4",
        Xgrid = FALSE, Ygrid = FALSE)
cumm4 <- data.frame(c4_trans, c=cumsum(c4_trans[1:399]))
c4_cumm <- cumm4[,-c(1:399)] 
c4_cumm_date <- dplyr::as_data_frame(c4_cumm, rownames = "Date")
c4_cumm_date $Date <- erikadates
ts_plot(c4_cumm_date , line.mode = "lines", width = 2, dash = NULL , slider = FALSE,
        type = "single", Xtitle ="Date",
        Ytitle = " Cummulative Precipitation (mm)", title = "CLUSTER 4- CUMMULATIVE",
        Xgrid = FALSE, Ygrid = FALSE)

quantile(cluster4$rain_tot, c(0,0.25, 0.5, 0.75,0.9,1))
c4_quantiles <- c(369, 577,983,1539, 1483,1085) 
ggplot(data=c4_cumm_date) + 
  geom_line(aes(x=Date,y=c.369,color = "Q0")) + 
  geom_line(aes(x=Date,y=c.577,color = "Q1")) + 
  geom_line(aes(x=Date,y=c.983,color = "Q2")) + 
  geom_line (aes(x=Date,y=c.1539,color = "Q3")) +
  geom_line(aes(x=Date,y=c.1483,color = "Q4")) + 
  geom_line (aes(x=Date,y=c.1085,color = "Q5")) +
  labs(x = "Date",
       y = " Cummulative Precipitation (mm)",
       title = "Erika 2015",
       subtitle = "Cluster 4 Quantiles")
ggplot(data=c4_date) +
  geom_line(aes(x=Date,y=`369`,color = "Q0")) + 
  geom_line(aes(x=Date,y=`577`,color = "Q1")) + 
  geom_line(aes(x=Date,y=`983`,color = "Q2")) + 
  geom_line (aes(x=Date,y=`1539`,color = "Q3")) +
  geom_line(aes(x=Date,y=`1483`,color = "Q4")) + 
  geom_line (aes(x=Date,y=`1085`,color = "Q5")) +
  labs(x = "Date",
       y = " Precipitation (mm)",
       title = "Erika 2015",
       subtitle = "Cluster 4 Quantiles")
c4_quat <- as.data.frame (c4_date[,c("Date","369","577","983","1539","1483","1085")])
##############CLUSTER 5############
######### Transpose dataframe and drop the column cluster
c5_trans <-as_tibble (t(cluster5[1:144]))
c5_date <- dplyr::as_data_frame(c5_trans, rownames = "Date")
c5_date $Date <- erikadates
ts_plot(c5_date , line.mode = "lines", width = 2, dash = NULL , slider = FALSE,
        type = "single", Xtitle ="Date",
        Ytitle = " Precipitation (mm)", title = "CLUSTER 5",
        Xgrid = FALSE, Ygrid = FALSE)
cumm5 <- data.frame(c5_trans, c=cumsum(c5_trans[1:149]))
c5_cumm <- cumm5[,-c(1:149)] 
c5_cumm_date <- dplyr::as_data_frame(c5_cumm, rownames = "Date")
c5_cumm_date $Date <- erikadates
ts_plot(c5_cumm_date , line.mode = "lines", width = 2, dash = NULL , slider = FALSE,
        type = "single", Xtitle ="Date",
        Ytitle = " Cummulative Precipitation (mm)", title = "CLUSTER 5- CUMMULATIVE",
        Xgrid = FALSE, Ygrid = FALSE)
###plot quantiles ###
quantile(cluster5$rain_tot, c(0,0.25, 0.5, 0.75,0.9,1))
c5_quantiles <- c(671, 816,1051,658, 704,899)
ggplot(data=c5_cumm_date) + 
  geom_line(aes(x=Date,y=c.671,color = "Q0")) + 
  geom_line(aes(x=Date,y=c.816,color = "Q1")) + 
  geom_line(aes(x=Date,y=c.1051,color = "Q2")) + 
  geom_line (aes(x=Date,y=c.658,color = "Q3")) +
  geom_line(aes(x=Date,y=c.704,color = "Q4")) + 
  geom_line (aes(x=Date,y=c.899,color = "Q5")) +
  labs(x = "Date",
       y = " Cummulative Precipitation (mm)",
       title = "Erika 2015",
       subtitle = "Cluster 5 Quantiles")
ggplot(data=c5_date) +
  geom_line(aes(x=Date,y=`671`,color = "Q0")) + 
  geom_line(aes(x=Date,y=`816`,color = "Q1")) + 
  geom_line(aes(x=Date,y=`1051`,color = "Q2")) + 
  geom_line (aes(x=Date,y=`658`,color = "Q3")) +
  geom_line(aes(x=Date,y=`704`,color = "Q4")) + 
  geom_line (aes(x=Date,y=`899`,color = "Q5")) +
  labs(x = "Date",
       y = " Precipitation (mm)",
       title = "Erika 2015",
       subtitle = "Cluster 5 Quantiles")
c5_quat <- as.data.frame (c5_date[,c("Date","671","816","1051","658","704","899")])

###########Peaks##########
c1_trans[, "peak_info"]
peak_info<- as.data.frame(lapply(c1_trans[,1:132], findpeaks, threshold=2))


############PLOTING SELECTED PIXELS######
#pixel <- c(1161)#)             #Cluster1
#pixel <- c(1640)#)             #Cluster2
#pixel <- c(699)#)              #Cluster3
#pixel <- c(1085) # )           #Cluster4
pixel <- c(899)#,)              #Cluster5

Cluster1 <- data.frame(
  Date = rep(erikadates, each = length(pixel)),
  cell = rep(pixel, length(erikadates)),
  Type = 'Max',
  RAIN = c(rain_sc[pixel]))

#pixel <- c(1219)#,)              # Cluster1
#pixel <- c(2016)#)               #Cluster2
#pixel <- c(363)#)                #Cluster3
#pixel <- c(369)#,                #Cluster4
pixel <- c(671)#,)                #Cluster5

Cluster2 <- data.frame(Date = rep(erikadates, each = length(pixel)),
                       cell = rep(pixel, length(erikadates)),
                       Type = 'Min',
                       RAIN = c(rain_sc[pixel]))
pixel <- rbind(Cluster1, Cluster2)
ggplot(pixel,
       aes(x = Date, y = RAIN,
           group = cell, col = Type)) +
  geom_line()

############PLOTING SELECTED PIXELS######
# for extraction of representative series based on quantiles for total rain in a pixel
test <- sapply(colnames(c5_cumm_date[2:150]), function(y){quantile(x=unlist(c5_cumm_date[,y]),c(0.25,.5,.75,0.9))})
matplot(t(test),type="l")
t1 <- as.data.frame(test)
class(t1)
t1_Trans <- t(t1)
t1_Trans <- dplyr::as_data_frame(t1_Trans, rownames = "Date")
t1_Trans$Date <- erikadates


c1_trans <-as_tibble (t(cluster1[1:144]))
c1_date <- dplyr::as_data_frame(c1_trans, rownames = "Date")
c1_date $Date <- erikadates


both <- merge(c1_date,t1_Trans,by="Date")

ts_plot(both , line.mode = "lines", width = 2, dash = NULL , slider = FALSE,
        type = "single", Xtitle ="Date",
        Ytitle = " Precipitation (mm)", title = "T1",
        Xgrid = FALSE, Ygrid = FALSE)

t <- as.data.frame(quantile(cluster1$rain_tot, probs = c(0,0.25,0.5,0.75,0.9,1)))
library(ggfortify)
p <- ggdistribution(dchisq, seq(t), df = 7, colour = 'blue')
###########EXPORTING OUTPUT##########
write.csv(c2_cumm_date,"D:/Study/MSC THESIS/R_output/Temporal Clustering\\c2_cumm_date.csv", row.names = FALSE)
writeRaster(dfr,"D:/Study/MSC THESIS/R_output\\ClustersA.tif", row.names = FALSE)
write.csv(cluster5[, 145:155],"D:/Study/MSC THESIS/R_output/Temporal Clustering\\Cluster5.csv", row.names = FALSE)
writeRaster(dfr,"D:/Study/MSC THESIS/R_output/Temporal Clustering\\Clusters5.tif", row.names = FALSE)







