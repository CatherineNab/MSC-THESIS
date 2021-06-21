##############SELECT RAINFALL AT VARYING THRESHOLDS############

working_dir <- "D:/Study/MSC THESIS/Thesis_r_project/" ##Select working directory
setwd(working_dir) # This sets your working directory to the working_dir path
dir.create(path = "outputCSVs/", showWarnings = FALSE, recursive = TRUE)#Results Store
#list of CSVs in folder
files = list.files(pattern="*.csv") 

dataFiles <- lapply(files, function(i) {
  df <- read.csv(i) 
  return(df)
})

for(k in 1:length(files)) {
  # change this to the name of your data CSV
  temp_file <- files[k]
    temp_data <- read.csv(temp_file) # Read the data input CSV
    temp_output <- temp_data[0,]  #create an empty copy of the input data
    dataLength <-Inf
    #calculate the information i need such as the size of the output data frame to prevent errors dues to different sizes of columns 
  for(i in 1:ncol(temp_data)) {
    dataLength <- length(temp_data[ , i])
  }

  temp_output[nrow(temp_output)+dataLength,] <- NA# create empty rows in data frame to store results
    temp_output[is.na(temp_output)] <- 0
  
      #run through the dataset and store the values greater than 1.
  for(i in 1:ncol(temp_data)) {
    #set threshold (mm) here to be included in the output file name.
    threshold <- 1.0 # other thresholds were 2.5mm, 5mm and 10mm
    
    indexOne <- which(temp_data[ , i] >= threshold)
    ifelse(is.na(indexOne[1]),indexOne[1] <- 1, indexOne[1] <- indexOne[1])
    
    #you cannot have a column that is all zeros
    temp_output[,i] <- c(tail(temp_data[,i], dataLength - indexOne[1] + 1), tail(temp_output[,i], dataLength - length(tail(temp_output[,i], dataLength - (indexOne[1] - 1)))))
  }
  #export the resulting input or provide a way to process multiple files at once logic may be added after this creating a function which takes the input files as an argument and loops through them
  write.csv(temp_output, file = paste0("outputCSVs/",files[k],"_Threshold",threshold,".csv"), row.names=F)
  
}


##############CLUSTER 1############
c1_Thresh1 <-  data.frame(read.csv(file = "D:/Study/MSC THESIS/Thesis_r_project/outputCSVs/c1_date.csv_Threshold1.csv")) 
        #add column for Time steps
c1_Thresh1 <- dplyr::as_data_frame(c1_Thresh1, rownames = "TimeSteps")
c1_Thresh1 $`TimeSteps` 
c1_Thresh1[, "TimeSteps"] = as.numeric(c1_Thresh1 $`TimeSteps`)  *30 #start at 30 minutes

 #Calculate Quantiles, Mean and Standard Deviation
c1_Thresh1[, "Q0"] <- apply(c1_Thresh1[, 2:133], 1,FUN=quantile, probs=0)
c1_Thresh1[, "Q1"] <- apply(c1_Thresh1[, 2:133], 1,FUN=quantile, probs=0.25)
c1_Thresh1[, "Q2"] <- apply(c1_Thresh1[, 2:133], 1,FUN=quantile, probs=0.5)
c1_Thresh1[, "Q3"] <- apply(c1_Thresh1[, 2:133], 1,FUN=quantile, probs=0.75)
c1_Thresh1[, "Q4"] <- apply(c1_Thresh1[, 2:133], 1,FUN=quantile, probs=0.9)
c1_Thresh1[, "Q5"] <- apply(c1_Thresh1[, 2:133], 1,FUN=quantile, probs=1)
c1_Thresh1[, "Mean"] <- apply(c1_Thresh1[,2:133], 1, mean) #max, mean, 

#convert Quantiles, Mean and Standard Deviation to rainfall intensity  (multiply by 2)
c1_Int1 <- data.frame(c1_Thresh1[,134:140]*2)
c1_Int1[, "TimeSteps"] = c1_Thresh1$TimeSteps #add time steps 
#plot quantiles and mean
ggplot(data=c1_Int1) + 
  geom_line(aes(x=TimeSteps,y=Q0,color = "Q0")) + 
  geom_line(aes(x=TimeSteps,y=Q1,color = "Q1")) + 
  geom_line(aes(x=TimeSteps,y=Q2,color = "Q2")) + 
  geom_line (aes(x=TimeSteps,y=Q3,color = "Q3")) +
  geom_line(aes(x=TimeSteps,y=Q4,color = "Q4")) + 
 geom_line (aes(x=TimeSteps,y=Q5,color = "Q5")) +
  geom_line (aes(x=TimeSteps,y=Mean,color = "Mean")) +
  labs(x = "TIME (MINUTES)",
       y = "  Int1 (MM/HOUR)",
       title = "Erika 2015: Cluster 1",
       subtitle = " Threshold = 1")
#total rainfall at each computed statistic
sum(c1_Thresh1$Q0)
sum(c1_Thresh1$Q1)
sum(c1_Thresh1$Q2)
sum(c1_Thresh1$Q3)
sum(c1_Thresh1$Q4)
sum(c1_Thresh1$Q5)
sum(c1_Thresh1$Mean)

#write.csv(c1_Thresh1,"D:/Study/MSC THESIS/Thesis_r_project/New folder/c1_Thresh1.csv", row.names = FALSE)

##############CLUSTER 3############
c3_Thresh1 <-  data.frame(read.csv(file = "D:/Study/MSC THESIS/Thesis_r_project/outputCSVs/c3_date.csv_Threshold1.csv")) 
#add column for Time steps
c3_Thresh1 <- dplyr::as_data_frame(c3_Thresh1, rownames = "TimeSteps")
c3_Thresh1 $`TimeSteps` 
c3_Thresh1[, "TimeSteps"] = as.numeric(c3_Thresh1 $`TimeSteps`)  *30 #start at 30 minutes

#Calculate Quantiles, Mean and Standard Deviation
c3_Thresh1[, "Q0"] <- apply(c3_Thresh1[, 2:434], 1,FUN=quantile, probs=0)
c3_Thresh1[, "Q1"] <- apply(c3_Thresh1[, 2:434], 1,FUN=quantile, probs=0.25)
c3_Thresh1[, "Q2"] <- apply(c3_Thresh1[, 2:434], 1,FUN=quantile, probs=0.5)
c3_Thresh1[, "Q3"] <- apply(c3_Thresh1[, 2:434], 1,FUN=quantile, probs=0.75)
c3_Thresh1[, "Q4"] <- apply(c3_Thresh1[, 2:434], 1,FUN=quantile, probs=0.9)
c3_Thresh1[, "Q5"] <- apply(c3_Thresh1[, 2:434], 1,FUN=quantile, probs=1)
c3_Thresh1[, "Mean"] <- apply(c3_Thresh1[,2:434], 1, mean) #max, mean, min, 

#convert Quantiles, Mean and Standard Deviation to rainfall intensity  (multiply by 2)
c3_Int1 <- data.frame(c3_Thresh1[,435:441]*2)
c3_Int1[, "TimeSteps"] = c3_Thresh1$TimeSteps#add time steps 
#plot quantiles and mean
ggplot(data=c3_Int1) + 
  geom_line(aes(x=TimeSteps,y=Q0,color = "Q0")) + 
  geom_line(aes(x=TimeSteps,y=Q1,color = "Q1")) + 
  geom_line(aes(x=TimeSteps,y=Q2,color = "Q2")) + 
  geom_line (aes(x=TimeSteps,y=Q3,color = "Q3")) +
  geom_line(aes(x=TimeSteps,y=Q4,color = "Q4")) + 
  geom_line (aes(x=TimeSteps,y=Q5,color = "Q5")) +
  geom_line (aes(x=TimeSteps,y=Mean,color = "Mean")) +
  labs(x = "TIME (MINUTES)",
       y = "  Int1 (MM/HOUR)",
       title = "Erika 2015: Cluster 3",
       subtitle = " Threshold = 1")
#total rainfall at each computed statistic
sum(c3_Thresh1$Q0)
sum(c3_Thresh1$Q1)
sum(c3_Thresh1$Q2)
sum(c3_Thresh1$Q3)
sum(c3_Thresh1$Q4)
sum(c3_Thresh1$Q5)
sum(c3_Thresh1$Mean)

##############CLUSTER 4############
c4_Thresh1 <-  data.frame(read.csv(file = "D:/Study/MSC THESIS/Thesis_r_project/outputCSVs/c4_date.csv_Threshold1.csv")) 
#add column for Time steps
c4_Thresh1 <- dplyr::as_data_frame(c4_Thresh1, rownames = "TimeSteps")
c4_Thresh1 $`TimeSteps` 
c4_Thresh1[, "TimeSteps"] = as.numeric(c4_Thresh1 $`TimeSteps`)  *30 #start at 30 minutes

#Calculate Quantiles, Mean and Standard Deviation
c4_Thresh1[, "Q0"] <- apply(c4_Thresh1[, 2:400], 1,FUN=quantile, probs=0)
c4_Thresh1[, "Q1"] <- apply(c4_Thresh1[, 2:400], 1,FUN=quantile, probs=0.25)
c4_Thresh1[, "Q2"] <- apply(c4_Thresh1[, 2:400], 1,FUN=quantile, probs=0.5)
c4_Thresh1[, "Q3"] <- apply(c4_Thresh1[, 2:400], 1,FUN=quantile, probs=0.75)
c4_Thresh1[, "Q4"] <- apply(c4_Thresh1[, 2:400], 1,FUN=quantile, probs=0.9)
c4_Thresh1[, "Q5"] <- apply(c4_Thresh1[, 2:400], 1,FUN=quantile, probs=1)
c4_Thresh1[, "Mean"] <- apply(c4_Thresh1[,2:400], 1, mean) #max, mean, min,

#convert Quantiles, Mean and Standard Deviation to rainfall intensity  (multiply by 2)
c4_Int1 <- data.frame(c4_Thresh1[,401:407]*2)
c4_Int1[, "TimeSteps"] = c4_Thresh1$TimeSteps#add time steps 
#plot quantiles and mean
ggplot(data=c4_Int1) + 
  geom_line(aes(x=TimeSteps,y=Q0,color = "Q0")) + 
  geom_line(aes(x=TimeSteps,y=Q1,color = "Q1")) + 
  geom_line(aes(x=TimeSteps,y=Q2,color = "Q2")) + 
  geom_line (aes(x=TimeSteps,y=Q3,color = "Q3")) +
  geom_line(aes(x=TimeSteps,y=Q4,color = "Q4")) + 
  geom_line (aes(x=TimeSteps,y=Q5,color = "Q5")) +
  geom_line (aes(x=TimeSteps,y=Mean,color = "Mean")) +
  labs(x = "TIME (MINUTES)",
       y = "  Int1 (MM/HOUR)",
       title = "Erika 2015: Cluster 4",
       subtitle = " Threshold = 1")
#total rainfall at each computed statistic
sum(c4_Thresh1$Q0)
sum(c4_Thresh1$Q1)
sum(c4_Thresh1$Q2)
sum(c4_Thresh1$Q3)
sum(c4_Thresh1$Q4)
sum(c4_Thresh1$Q5)
sum(c4_Thresh1$Mean)

##############CLUSTER 5############
c5_Thresh1 <-  data.frame(read.csv(file = "D:/Study/MSC THESIS/Thesis_r_project/outputCSVs/c5_date.csv_Threshold1.csv")) 
#add column for Time steps
c5_Thresh1 <- dplyr::as_data_frame(c5_Thresh1, rownames = "TimeSteps")
c5_Thresh1 $`TimeSteps` 
c5_Thresh1[, "TimeSteps"] = as.numeric(c5_Thresh1 $`TimeSteps`)  *30 #start at 30 minutes

#Calculate Quantiles, Mean and Standard Deviation
c5_Thresh1[, "Q0"] <- apply(c5_Thresh1[, 2:150], 1,FUN=quantile, probs=0)
c5_Thresh1[, "Q1"] <- apply(c5_Thresh1[, 2:150], 1,FUN=quantile, probs=0.25)
c5_Thresh1[, "Q2"] <- apply(c5_Thresh1[, 2:150], 1,FUN=quantile, probs=0.5)
c5_Thresh1[, "Q3"] <- apply(c5_Thresh1[, 2:150], 1,FUN=quantile, probs=0.75)
c5_Thresh1[, "Q4"] <- apply(c5_Thresh1[, 2:150], 1,FUN=quantile, probs=0.9)
c5_Thresh1[, "Q5"] <- apply(c5_Thresh1[, 2:150], 1,FUN=quantile, probs=1)
c5_Thresh1[, "Mean"] <- apply(c5_Thresh1[,2:150], 1, mean) #max, mean, min, 

#convert Quantiles, Mean and Standard Deviation to rainfall intensity  (multiply by 2)
c5_Int1 <- data.frame(c5_Thresh1[,151:157]*2)
c5_Int1[, "TimeSteps"] = c5_Thresh1$TimeSteps#add time steps 
#plot quantiles and mean
ggplot(data=c5_Int1) + 
  geom_line(aes(x=TimeSteps,y=Q0,color = "Q0")) + 
  geom_line(aes(x=TimeSteps,y=Q1,color = "Q1")) + 
  geom_line(aes(x=TimeSteps,y=Q2,color = "Q2")) + 
  geom_line (aes(x=TimeSteps,y=Q3,color = "Q3")) +
  geom_line(aes(x=TimeSteps,y=Q4,color = "Q4")) + 
  geom_line (aes(x=TimeSteps,y=Q5,color = "Q5")) +
  geom_line (aes(x=TimeSteps,y=Mean,color = "Mean")) +
  labs(x = "TIME (MINUTES)",
       y = "  Int1 (MM/HOUR)",
       title = "Erika 2015: Cluster 5",
       subtitle = " Threshold = 1")
#total rainfall at each computed statistic
sum(c5_Thresh1$Q0)
sum(c5_Thresh1$Q1)
sum(c5_Thresh1$Q2)
sum(c5_Thresh1$Q3)
sum(c5_Thresh1$Q4)
sum(c5_Thresh1$Q5)
sum(c5_Thresh1$Mean)

###########EXPORTING OUTPUT##########
write.csv(c1_Int1,"D:/Study/MSC THESIS/Thesis_r_project/Quantiles\\c1_Thresh1.csv", row.names = FALSE)
write.csv(c3_Int1,"D:/Study/MSC THESIS/Thesis_r_project/Quantiles\\c3_Thresh1.csv", row.names = FALSE)
write.csv(c4_Int1,"D:/Study/MSC THESIS/Thesis_r_project/Quantiles\\c4_Thresh1.csv", row.names = FALSE)
write.csv(c5_Int1,"D:/Study/MSC THESIS/Thesis_r_project/Quantiles\\c5_Thresh1.csv", row.names = FALSE)
