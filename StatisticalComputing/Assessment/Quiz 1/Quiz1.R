#Q1
A <- rbind(c(1, 5, 7, 9),
           c(2, 4, 8, 4),
           c(7, 0, 6, 5))

A

A <- cbind(c(1,2,7),
           c(5,4,0),
           c(7,8,6),
           c(9,4,5))

#Q2
potus <- read.table("Assessment/Quiz 1/potus.txt", header = TRUE, sep = ";")
  
is.na(potus)

potus <- na.omit(potus)

#Q3
n <- 10000
coffee <- rnorm(n, 50, 15)
milk <- rnorm(n, 200, 15)

#create dataframe comprising these two variables (i.e. vectors)
coffeedataset <- data.frame(milk, coffee)

#sum the total volume of each iteration 
coffeedataset <- transform(coffeedataset, totalvolume = milk+coffee)

#Creating a new variable of whether coffee has spilled over
coffeedataset$overspill <- ifelse(coffeedataset$totalvolume >280, "YES", "NO")

#counts number of overspills - in this dataset my coffee overspilled 781 times
table(coffeedataset$overspill)


#Q4
#reading in dataset
load(file = "Assessment/Quiz 1/lakes.RData")

#For some reason the names of the lakes were loaded as rownames rather than as a proper column, so creating a new variable with the names of lakes
lakes$loch <- rownames(lakes)

#Surface area of Loch Assynt - 8 (not sure what unit of measurement this is referring to)
LockAssynt_Surfacearea <- lakes[lakes$loch=="Loch Assynt", 2]

#Average pH value - mean pH is 7.65 and median is 7.75. Similarity of measures of central tendency suggests  this variable is normally distributed
Mean_pH <- mean(lakes$pH)
Median_pH <- median(lakes$pH)

#Most acidic lake - Llyn Eiddwen with a pH value of 6
acidic <- lakes[lakes$pH==min(lakes$pH), 6]

#Creating new column called Volume

##First there are two NAs for mean depth. These have to be omitted - takes our sample from 56 to 54 lakes
lakes <- na.omit(lakes)

#New variable called Volume, which is the product of the surface area and mean depth - again, not sure what units these are in
lakes <- transform(lakes, Volume = SurfaceArea*MeanDepth)

#combined total biovolume of the 10 largest lakes (in the sense of having the largest surface areas)
##First grabbing order of surface area
permut <- order(lakes$SurfaceArea)

#rearranging dataframe by surface area in ascending order
lakes <- lakes[permut,]

#grabbing the last 10 rows, corresponding with the lakes with the 10 largest surface areas
lakes_surfacearea <- lakes[47:56,]

#total biovolume for the 10 largest lakes by surface area - 275,147L
lakes_totalbiovolume <- sum(lakes_surfacearea$TotalBiovolume)



