#Week3

##########################################
#WORKING WITH DATAFRAMES
kids <- rbind(c(4,15,101),
              c(11,28,132))

colnames(kids) <- c("age", "weight", "height")
rownames(kids) <- c("Mary", "John")

kids2 <- cbind(kids, gender = c("f","m"))

#Turn matrix into data frame
kids <- as.data.frame(kids)

#or alternatively,
kids <- data.frame(age=c(4,11),
                   weight = c(15,28),
                   height = c(101,132),
                   gender = c("f", "f"))
rownames(kids) <- c("Mary", "John")

#Dataframes behave like lists - use $ operator
kids$age
kids[1]
kids[[1]]

##changing values
kids["John", "age"] <- 5

##########################################
##########################################
##########################################
#ADDING NEW COLUMNS - MUTATE EQUIVALENT

chol <- readr::read_csv("Week3/Data/chol.csv")

#Works for data frames and matrices
chol <- cbind(chol, log.hdl.ldl=log(chol[,"hdl"]/chol[,"ldl"]))

#ONLY WORKS FOR DATA FRAMES
chol$ldl.hdl.ldl <- log(chol$hdl/chol$hdl)

#better way - do NOT need $ operator
chol <- transform(chol, log.hdl.ldl=log(hdl/ldl))

##########################################
##########################################
##########################################
#REMOVING COLUMNS - SELECT EQUIVALENT
chol <- chol[,-3]

##########################################
##########################################
##########################################
#SUBSETTING/SELECTING

#create new dataset based on condition of column
chol.smoked <- chol[chol$smoke!="no",]
#get rid of NA
chol.smoked <- na.omit(chol.smoked)

##again subset data in one go - SAME AS TRANFORM (do not need $ operator)
chol.smoked <- subset(chol, smoke !="no")

#TASK 1
chol.lowhdl <- subset(chol, hdl<40)

##########################################
##########################################
##########################################
#SORTING - SAME AS ARRANGE

permut <- order(chol$age)
chol <- chol[permut,]
chol

##########################################
##########################################
##########################################
#Task2

cia <- readr::read_csv("Week3/Data/cia.csv")

#1
cia_pop <- cia[cia$Population!="?",]

#2
cia_pop$Population <- as.numeric(cia_pop$Population)
cia_pop <- subset(cia_pop, Population<10000)

#3
##Transforming to numeric
cia <- transform(cia, MilExpGDP=((as.numeric(MilitaryExpenditure)/as.numeric(GDP))*100))
cia_militarygdp <- subset(cia, MilExpGDP>8)

#4
cia_GDP <- cia[cia$Continent=="Europe",]
cia_GDP <- cia[cia$GDP!="?",]
sum(as.numeric(cia_GDP$GDP))

#5
cia_5 <- transform(cia, GDPPop=((as.numeric(GDP)/as.numeric(Population))))
cia_5 <- transform(cia_5, MilitaryPop=((as.numeric(MilitaryExpenditure)/as.numeric(Population))))

#6
permut <- order(cia$Life)
cia <- cia[permut,]

##########################################
##########################################
##########################################

#CAN'T DO ALL THAT MERGE STUFF, BUT IT'S SIMPLE ENOUGH

load(file = "Week3/Data/patients_weights.RData")

head(patients)
head(weights)

PatientWeight <- merge(patients, weights, by="PatientID")

##########################################
##########################################
##########################################

#reading from text files

chol_txt <- read.table("Week3/Data/chol.txt")

colnames(chol_txt) <- c("ldl", "hdl", "trig", "age", "gender", "smoke")

#chicking what type each column/variable is
sapply(chol_txt, class)
str(chol_txt)

##########################################
##########################################
##########################################
#Hierarchical data

library(jsonlite) # Load required package
uri <- "https://api.tfl.gov.uk/line/mode/tube/status"
data <- read_json(uri, simplifyVector = FALSE) # Download data and convert
for (line in data) { # We'll learn about loops later
  cat("Disruptions on",line$name,"\n")
  print(line$disruptions) # Most of the time there are none
}


