A = matrix(c(5,4,3,1,1,0,3,1,0,6,8,1,4,7,9,1),nrow = 4)

A

solve(A)

crossprod(A)
t(A)%*%A

######################

df <- read.table("Week3/saheart.txt", header = TRUE, sep = ",")

is.na(df)
##########################

x <- runif(50,1,99)
y <- runif(50,1,99)

mean_x=mean(x)
mean_y=mean(y)

sd_x=sd(x)
sd_y=sd(y)

length(x)

(mean_x - mean_y)/sqrt((sd_x/length(x))+(sd_y/length(y)))

t.test(x,y)

##########################
#read in dataset
load(file = "Week3/Data/starwars.RData")

str(starwars)
##########################

#eye colour - blue
Lukeseyecolour <- starwars[1,6]
##############################

#characters taller than Han Solo
Hansoloheight <- starwars[14,2]

tallerthanHan <- subset(starwars, height>Hansoloheight)

length(tallerthanHan)
###############################

#Creating new variable BMI
starwars <- transform(starwars, BMI=10000*mass/height^2)

permut <- order(starwars$BMI)
starwars <- starwars[permut,]
head(starwars)
#Not counting NAs, Jabba has the highest BMI

#homeworld is Kashyyyk
starwars[starwars$name == "Chewbacca",]

Chewbaccashome <- starwars[starwars$homeworld == "Kashyyyk",]
Chewbaccashome <- na.omit(Chewbaccashome$name)
