#Reading in library
library(tidyverse)


#Task 1

#a)
vol.cyl <- function(r, h){
  pi*r^2*h
}

#b)
unused.volume <- function(r,h,n){
  #Defining dimensions of cuboid box
  length <- 2*r*n
  width <- 2*r
  height <- h
  
  #calculating volume of cuboid box for n
  totalvol_cuboid <-  length*width*height
  
  #calculating total volume of n cylinders using function from a)
  totalvol_cylinders <- vol.cyl(r,h)*n
  
  #calculating unused volume
  totalvol_cuboid-totalvol_cylinders
  
}

#example: unused volume when radius is 3 and height is 10 and n is 5
unused.volume(3,10,5)

##################################
##################################
##################################
##################################
##################################
##################################

#Task 2

##Please pardon how convoluted this is

#This image is very noisy
image(img, col=grey(seq(0,1,len=256)))

#Creating function with ifelse statement that only works if d is odd or if the number of rows is greater than d
denoise <- function(I, d) {
  if (!d %% 2) {
    stop("ERROR: Your number is even - enter an odd number")
  } else if (nrow(I) < d) {
    stop("ERROR: The dimensions of your matrix must be larger than dxd")
  }
  else {
    
    #Double for loop
    
    #creating vector to store values
    output <- vector('list', nrow(I) - (d - 1))
    
    #Double for loop to create a list of lists of all the dxd boxes in the matrix
    for (i in seq_len(nrow(I) - d + 1) - 1) {
      for (j in seq_len(ncol(I) - d + 1) - 1) {
        loopingout <- I[1:d + i, 1:d + j]        #output of individual loops
        output[[i + 1]][[j + 1]] <- replace(loopingout, ceiling(length(loopingout)/2), median(loopingout)) #replacing middle fig with median of dxd box
      }
    }
    
    #Extracting all of of the dxd lists and concatenating them by row
    matrixfigs <- matrix(unlist(output), ncol = d, byrow = T) %>%
      as.matrix()
    
    #selecting the third column, which is where those median values are (this would need altering if d!=5)
    matrixfigs <- matrixfigs[, 3]
    
    #selecting every fifth value, which is the median value, and storing in J (this would need altering if d!=5)
    J <- matrixfigs[seq(3, length(matrixfigs), d)]
    
    #the figures are extracted in a single row, so creating a matrix of the number of rows required
    J <- matrix(J, nrow = (nrow(I)-d+1))
    
    #transposing the final image, storing in matrix called img.denoised
    img.denoised <- t(J)
  }
}

image(denoise(img,5), col=grey(seq(0,1,len=256)))

##################################
##################################
##################################
##################################
##################################
##################################

#Task 3

#using lapply instead of resorting to loops
#guarantees returning as a list
pascal <- function(n){
  lapply(0:(n-1), function(i) choose(i, 0:i))
}

pascal(3)

##################################
##################################
##################################
##################################
##################################
##################################

#Task 4

##creating function using ifelse statement
tp <- function(x, x0, r){
  ifelse(x>x0, (x-x0)^r, 0)
}

#it works!
tp(4, 2, 2)

#also works if x has length <1
tp(-2:55, 2, 2)

##################################
##################################
##################################
##################################
##################################
##################################

#Task 5

#a)
#creating function called bspline
bspline <- function(x, delta){
  delta <- 1 #assigning 1 as default value for delta
   (1/(2*delta^2))*(tp(x, 0, 2)-3*tp(x, delta, 2)+3*tp(x, 2*delta, 2)-tp(x, 3*delta, 2))
  }

#b)
#creating a sequence of equally-spaced numbers from -1 to 4
x <- seq(from = -1, to = 4, length.out = 250)

#plotting
plot(bspline(x), 
     type = "l",
     xlab = "x",
     ylab = expression("B"[1]*(x)),
     xaxt="n")

##################################
##################################
##################################
##################################
##################################
##################################

# Task 6

#a) 
projection <- function(x, k) {
  
  #creating  delta function
  delta <- (max(x)-min(x))/(k-2)
  
  #creating sequence called t
  t <- seq(min(x)-2*delta, max(x)-delta, length.out=k)
  
  
  #creating matrix to store function values
    Z <- matrix(nrow=length(x), ncol=k)
  
  #create matrix Z
    for (i in seq_along(x))
     for(j in seq_along(t))
      Z[i,j] <- bspline(x[i]-t[j])


##Projection matrix
  #Tried to get the projection matrix with some insight on the system of linear equations but didn't get there, sadly. I know there's a solve() call in there somewhere
   # PA <- t(Z)%*%Z
   # PB <- t(Z)%*%Z
   # P2 <- solve(PA, PB)
    
    P <- Z %*% (t(Z)%*%Z) %*% t(Z)
    
# Returning the entries as a list
object <- list(x, delta, Z, P)

#Creating class 'projection#
class(object) <- "projection"
object
  
}

#b)
plot.projection <- function(object){
  if (!inherits(object,"projection"))
    stop("Function only works for an object of class projection")
  
  x <- projection
  y <- responsevector
  
 Py <-  plot(x,y)
}


#c)
#Was really struggling with this one, so ended up going the ggplot route

ggplot(toplot, aes(x=times, y = accel))+
  geom_point(shape =1) +
  geom_smooth(method="loess")+
  theme_bw()


