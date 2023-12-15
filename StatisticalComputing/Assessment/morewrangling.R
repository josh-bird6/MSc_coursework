
x <- matrix(1:9, nrow=3)

##START HERE

#

Itest <- img

n <- 5L

res <- vector('list', nrow(Itest)-(5-1))

for (i in seq_len(nrow(Itest)-n+1)-1L){
  for (j in seq_len(nrow(Itest)-n+1)-1L){
    tmp <- Itest[1:n+i, 1:n+j]
    res[[i+1L]][[j+1L]] <- replace(tmp, ceiling(length(tmp)/2), median(tmp))
  }
}

#Extracting all of of the dxd lists and concatenating them
matrixfigs<- matrix(unlist(res), ncol=5, byrow=T) %>% 
  as.matrix()

#selecting the third column, which is where those median values are
matrixfigs <- matrixfigs[,3]

#selecting every fifth value, which is the median value
matrixfigs<- matrixfigs[seq(3, length(matrixfigs), 5)]

#creating a matrix of the number of elements required
matrixfigstest <- matrix(matrixfigs, nrow =252)

#transposing the final image
img.denoised <- t(matrixfigstest)


image(matrixfigstest, col=grey(seq(0,1,len=256)))










for (i in newseq){
  
  matrixmeds <- matrix()
  
  matrixmeds[i] <- matrixfigs[3, i]
}


matrixtest[i] <- median(as.matrix(I[1:5, (1+i):(5+i)]))
# matrixtest2[i] <- median(as.matrix(I[2:6, (1+i):(5+i)]))
# matrixtest3[i] <- median(as.matrix(I[3:7, (1+i):(5+i)]))
# matrixtest4[i] <- median(as.matrix(I[4:8, (1+i):(5+i)]))
# matrixtest5[i] <- median(as.matrix(I[5:9, (1+i):(5+i)]))
# matrixtest6[i] <- median(as.matrix(I[6:10, (1+i):(5+i)]))
# matrixtest7[i] <- median(as.matrix(I[7:11, (1+i):(5+i)]))
# matrixtest8[i] <- median(as.matrix(I[8:12, (1+i):(5+i)]))
# matrixtest9[i] <- median(as.matrix(I[9:13, (1+i):(5+i)]))
# matrixtest10[i] <- median(as.matrix(I[10:14, (1+i):(5+i)]))

Itest <- I
Itest[3,3+seq_along(x)] <- matrixtest[seq_along(x)]

##########################


m1 <- matrix(1, nrow=2, ncol=3)
 m2 <- matrix(2, nrow=3, ncol=3)
l <- list(m1, m2)
 do.call(rbind, l)