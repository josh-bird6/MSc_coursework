


image(img, col=grey(seq(0,1,len=256)))

#Defining mxn matrix as I

I <- img

image(Itest, col=grey(seq(0,1,len=256)))

x <- 0:250


  # I_test <- I[(3+i):(3+i)]   
  
# for (i in 1:nrow(I)) {
#   # for (j in 1:ncol(I)){
#   df[i] <- median(as.vector(I[1:5, (1+i):(5+i)]))
# }





for (i in seq_along(x)){
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
  # Itest[4,4+seq_along(x)] <- matrixtest2[seq_along(x)]
  # Itest[5,5+seq_along(x)] <- matrixtest3[seq_along(x)]
  # Itest[6,6+seq_along(x)] <- matrixtest4[seq_along(x)]
  # Itest[7,7+seq_along(x)] <- matrixtest5[seq_along(x)]
  # Itest[8,8+seq_along(x)] <- matrixtest6[seq_along(x)]
  # Itest[9,9+seq_along(x)] <- matrixtest7[seq_along(x)]
  # Itest[10,10+seq_along(x)] <- matrixtest8[seq_along(x)]
  # Itest[11,11+seq_along(x)] <- matrixtest9[seq_along(x)]
  # Itest[12,12+seq_along(x)] <- matrixtest10[seq_along(x)]

}


x <- 0:(nrow(I)-5-1)

x <- 0:250

for (i in seq_along(x)) {
  
  matrixtest <- matrix()
  matrixtest[i] <- median(I[1:5, (1 + i):(5 + i)])
  
  Itest <- I
  Itest[3, 3 + seq_along(x)] <- matrixtest[seq_along(x)]
  
}


for (i in seq_len(nrow(I)-n+1)-1L){
  for (j in seq_len(nrow(I)-n+1)-1L){
    Itest <- I[1:n+i, 1:n+j] 
    I[i+1L][j+1L] <- replace(Itest, ceiling(length(Itest)/2), median(Itest))
  }
}




matrixtest <- median(I[1:5, 1:5])

testfunction(I)

test_median

I[2:6,1:5]
I[1:5,2:6]
I[1:5,3:7]
I[1:5,4:8]
I[1:5,5:9]












