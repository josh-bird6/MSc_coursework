moves <- rbind(c( 1, 0),              # There are four possible moves
               c(-1, 0),              # right, left, up and down
               c( 0, 1),
               c( 0,-1))
n <- 100                              # Number of steps            
choices <- sample(4, n, replace=TRUE) # Choose which step type for each step
selected.moves <- moves[choices,]     # Translate to moves
coords <- cbind(x=cumsum(selected.moves[,1]), y=cumsum(selected.moves[,2]))
# Translate to coordinates

#first transforming to data.frame
coords <- data.frame(coords)

coords %>% 
  ggplot(aes(x=x, y=y)) +
  geom_point(size=2)+
  geom_path() +
  labs(title = "Spatial Random Walk")+
  theme_bw()

plot(coords$x, coords$y, 
     type = "o", 
     pch=19, 
     col = "black",
     xlab = "x",
     ylab = "y",
     main = "Spatial Random Walk")
