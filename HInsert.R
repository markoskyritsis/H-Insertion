HInsert <- function(x,y, render = F,w1 = 0.1, w2 = 0.2, w3 = 0.1) {
  fullTourX <- x
  fullTourY <- y
  subtourX <- c()
  subtourY <- c()
  subtourX[1] <- fullTourX[1]
  subtourY[1] <- fullTourY[1]
  
  #First connect the furthest point
  longest <- 0
  longestNode <- Inf
  for (i in 2: length(x)) {
    #Find the farthest point
    t <- findDist(fullTourX[1],fullTourX[i],fullTourY[i],fullTourY[i])
    if (t > longest) {
      longest <- t
      longestNode <- i
    }
  }
  subtourX[2] <- fullTourX[longestNode]
  subtourY[2] <- fullTourY[longestNode]
  
  
  fullTourX <- fullTourX[-longestNode]
  fullTourY <- fullTourY[-longestNode]
  
  fullTourX <- fullTourX[-1]
  fullTourY <- fullTourY[-1]

  subtourX <- c(subtourX,subtourX[1])    
  subtourY <- c(subtourY,subtourY[1])    
  
  
  # best node
  best <- 0
  bestTourX <- subtourX
  bestTourY <- subtourY
  bestNode <- Inf 

  #Initial centroid of full tour  
  centroidXF <- mean(fullTourX)
  centroidYF <- mean(fullTourY)
  j <- 1
  while (length(fullTourX) > 0) {    
    XY <- matrix(c(fullTourX, fullTourY), ncol = 2)  
    cx <- x[chull(XY)]
    cy <- y[chull(XY)]
    
    for (i in 1:length(cx)) {
      tempTourX <- c(subtourX[1:j], fullTourX[chull(XY)[i]])
      
      tempTourX <- c(tempTourX,subtourX[(j+1):length(subtourX)])
      tempTourY <- c(subtourY[1:j], fullTourY[chull(XY)[i]])
      tempTourY <- c(tempTourY,subtourY[(j+1):length(subtourY)])
      
      #get distance from centroid
      centroidX <- mean(tempTourX)
      centroidY <- mean(tempTourY)
      distX <- fullTourX[chull(XY)[i]] - centroidX
      distY <- fullTourY[chull(XY)[i]] - centroidY
      distA <- sqrt(distX^2 + distY^2) 

      #get sd from convex hull
      tempTour <- matrix(c(tempTourX, tempTourY),ncol = 2)
      hullSD <- sd(tempTour[chull(tempTour),]) 


      # get tour length
      perimeter <- getDist(matrix(c(tempTourX,tempTourY),ncol=2))
      # use logistic formula to get the p-value (cost)
      cost <-  1 / (1+exp(-1*(w1*(1/perimeter) - w2 * hullSD  +w3*distA)))   
      if ((cost) > best) {
        best <- cost
        bestNode <- chull(XY)[i]
        
        bestTourX <- tempTourX
        bestTourY <- tempTourY
      }
    }
    if (j == length(subtourX)-1) {
      subtourX <- bestTourX
      subtourY <- bestTourY

      fullTourX <- fullTourX[-bestNode]
      fullTourY <- fullTourY[-bestNode]
      j <- 1
      best <- 0
      bestNode <- Inf
      if (render == T) {
        plot(x,y)
        lines(subtourX,subtourY)
      }
    }
    else
      j <- j + 1
  }
  
  return(matrix(c(bestTourX,bestTourY), ncol = 2))
}