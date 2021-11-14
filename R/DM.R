#Return Manhattan distance
manhattanDist <- function (src, dest)
{
  return (abs(dest[1] - src[1]) + abs(dest[2] - src[2]))
}

#return the closest package from the current position
selectPackage <- function (currentPos, packageMatrix)
{
  distance <- NULL
  shortestDist <- NULL
  closestP <- NULL
  for(i in 1:nrow(packageMatrix))
  {
    newPM <- packageMatrix[c(i), c(1,2)]
    if(packageMatrix[i,5]==0)
    {
      distance <- manhattanDist(currentPos, newPM)
      if((distance<shortestDist) || is.null(shortestDist))
      {
        shortestDist = distance
        closestPackage <- packageMatrix[c(i), c(1,2)]
        closestDelivery <- packageMatrix[c(i), c(3,4)]
      }
    }
  }
  return (closestPackage)
}

#check if the node exists in the Visited set or not 
isInSet = function(node, set) {
  if (length(set) == 0) {
    return (0)
  }
  for (i in 1:length(set)) {
    if (all(node == c(set[[i]]$x, set[[i]]$y))) {
      return (i)
    }
  }
  return (0)
}

#Add the node to the visited set 
addNode <- function (expanded, g, neighbourNode, destination, frontier, directionNumber) {
  h = manhattanDist(neighbourNode, destination)
  frontierX = sapply(frontier, function(item)item$x)
  frontierY = sapply(frontier, function(item)item$y)
  neighbourIndex = which(frontierX == neighbourNode[1] & frontierY == neighbourNode[2])
  if (length(neighbourIndex) != 0) {
    if (frontier[[neighbourIndex]]$g >= g) 
      {
        frontier[[neighbourIndex]]$g = g
        frontier[[neighbourIndex]]$f = g+h
        frontier[[neighbourIndex]]$path = append(expanded$path, c(directionNumber))
      }
    } else {
    frontier = append(frontier, list(list(
      x = neighbourNode[1],
      y = neighbourNode[2],
      g = g,
      h = h,
      f = g+h,
      path = append(expanded$path, c(directionNumber))
    )))
  }
  return (frontier)
}

#A star algorithm 
aStarSearch <- function (roads, car, destination) {
  h = manhattanDist(c(car$x, car$y), destination)
  frontier = list(list(
    x = car$x,
    y = car$y,
    g = 0,
    h = h,
    f = h,
    path = c()
  ))
  visitedSet = list()
  
  
  while (1) {
    #Find the best cost 
    scores=sapply(frontier,function(item)item$f)
    expandedIndex = which.min(scores)
    expanded = frontier[[expandedIndex]]
    frontier = frontier[-expandedIndex]
    visitedSet = append(visitedSet, list(expanded))
    
    currNodeX = expanded$x
    currNodeY = expanded$y
    
    #Expand the nodes in 4 directions 
    nodeDown = expanded$y-1
    nodeUp = expanded$y+1
    nodeLeft = expanded$x-1
    nodeRight = expanded$x+1
    
    #If the current node(x,y) is destination then return the path
    if (currNodeX == destination[1] & currNodeY == destination[2]) {
      return (expanded$path[1])
    }
    
    #Add node that is below to the frontier(If not in the visited set)
    if (nodeDown > 0 & !isInSet(c(currNodeX, nodeDown), visitedSet)) {
      neighbourNode = c(currNodeX, nodeDown)
      g = expanded$g + roads$vroads[currNodeX, nodeDown]
      frontier = addNode(expanded, g, neighbourNode, destination, frontier, 2)
    }
    #Add node that is above to the frontier(If not in the visited set)
    if (nodeUp <= 10 & !isInSet(c(currNodeX, nodeUp), visitedSet)) {
      neighbourNode = c(currNodeX, nodeUp)
      g = expanded$g + roads$vroads[currNodeX, currNodeY]
      frontier = addNode(expanded, g, neighbourNode, destination, frontier, 8)
    }
    #Add node that is to the left to the frontier(If not in the visited set)
    if (nodeLeft > 0 & !isInSet(c(nodeLeft, currNodeY), visitedSet)) {
      neighbourNode = c(nodeLeft, currNodeY)
      g = expanded$g + roads$hroads[nodeLeft, currNodeY]
      frontier = addNode(expanded, g, neighbourNode, destination, frontier, 4)
    }
    #Add node that is to the right to the frontier(If not in the visited set)
    if (nodeRight <= 10 & !isInSet(c(nodeRight, currNodeY), visitedSet)) {
      neighbourNode = c(nodeRight, currNodeY)
      g = expanded$g + roads$hroads[currNodeX, currNodeY]
      frontier = addNode(expanded, g, neighbourNode, destination, frontier, 6)
    }
  }
}


myFunction <- function (roads, car, packages) {
  if (car$load == 0) {
    #If car is not carrying a package(0), pickup the closest package
    destination = selectPackage(c(car$x, car$y), packages)
    #destination = c(packages[nextPackageTopick, 1], packages[nextPackageTopick, 2])
  } else {
    # astar search for current point till destination
    destination = c(packages[car$load, 3], packages[car$load, 4])
  }
  if (car$x == destination[1] & car$y == destination[2]) {
    car$nextMove = 5
    return (car)
  }
  nm = aStarSearch(roads, car, destination)
  car$nextMove = nm
  return(car)
}



#' testDM
#'
#' Use this to debug under multiple circumstances and to see how your function compares with the par function
#' The mean for the par function (with n=500) on this is 172.734, and the sd is approximately 39.065.
#'
#' Your final result will be based on how your function performs on a similar run of 500 games, though with
#' a different seed used to select them.
#'
#' This set of seeds is chosen so as to include a tricky game that has pick ups and deliveries on the same
#' spot. This will occur in the actual games you are evaluated on too.
#'
#' While this is dependent on the machine used, we expect your function to be able to run the 500 evaluation games on
#' the evaluation machine in under 4 minutes (250 seconds). If the evaluation machine is slower than expected,
#' this will be altered so that the required time is 25% slower than the par function.
#'
#' The par function takes approximately 96 seconds on my laptop (with n=500 and verbose=0).
#'
#' @param myFunction The function you have created to control the Delivery Man game.
#' @param verbose Set to 0 for no output, 1 for a summary of the results of the games played (mean,
#' standard deviation and time taken), and 2 for the above plus written output detailing seeds used and the
#' runDeliveryMan output of the result of each game.
#' @param returnVec Set to TRUE if you want the results of the games played returned as a vector.
#' @param n The number of games played. You will be evaluated on a set of 500 games, which is also the default here.
#' @param timeLimit The time limit. If this is breached, a NA is returned.
#' @return If returnVec is false, a scalar giving the mean of the results of the games played. If returnVec is TRUE
#' a vector giving the result of each game played. If the time limit is breached, a NA is returned.
#' @export
testDM=function(myFunction,verbose=0,returnVec=FALSE,n=500,seed=21,timeLimit=250){
  if (!is.na(seed))
    set.seed(seed)
  seeds=sample(1:25000,n)
  startTime=Sys.time()
  aStar=sapply(seeds,function(s){
    midTime=Sys.time()
    if (as.numeric(midTime)-as.numeric(startTime)>timeLimit) {
      cat("\nRun terminated due to slowness.")
      return (NA)
    }
    set.seed(s)
    if (verbose==2)
      cat("\nNew game, seed",s)
    runDeliveryMan(myFunction,doPlot=F,pause=0,verbose=verbose==2)
  })
  endTime=Sys.time()
  if (verbose>=1){
    cat("\nMean:",mean(aStar))
    cat("\nStd Dev:",sd(aStar))
    cat("\nTime taken:",as.numeric(endTime)-as.numeric(startTime),"seconds.")
  }
  if (returnVec)
    return(aStar)
  else
    return (mean(aStar))
}

#' Run Delivery Man
#'
#' Runs the delivery man game. In this game, deliveries are randomly placed on a city grid. You
#' must pick up and deliver the deliveries as fast as possible under changing traffic conditions.
#' Your score is the time it takes for you to complete this task. To play manually pass manualDM
#' as the carReady function and enter the number pad direction numbers to make moves.
#' @param carReady Your function that takes three arguments: (1) a list of two matrices giving the
#' traffic conditions. The first matrix is named 'hroads' and gives a matrix of traffic conditions
#' on the horizontal roads. The second matrix is named 'vroads' and gives a matrix of traffic
#' conditional on the vertical roads. <1,1> is the bottom left, and <dim,dim> is the top right.
#'(2) a list providing information about your car. This
#' list includes the x and y coordinates of the car with names 'x' and 'y', the package the car
#' is carrying, with name 'load' (this is 0 if no package is being carried), a list called
#' 'mem' that you can use to store information you want to remember from turn to turn, and
#' a field called nextMove where you will write what you want the car to do. Moves are
#' specified as on the number-pad (2 down, 4 left, 6 right, 8 up, 5 stay still). (3) A
#' matrix containing information about the packages. This contains five columns and a row for each
#' package. The first two columns give x and y coordinates about where the package should be picked
#' up from. The next two columns give x and y coordinates about where the package should be
#' delivered to. The final column specifies the package status (0 is not picked up, 1 is picked up but not
#' delivered, 2 is delivered).
#' Your function should return the car object with the nextMove specified.
#' @param dim The dimension of the board. You will be scored on a board of dimension 10. Note that
#' this means you will have to remove duplicated nodes from your frontier to keep your AStar
#' computationally reasonable! There is a time limit for how long an average game can be run in, and
#' if your program takes too long, you will penalized or even fail.
#' @param turns The number of turns the game should go for if deliveries are not made. Ignore this
#' except for noting that the default is 2000 so if you have not made deliveries after 2000 turns
#' you fail.
#' @param doPlot Specifies if you want the game state to be plotted each turn.
#' @param pause The pause period between moves. Ignore this.
#' @param del The number of deliveries. You will be scored on a board with 5 deliveries.
#' @return A string describing the outcome of the game.
#' @export
runDeliveryMan <- function (carReady=myFunction,dim=10,turns=2000,
                            doPlot=T,pause=0.1,del=5,verbose=T) {
  roads=makeRoadMatrices(dim)
  car=list(x=1,y=1,wait=0,load=0,nextMove=NA,mem=list())
  packages=matrix(sample(1:dim,replace=T,5*del),ncol=5)
  packages[,5]=rep(0,del)
  for (i in 1:turns) {
    roads=updateRoads(roads$hroads,roads$vroads)
    if (doPlot) {
      makeDotGrid(dim,i)
      plotRoads(roads$hroads,roads$vroads)
      points(car$x,car$y,pch=16,col="blue",cex=3)
      plotPackages(packages)
    }
    if (car$wait==0) {
      if (car$load==0) {
        on=packageOn(car$x,car$y,packages)
        if (on!=0) {
          packages[on,5]=1
          car$load=on
        }
      } else if (packages[car$load,3]==car$x && packages[car$load,4]==car$y) {
        packages[car$load,5]=2
        car$load=0
        if (sum(packages[,5])==2*nrow(packages)) {
          if (verbose)
            cat("\nCongratulations! You suceeded in",i,"turns!")
          return (i)
        }
      }
      car=carReady(roads,car,packages)
      car=processNextMove(car,roads,dim)
    } else {
      car$wait=car$wait-1
    }
    if (pause>0) Sys.sleep(pause)
  }
  cat("\nYou failed to complete the task. Try again.")
  return (NA)
}
#' @keywords internal
packageOn<-function(x,y,packages){
  notpickedup=which(packages[,5]==0)
  onX=which(packages[,1]==x)
  onY=which(packages[,2]==y)
  available=intersect(notpickedup,intersect(onX,onY))
  if (length(available)!=0) {
    return (available[1])
  }
  return (0)
}
#' @keywords internal
processNextMove<-function(car,roads,dim) {
  nextMove=car$nextMove
  if (nextMove==8) {
    if (car$y!=dim) {
      car$wait=roads$vroads[car$x,car$y]
      car$y=car$y+1
    } else {
      warning(paste("Cannot move up from y-position",car$y))
    }
  } else if (nextMove==2) {
    if (car$y!=1) {
      car$y=car$y-1
      car$wait=roads$vroads[car$x,car$y]
    } else {
      warning(paste("Cannot move down from y-position",car$y))
    }
  }  else if (nextMove==4) {
    if (car$x!=1) {
      car$x=car$x-1
      car$wait=roads$hroads[car$x,car$y]
    } else {
      warning(paste("Cannot move left from x-position",car$x))
    }
  }  else if (nextMove==6) {
    if (car$x!=dim) {
      car$wait=roads$hroads[car$x,car$y]
      car$x=car$x+1
    } else {
      warning(paste("Cannot move right from x-position",car$x))
    }
  } else if (nextMove!=5) {
    warning("Invalid move. No move made. Use 5 for deliberate no move.")
  }
  car$nextMove=NA
  return (car)
}

#' @keywords internal
plotPackages=function(packages) {
  notpickedup=which(packages[,5]==0)
  notdelivered=which(packages[,5]!=2)
  points(packages[notpickedup,1],packages[notpickedup,2],col="green",pch=18,cex=3)
  points(packages[notdelivered,3],packages[notdelivered,4],col="red",pch=18,cex=3)
}

#' @keywords internal
makeDotGrid<-function(n,i) {
  plot(rep(seq(1,n),each=n),rep(seq(1,n),n),xlab="X",ylab="Y",main=paste("Delivery Man. Turn ", i,".",sep=""))
}

#' @keywords internal
makeRoadMatrices<-function(n){
  hroads=matrix(rep(1,n*(n-1)),nrow=n-1)
  vroads=matrix(rep(1,(n-1)*n),nrow=n)
  list(hroads=hroads,vroads=vroads)
}

#' @keywords internal
plotRoads<- function (hroads,vroads) {
  for (row in 1:nrow(hroads)) {
    for (col in 1:ncol(hroads)) {
      lines(c(row,row+1),c(col,col),col=hroads[row,col])
    }
  }
  for (row in 1:nrow(vroads)) {
    for (col in 1:ncol(vroads)) {
      lines(c(row,row),c(col,col+1),col=vroads[row,col])
    }
  }
}
#' @keywords internal
updateRoads<-function(hroads,vroads) {
  r1=runif(length(hroads))
  r2=runif(length(hroads))
  for (i in 1:length(hroads)) {
    h=hroads[i]
    if (h==1) {
      if (r1[i]<.05) {
        hroads[i]=2
      }
    }
    else {
      if (r1[i]<.05) {
        hroads[i]=h-1
      } else if (r1[i]<.1) {
        hroads[i]=h+1
      }
    }
    v=vroads[i]
    if (v==1) {
      if (r2[i]<.05) {
        vroads[i]=2
      }
    }
    else {
      if (r2[i]<.05) {
        vroads[i]=v-1
      } else if (r2[i]<.1) {
        vroads[i]=v+1
      }
    }
  }
  list (hroads=hroads,vroads=vroads)
}
