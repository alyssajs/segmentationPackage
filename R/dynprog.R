#' Optimal segmentation with dynamic programming
#'
#' @param dataVec A vector of data to be segmented
#' @param maxSegs The maximum number of segments to create
#'
#' @return A cost matrix with the entry at (i, j) being the optimal cost
#'         for creating i segments up to data point j
#' @export
#'
#' @examples sampleVec <- 1:30
#' result <- DYNPROG(sampleVec, 5)
DYNPROG <- function(dataVec, maxSegs)
{
  cost_mat <- matrix(nrow=maxSegs, ncol=length(dataVec))
  change_mat <- matrix(data=NA, nrow=maxSegs, ncol=maxSegs)


  #calculate cost for one segment from beginning to each data point
  cost_mat[1,] <- cumsum(dataVec^2) - cumsum(dataVec)^2/seq_along(dataVec)

  #calculate minimum value of change_mat[1,]
  change_mat[1, 1] <- length(dataVec)


  #Q <- sum of squares of data
  Qvec <- c(0, cumsum(dataVec^2))

  #S <- sum of data
  Svec <- c(0, cumsum(dataVec))

  #for each row in cost matrix
  for(numSegs in 2:maxSegs)
  {
    #calculate optimal loss of new segment

    #t' = all points before current vector location
    #t = current vector location

    #for all t' up to current vector location
    for(stoppingPoint in numSegs:length(dataVec))
    {

      #create variable with indices of potential new changepoint values
      possibleLastSegmentStarts <- numSegs:stoppingPoint

      #calculate c(t',t]
      optPrevLoss <- cost_mat[numSegs-1, possibleLastSegmentStarts-1]

      optCost <- getOptimalCost(Qvec, Svec, possibleLastSegmentStarts, stoppingPoint)

      possibleCosts <- optPrevLoss + optCost

      minCost <- min(possibleCosts)

      cost_mat[numSegs, stoppingPoint] <- minCost


    }
    #endfor

    #get changepoint when considering whole signal
    #change_mat[numSegs, numSegs] <- which.min(cost_mat[numSegs,])
    #for(changepoint in 1:(numSegs-1))
    #{
    #prev changepoint
    #prev_changepoint <- change_mat[numSegs, numSegs-changepoint+1]

    #get changepoint when considering signal up to prev changepoint
    #change_mat[numSegs, numSegs-changepoint] <-
    #which.min(cost_mat[numSegs, 1:(prev_changepoint-1)])
    #}

  }

  return(cost_mat)
  #return cost matrix and changepoint matrix
}


#' Get optimal cost for new segment
#'
#' @param qVec Sum of squares up to the end of the data
#' @param sVec Sum of the data
#' @param start Beginning index
#' @param end   Ending index
#'
#' @return A vector of optimal costs
#' @export
#'
#' @examples testVec <- 1:30
#' qVec <- cumsum(testVec^2)
#' sVec <- cumsum(testVec)
#' cost <- getOptimalCost(qVec, sVec, 1, 4)
getOptimalCost <- function(qVec, sVec, start, end)
{
  startIndex <- start
  endIndex <- end+1
  #qVec[end] = sum of squares up to end
  #qVec[start] = sum of squares up to start
  #sVec[end] = sum of data up to end (go to end + 1 to compensate for the 0
  #added on at the beginning)
  #sVec[start] = sum of data up to start
  costVec <- qVec[endIndex] - qVec[startIndex] - (sVec[endIndex]-sVec[startIndex])^2/(endIndex-startIndex)
  return(costVec)
}
