
#' Perform principal components analysis
#'
#' @param data.matrix The feature matrix on which to perform PCA
#'
#' @return A rotation matrix, a matrix of loadings, and a center matrix
#' @export
#'
#' @examples PCA(iris[,2:3])
PCA <- function(data.matrix)
{
  #scale
  standardData <- scale(data.matrix, center=TRUE, scale=FALSE)

  #use svd
  decomp <- base::svd(standardData)


  #rotation matrix equivalent
  rot <- decomp$v

  #x matrix equivalent
  xMat <- decomp$u %*% diag(decomp$d)

  result <- list()

  result$rotation <- rot
  result$x <- xMat
  result$center <- colMeans(standardData)

  return(result)

}
