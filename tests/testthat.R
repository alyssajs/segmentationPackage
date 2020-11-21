library(testthat)
library(segmentationPackage)

test_check("segmentationPackage")

test_that("dynprog returns a numeric matrix",
          {
            testVec <- 1:30
            result <- DYNPROG(testVec, 3)
            expect_true(is.numeric(result))
            expect_true(is.matrix(result))
          })
test_that("getOptimalCost returns a numeric vector",
          {
            testVec <- 1:30
            qVec <- cumsum(testVec^2)
            sVec <- cumsum(testVec)
            result <- getOptimalCost(qVec, sVec, 1, 3)
            expect_true(is.numeric(result))
            expect_true(is.vector(result))

          })

test_that("PCA returns two matrices and a vector",
          {
            pcaRes <- PCA(iris[,2:3])
            expect_true(is.matrix(pcaRes$x))
            expect_true(is.matrix(pcaRes$rotation))
            expect_true(is.vector(pcaRes$center))
          })
