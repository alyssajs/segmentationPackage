
test_that("PCA returns two matrices and a vector",
          {
            pcaRes <- PCA(iris[,2:3])
            expect_true(is.matrix(pcaRes$x))
            expect_true(is.matrix(pcaRes$rotation))
            expect_true(is.vector(pcaRes$center))
          })
