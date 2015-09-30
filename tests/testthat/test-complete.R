library(convey)
context("complete")

x<-c(1,3,5)
names(x)<-as.character(1:3)
n<-as.character(1:10)
a<- complete(x,n)

test_that("output", {
expect_equal(length(a),length(n))
expect_equal(sum(a==0),length(n)-length(x))
expect_named(a)
expect_equal(names(which(a!=0)),names(x))
})


