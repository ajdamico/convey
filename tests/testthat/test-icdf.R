
context("Icdf output")
set.seed(1)
Y<- rchisq(10,3)
H<- rep("str1",10)
PSU<-1:10
w<- rep(2,10)
test<- data.frame(Y=Y, H=H, PSU=PSU, w=w)
des<- survey::svydesign(id=~PSU, strata = ~H, weights=w, data=test)
des <- convey_prep( des )
a <- icdf(~Y, des, 5)

test_that("out components", {
  expect_is(a,"cvystat")
  expect_equal(length(a),1)
  expect_is(a[1],"numeric")
  expect_equal(length(attr(a,"lin")),nrow(test))
  expect_named(attributes(a),c("class","lin","var","statistic"))
  expect_lte(a[1],1)
  expect_gte(a[1],0)
})
