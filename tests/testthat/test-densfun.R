
context("Cdf values")
set.seed(1)
Y<- rchisq(10,3)
H<- rep("str1",10)
PSU<-1:10
w<- rep(2,10)
test<- data.frame(Y=Y, H=H, PSU=PSU, w=w)
des<- survey::svydesign(id=~PSU, strata = ~H, weights=w, data=test)
des <- convey_prep( des )
a<- densfun(~Y, des, x=3, fun="F")
test_that("output no intervalo [0,1]", {
expect_lte(a,1)
expect_gte(a,0)
})
