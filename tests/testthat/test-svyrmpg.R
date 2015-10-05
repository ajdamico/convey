context("rmpg output")
library(vardpoor)
data(eusilc)
dati = data.frame(1:nrow(eusilc), eusilc)
colnames(dati)[1] <- "IDd"

des_eusilc <- survey:::svydesign(ids = ~db040, weights = ~rb050, data = eusilc)

des_eusilc <- convey_prep(des_eusilc)
dati <- data.frame(IDd = 1:nrow(eusilc), eusilc)
vardpoor_linrmpgw <- linrmpg(Y="eqIncome", id="IDd", weight = "rb050", Dom=NULL,
  dataset=dati, percentage=60, order_quant=50)

vardest<- vardpoor_linrmpgw$value
attributes(vardest)<- NULL
vardest<- unlist(vardest)
varse<- SE_lin2(vardpoor_linrmpgw$lin$lin_rmpg, des_eusilc)
attributes(varse)<- NULL
fun_svyrmpgw <- svyrmpg(~eqIncome, design = des_eusilc, 0.5, 0.6)
convest<-coef(fun_svyrmpgw)
attributes(convest)<-NULL
convse<- survey:::SE(fun_svyrmpgw)
attributes(convse)<-NULL

test_that("compare results convey vs vardpoor",{
  expect_equal(vardest,100*convest)
  expect_equal(varse, 100*convse)
})
