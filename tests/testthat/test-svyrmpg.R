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
#domain
vardpoor_rmpgd <- linrmpg(Y = "eqIncome", id = "IDd", weight = "rb050", Dom = c("db040"),    dataset = dati)
#  point estimates
vardestd<-unlist(vardpoor_rmpgd$value$rmpg)
#  se estimates
varsed<-sapply(data.frame(vardpoor_rmpgd$lin)[,2:10],function(t) SE_lin2(t,des_eusilc))
attributes (varsed) <- NULL
# library convey
fun_rmpgd <- survey:::svyby(~eqIncome, by = ~db040, design = des_eusilc,
  FUN = svyrmpg,deff = FALSE)
convestd<- coef(fun_rmpgd)
attributes(convestd) <- NULL
convsed<- survey:::SE(fun_rmpgd)



test_that("compare results convey vs vardpoor",{
  expect_equal(vardest,100*convest)
  expect_equal(varse, 100*convse)
  expect_equal(vardestd, 100*convestd)
  expect_equal(varsed, 100*convsed )
})
