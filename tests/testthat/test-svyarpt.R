library(vardpoor)
data(eusilc)
dati = data.frame(1:nrow(eusilc), eusilc)
colnames(dati)[1] <- "IDd"

des_eusilc <- survey:::svydesign(ids = ~db040, weights = ~rb050, data = eusilc)

des_eusilc <- convey_prep(des_eusilc)
dati <- data.frame(IDd = 1:nrow(eusilc), eusilc)
vardpoor_arptw <- linarpt(Y = "eqIncome", id = "IDd", weight = "rb050", Dom = NULL, dataset = dati, percentage = 60, order_quant = 50)

vardest<- vardpoor_arptw$value
attributes(vardest)<- NULL
vardest<- unlist(vardest)
varse<- SE_lin2(vardpoor_arptw$lin$lin_arpt, des_eusilc)
attributes(varse)<- NULL
fun_arptw <- svyarpt(~eqIncome, design = des_eusilc, 0.5, 0.6)
convest<-coef(fun_arptw)
attributes(convest)<-NULL
convse<- SE(fun_arptw)
attributes(convse)<-NULL

test_that("compare results convey vs vardpoor",{
  expect_equal(vardest, convest)
  expect_equal(varse, convse)
})
