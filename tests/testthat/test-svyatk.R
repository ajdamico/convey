context("Atk output")
library(IC2)
library(vardpoor)
data(eusilc)
dati = data.frame(1:nrow(eusilc), eusilc)
colnames(dati)[1] <- "IDd"

des_eusilc <- svydesign(ids = ~rb030, strata =~db040,  weights = ~rb050, data = eusilc)

des_eusilc <- convey_prep(des_eusilc)
convey_atk <- svyatk(~eqIncome, subset(des_eusilc, eqIncome > 0) )

dati <- data.frame(IDd = 1:nrow(eusilc), eusilc)
IC2_atk <- calcAtkinson( x = dati$eqIncome[dati$eqIncome > 0], w = dati$rb050[dati$eqIncome > 0] )$ineq$index

# point estiamte
vardest <- as.numeric( IC2_atk )
convest <- as.numeric(coef(convey_atk)[1])
#domain
# IC2 point estimates
library(plyr)
vardestd <- ddply( subset(dati, eqIncome > 0), .(db040), summarize, fun = calcAtkinson(x = eqIncome, w = rb050)$ineq$index)$fun
# convey point estimates
convestd <- as.numeric( coef( svyby(~eqIncome, ~factor(db040), subset(des_eusilc, eqIncome > 0), svyatk) ) )

test_that("compare results convey vs vardpoor",{
  expect_equal(vardest,convest)
  expect_equal(vardestd, convestd)
})
