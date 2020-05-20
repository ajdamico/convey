context("Atk output")
library(survey)
library(IC2)
library(laeken)
data(eusilc)
dati = data.frame(IDd = seq( 10000 , 10000 + nrow( eusilc ) - 1 ) , eusilc)
dati_nz <- subset(dati, eqIncome > 0)

des_eusilc <- svydesign(ids = ~rb030, strata =~db040,  weights = ~rb050, data = eusilc)

des_eusilc <- convey_prep(des_eusilc)
convey_atk <- svyatk(~eqIncome, subset(des_eusilc, eqIncome > 0) )

IC2_atk <- calcAtkinson( x = dati_nz$eqIncome, w = dati_nz$rb050 )$ineq$index

# point estiamte
vardest <- as.numeric( IC2_atk )
convest <- as.numeric(coef(convey_atk)[1])
#domain
# IC2 point estimates
vardestd <- sapply( split(dati_nz, dati_nz$hsize), function(x){ calcAtkinson( x = x$eqIncome, w = x$rb050 )$ineq$index[[1]] } )
vardestd <- as.numeric( vardestd )

# convey point estimates
convestd <- as.numeric( coef( svyby(~eqIncome, ~factor(hsize), subset(des_eusilc, eqIncome > 0), svyatk) ) )

test_that("compare results convey vs vardpoor",{
  expect_equal(vardest,convest)
  expect_equal(vardestd, convestd)
})
