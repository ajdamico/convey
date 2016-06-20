context("Qsr output survey.design and svyrep.design")
library(vardpoor)
library(survey)
data(eusilc) ; names( eusilc ) <- tolower( names( eusilc ) )

des_eusilc <- svydesign(ids = ~rb030, strata =~db040,  weights = ~rb050, data = eusilc)
des_eusilc <- convey_prep(des_eusilc)
des_eusilc_rep <- as.svrepdesign(des_eusilc, type= "bootstrap")
des_eusilc_rep <- convey_prep(des_eusilc_rep)

# database-backed design
library(MonetDBLite)
library(DBI)
dbfolder <- tempdir()
conn <- dbConnect( MonetDBLite::MonetDBLite() , dbfolder )
dbWriteTable( conn , 'eusilc' , eusilc )

dbd_eusilc <-
  svydesign(
    ids = ~rb030 ,
    strata = ~db040 ,
    weights = ~rb050 ,
    data="eusilc",
    dbname=dbfolder,
    dbtype="MonetDBLite"
  )
dbd_eusilc <- convey_prep( dbd_eusilc )


a1 <- svyqsr(~eqincome, design = des_eusilc)
a2 <- svyby(~eqincome, by = ~db040, design = des_eusilc, FUN = svyqsr, deff = FALSE)

b1 <- svyqsr(~eqincome, design = des_eusilc_rep)

b2 <- svyby(~eqincome, by = ~db040, design = des_eusilc_rep, FUN = svyqsr,deff = FALSE)

c1 <- svyqsr( ~ eqincome , design = dbd_eusilc )
c2 <- svyby(~ eqincome, by = ~db040, design = dbd_eusilc, FUN = svyqsr, order = 0.5, percent = 0.6,deff = FALSE)

dbRemoveTable( conn , 'eusilc' )

rel_error1 <- abs(SE(a1)-SE(b1))/SE(a1)
rel_error2 <- max(abs(SE(a2)-SE(b2))/SE(a2))

test_that("output svyarpr",{
  expect_is(coef(a1),"numeric")
  expect_is(coef(a2), "numeric")
  expect_is(coef(b1),"numeric")
  expect_is(coef(b2),"numeric")
  expect_equal(coef(a1), coef(b1))
  expect_equal(coef(a2), coef(b2))
 # expect_lte(rel_error1,.1)
 # expect_lte(rel_error1,.2)
  expect_equal(coef(a1), coef(c1))
  expect_equal(coef(a2), coef(c2))

  expect_is(SE(a1),"numeric")
  expect_is(SE(a2), "numeric")
  expect_is(SE(b1),"numeric")
  expect_is(SE(b2),"numeric")
  expect_lte(confint(a1)[1], coef(a1))
  expect_gte(confint(a1)[2],coef(a1))
  expect_lte(confint(b1)[,1], coef(b1))
  expect_gte(confint(b1)[2], coef(b1))
  expect_equal(sum(confint(a2)[,1]<= coef(a2)),length(coef(a2)))
  expect_equal(sum(confint(a2)[,2]>= coef(a2)),length(coef(a2)))
  expect_equal(sum(confint(b2)[,1]<= coef(b2)),length(coef(b2)))
  expect_equal(sum(confint(b2)[,2]>= coef(b2)),length(coef(b2)))
})
