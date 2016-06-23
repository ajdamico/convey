context("Poormed output survey.design and svyrep.design")
library(vardpoor)
library(survey)
data(eusilc) ; names( eusilc ) <- tolower( names( eusilc ) )

des_eusilc <- svydesign(ids = ~rb030, strata =~db040,  weights = ~rb050, data = eusilc)
des_eusilc <- convey_prep(des_eusilc)
des_eusilc_rep <- as.svrepdesign(des_eusilc, type= "bootstrap")
des_eusilc_rep <- convey_prep(des_eusilc_rep)

a1 <- svypoormed(~eqincome, design = des_eusilc)
a2 <- svyby(~eqincome, by = ~db040, design = des_eusilc, FUN = svypoormed, deff = FALSE)

b1 <- svypoormed(~eqincome, design = des_eusilc_rep)
b2 <- svyby(~eqincome, by = ~db040, design = des_eusilc_rep, FUN = svypoormed, deff = FALSE)

cv_dif1 <- 100*abs(cv(a1)-cv(b1))
cv_diff2 <- 100*max(abs(cv(a2)-cv(b2)))

test_that("output svypoomed",{
  expect_is(coef(a1),"numeric")
  expect_is(coef(a2), "numeric")
  expect_is(coef(b1),"numeric")
  expect_is(coef(b2),"numeric")
  expect_equal(coef(a1), coef(b1))
  expect_equal(coef(a2), coef(b2))
  expect_lte(cv_dif1,5)
  expect_lte(cv_diff2,5)
  expect_equal(coef(a1), coef(c1))
  expect_equal(coef(a2), coef(c2))
  expect_equal(SE(a1), SE(c1))
  expect_equal(SE(a2), SE(c2))
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






# library(MonetDBLite) is only available on 64-bit machines,
# so do not run this block of code in 32-bit R
if( .Machine$sizeof.pointer > 4 ){


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


	c1 <- svypoormed( ~ eqincome , design = dbd_eusilc )
	c2 <- svyby(~ eqincome, by = ~db040, design = dbd_eusilc, FUN = svypoormed, deff = FALSE)

	dbRemoveTable( conn , 'eusilc' )

	test_that("database svypoomed",{
	  expect_equal(coef(a1), coef(c1))
	  expect_equal(coef(a2), coef(c2))
	  expect_equal(SE(a1), SE(c1))
	  expect_equal(SE(a2), SE(c2))
	}

}