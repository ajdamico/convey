context("Rmpg output survey.design and svyrep.design")
library(laeken)
library(survey)


data(api)
dstrat1<-convey_prep(svydesign(id=~1,data=apistrat))

expect_error(svyrmpg(~api00, design=dstrat1))

test_that("svyrmpg works on unweighted designs",{
	svyrmpg(~api00, design=dstrat1, percent=1)
})


data(eusilc) ; names( eusilc ) <- tolower( names( eusilc ) )

des_eusilc <- svydesign(ids = ~rb030, strata =~db040,  weights = ~rb050, data = eusilc)
des_eusilc <- convey_prep(des_eusilc)
des_eusilc_rep <- as.svrepdesign(des_eusilc, type= "bootstrap")
des_eusilc_rep <- convey_prep(des_eusilc_rep)

a1 <- svyrmpg(~eqincome, design = des_eusilc)
a2 <- svyby(~eqincome, by = ~hsize, design = subset( des_eusilc , hsize < 8 ) , FUN = svyrmpg)

b1 <- svyrmpg(~eqincome, design = des_eusilc_rep)

b2 <- svyby(~eqincome, by = ~hsize, design = subset( des_eusilc_rep, hsize < 8 ) , FUN = svyrmpg)

rel_error1 <- abs(SE(a1)-SE(b1))/SE(a1)
rel_error2 <- max(abs(SE(a2)-SE(b2))/SE(a2))

test_that("output svyrmpg",{
  expect_is(coef(a1),"numeric")
  expect_is(coef(a2), "numeric")
  expect_is(coef(b1),"numeric")
  expect_is(coef(b2),"numeric")
  expect_equal(coef(a1), coef(b1))
  expect_equal(coef(a2), coef(b2))
  #expect_lte(rel_error1,.1)
  #expect_lte(rel_error1,.2)
  expect_is(SE(a1),"matrix")
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

test_that("database svyrmpg",{
skip_on_cran()

	# database-backed design
	library(RSQLite)
	library(DBI)
	dbfile <- tempfile()
	conn <- dbConnect( RSQLite::SQLite() , dbfile )
	dbWriteTable( conn , 'eusilc' , eusilc )

	dbd_eusilc <-
	  svydesign(
		ids = ~rb030 ,
		strata = ~db040 ,
		weights = ~rb050 ,
		data="eusilc",
		dbname=dbfile,
		dbtype="SQLite"
	  )
	dbd_eusilc <- convey_prep( dbd_eusilc )

	c1 <- svyrmpg( ~ eqincome , design = dbd_eusilc )
	c2 <- svyby(~ eqincome, by = ~hsize, design = subset( dbd_eusilc , hsize < 8 ) , FUN = svyrmpg)

	dbRemoveTable( conn , 'eusilc' )
		dbDisconnect( conn )

	  expect_equal(coef(a1), coef(c1))
	  expect_equal(coef(a2), coef(c2))
	  expect_equal(SE(a1), SE(c1))
	  expect_equal(SE(a2), SE(c2))

# compare subsetted objects to svyby objects
sub_des <- svyrmpg( ~eqincome , design = subset( des_eusilc , hsize == 1) )
sby_des <- svyby( ~eqincome, by = ~hsize, design = subset( des_eusilc , hsize < 8 ) , FUN = svyrmpg)
sub_rep <- svyrmpg( ~eqincome , design = subset( des_eusilc_rep , hsize == 1) )
sby_rep <- svyby( ~eqincome, by = ~hsize, design = subset( des_eusilc_rep , hsize < 8 ) , FUN = svyrmpg)

# subsets equal svyby
  expect_equal(as.numeric(coef(sub_des)), as.numeric(coef(sby_des))[1])
  expect_equal(as.numeric(coef(sub_rep)), as.numeric(coef(sby_rep))[1])
  expect_equal(as.numeric(SE(sub_des)), as.numeric(SE(sby_des))[1])
  expect_equal(as.numeric(SE(sub_rep)), as.numeric(SE(sby_rep))[1])

  # coefficients should match across svydesign & svrepdesign
  expect_equal(as.numeric(coef(sub_des)), as.numeric(coef(sby_rep))[1])

  # coefficients of variation should be within five percent
  cv_dif <- abs(cv(sub_des)-cv(sby_rep)[1])
  expect_lte(cv_dif,5)





# second run of database-backed designs #

  # database-backed design
  library(RSQLite)
  library(DBI)
  dbfile <- tempfile()
  conn <- dbConnect( RSQLite::SQLite() , dbfile )
  dbWriteTable( conn , 'eusilc' , eusilc )

  dbd_eusilc <-
    svydesign(
      ids = ~rb030 ,
      strata = ~db040 ,
      weights = ~rb050 ,
      data="eusilc",
      dbname=dbfile,
      dbtype="SQLite"
    )

  dbd_eusilc <- convey_prep( dbd_eusilc )

  # create a hacky database-backed svrepdesign object
  # mirroring des_eusilc_rep
  dbd_eusilc_rep <-
    svrepdesign(
      weights = ~ rb050,
      repweights = des_eusilc_rep$repweights ,
      scale = des_eusilc_rep$scale ,
      rscales = des_eusilc_rep$rscales ,
      type = "bootstrap" ,
      data = "eusilc" ,
      dbtype="SQLite" ,
      dbname = dbfile ,
      combined.weights = FALSE
    )

  dbd_eusilc_rep <- convey_prep( dbd_eusilc_rep )

  sub_dbd <- svyrmpg( ~eqincome , design = subset( dbd_eusilc , hsize == 1) )
  sby_dbd <- svyby( ~eqincome, by = ~hsize, design = subset( dbd_eusilc , hsize < 8 ) , FUN = svyrmpg)
  sub_dbr <- svyrmpg( ~eqincome , design = subset( dbd_eusilc_rep , hsize == 1) )
  sby_dbr <- svyby( ~eqincome, by = ~hsize, design = subset( dbd_eusilc_rep , hsize < 8 ) , FUN = svyrmpg)

  dbRemoveTable( conn , 'eusilc' )
		dbDisconnect( conn )


  # compare database-backed designs to non-database-backed designs
  # dbi subsets equal non-dbi subsets
    expect_equal(coef(sub_des), coef(sub_dbd))
    expect_equal(coef(sub_rep), coef(sub_dbr))
    expect_equal(SE(sub_des), SE(sub_dbd))
    expect_equal(SE(sub_rep), SE(sub_dbr))
 


  # compare database-backed subsetted objects to database-backed svyby objects
  # dbi subsets equal dbi svyby
    expect_equal(as.numeric(coef(sub_dbd)), as.numeric(coef(sby_dbd))[1])
    expect_equal(as.numeric(coef(sub_dbr)), as.numeric(coef(sby_dbr))[1])
    expect_equal(as.numeric(SE(sub_dbd)), as.numeric(SE(sby_dbd))[1])
    expect_equal(as.numeric(SE(sub_dbr)), as.numeric(SE(sby_dbr))[1])
 

})

