context("Gpg output survey.design and svyrep.design")
library(laeken)
library(survey)


data(api)
dstrat1<-convey_prep(svydesign(id=~1,data=apistrat))
dstrat1 <- update( dstrat1 , sex = ifelse( both == 'Yes' , 'male' , 'female' ) )
test_that("svygpg works on unweighted designs",{
	svygpg(~api00, design=dstrat1, ~sex)
})


data(ses) ; names( ses ) <- gsub( "size" , "size_" , tolower( names( ses ) ) )
des_ses <- svydesign(id=~1, weights=~weights, data=ses)
des_ses <- convey_prep(des_ses)
des_ses_rep <- as.svrepdesign(des_ses, type = "bootstrap")
des_ses_rep <- convey_prep(des_ses_rep)


a1 <- svygpg(~earningshour, des_ses, ~sex)

a2 <- svyby(~earningshour, by = ~location, design = des_ses, FUN = svygpg, sex=~sex, deff = FALSE)

b1 <- svygpg(~earningshour, design = des_ses_rep, ~sex)

b2 <- svyby(~earningshour, by = ~location, design = des_ses_rep,
  FUN = svygpg, sex=~sex, deff = FALSE)

cv_dif1 <- abs(cv(a1)-cv(b1))
pos_est <- coef(a2)> 0
cv_diff2 <- max(abs(SE(a2)[pos_est]/coef(a2)[pos_est]-SE(b2)[pos_est]/coef(b2)[pos_est]))

test_that("output svygpg",{
  expect_is(coef(a1),"numeric")
  expect_is(coef(a2), "numeric")
  expect_is(coef(b1),"numeric")
  expect_is(coef(b2),"numeric")
  expect_equal(coef(a1), coef(b1))
  expect_equal(coef(a2), coef(b2))
  expect_lte(cv_dif1, coef(a1) * 0.05 ) # the difference between CVs should be less than 5% of the coefficient, otherwise manually set it
  expect_lte(cv_diff2, max( coef(a2) ) * 0.1 ) # the difference between CVs should be less than 10% of the maximum coefficient, otherwise manually set it
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



	# database-backed design
	library(RSQLite)
	library(DBI)
	dbfile <- tempfile()
	conn <- dbConnect( RSQLite::SQLite() , dbfile )
	dbWriteTable( conn , 'ses' , ses )
	dbd_ses <- svydesign(id=~1, weights=~weights, data="ses", dbname=dbfile, dbtype="SQLite")
	dbd_ses <- convey_prep( dbd_ses )

	c1 <-  svygpg(formula=~earningshour, design=dbd_ses, sex= ~sex)
	c2 <- svyby(~earningshour, by = ~location, design = dbd_ses, FUN = svygpg, sex=~sex, deff = FALSE)

	dbRemoveTable( conn , 'ses' )

	test_that("database svygpg",{
	  expect_equal(coef(a1), coef(c1))
	  expect_equal(coef(a2), coef(c2))
	  expect_equal(SE(a1), SE(c1))
	  expect_equal(SE(a2), SE(c2))
	})






# compare subsetted objects to svyby objects
sub_des <- svygpg( ~earningshour , sex=~sex, design = subset( des_ses , location == "AT1" ) )
sby_des <- svyby( ~earningshour, sex=~sex, by = ~location, design = des_ses, FUN = svygpg)
sub_rep <- svygpg( ~earningshour, sex=~sex , design = subset( des_ses_rep , location == "AT1" ) )
sby_rep <- svyby( ~earningshour, sex=~sex, by = ~location, design = des_ses_rep, FUN = svygpg)

test_that("subsets equal svyby",{
	expect_equal(as.numeric(coef(sub_des)), as.numeric(coef(sby_des))[1])
	expect_equal(as.numeric(coef(sub_rep)), as.numeric(coef(sby_rep))[1])
	expect_equal(as.numeric(SE(sub_des)), as.numeric(SE(sby_des))[1])
	expect_equal(as.numeric(SE(sub_rep)), as.numeric(SE(sby_rep))[1])

	# coefficients should match across svydesign & svrepdesign
	expect_equal(as.numeric(coef(sub_des)), as.numeric(coef(sby_rep))[1])

	# coefficients of variation should be within five percent
	cv_dif <- abs(cv(sub_des)-cv(sby_rep)[1])
	expect_lte(cv_dif,5)
})




# second run of database-backed designs #

	# database-backed design
	library(RSQLite)
	library(DBI)
	dbfile <- tempfile()
	conn <- dbConnect( RSQLite::SQLite() , dbfile )
	dbWriteTable( conn , 'ses' , ses )

	dbd_ses <- svydesign(id=~1, weights=~weights, data="ses", dbname=dbfile, dbtype="SQLite")

	dbd_ses <- convey_prep( dbd_ses )

	# create a hacky database-backed svrepdesign object
	# mirroring des_ses_rep
	dbd_ses_rep <-
		svrepdesign(
			weights = ~ weights,
			repweights = des_ses_rep$repweights ,
			scale = des_ses_rep$scale ,
			rscales = des_ses_rep$rscales ,
			type = "bootstrap" ,
			data = "ses" ,
			dbtype="SQLite" ,
			dbname = dbfile ,
			combined.weights = FALSE
		)

	dbd_ses_rep <- convey_prep( dbd_ses_rep )

	sub_dbd <- svygpg( ~earningshour, sex=~sex , design = subset( dbd_ses , location == "AT1" ) )
	sby_dbd <- svyby( ~earningshour, sex=~sex, by = ~location, design = dbd_ses, FUN = svygpg)
	sub_dbr <- svygpg( ~earningshour, sex=~sex , design = subset( dbd_ses_rep , location == "AT1" ) )
	sby_dbr <- svyby( ~earningshour, sex=~sex, by = ~location, design = dbd_ses_rep, FUN = svygpg)

	dbRemoveTable( conn , 'ses' )


	# compare database-backed designs to non-database-backed designs
	test_that("dbi subsets equal non-dbi subsets",{
		expect_equal(coef(sub_des), coef(sub_dbd))
		expect_equal(coef(sub_rep), coef(sub_dbr))
		expect_equal(SE(sub_des), SE(sub_dbd))
		expect_equal(SE(sub_rep), SE(sub_dbr))
	})


	# compare database-backed subsetted objects to database-backed svyby objects
	test_that("dbi subsets equal dbi svyby",{
		expect_equal(as.numeric(coef(sub_dbd)), as.numeric(coef(sby_dbd))[1])
		expect_equal(as.numeric(coef(sub_dbr)), as.numeric(coef(sby_dbr))[1])
		expect_equal(as.numeric(SE(sub_dbd)), as.numeric(SE(sby_dbd))[1])
		expect_equal(as.numeric(SE(sub_dbr)), as.numeric(SE(sby_dbr))[1])
	})

