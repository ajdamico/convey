context("svyrenyi output survey.design and svyrep.design")

library(laeken)
library(survey)
data(api)
data(eusilc) ; names( eusilc ) <- tolower( names( eusilc ) )

for ( this_eps in c( 0.5 , 1 , 2 ) ){


	dstrat1<-convey_prep(svydesign(id=~1,data=apistrat))
	test_that("svyrenyi works on unweighted designs",{
		svyrenyi(~api00, design=dstrat1, epsilon = this_eps)
	})


	des_eusilc <- svydesign(ids = ~rb030, strata =~db040,  weights = ~rb050, data = eusilc)
	des_eusilc <- convey_prep(des_eusilc)
	des_eusilc_rep_save <- des_eusilc_rep <-as.svrepdesign(des_eusilc, type= "bootstrap" , replicates = 30)
	des_eusilc_rep <- convey_prep(des_eusilc_rep)

	des_eusilc <- subset( des_eusilc , eqincome > 0 )
	des_eusilc_rep <- subset( des_eusilc_rep , eqincome > 0 )



	a1 <- svyrenyi(~eqincome, design = des_eusilc , epsilon = this_eps )
	a2 <- svyby(~eqincome, by = ~hsize, design = des_eusilc, FUN = svyrenyi , epsilon = this_eps )

	b1 <- svyrenyi(~eqincome, design = des_eusilc_rep , epsilon = this_eps )

	b2 <- svyby(~eqincome, by = ~hsize, design = des_eusilc_rep, FUN = svyrenyi , epsilon = this_eps )

	cv_dif1 <- abs(cv(a1)-cv(b1))
	cv_diff2 <- max(abs(SE(a2)-SE(b2)))

	test_that("output svyrenyi",{
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

		dbd_eusilc <- subset( dbd_eusilc , eqincome > 0 )

		c1 <- svyrenyi( ~ eqincome , design = dbd_eusilc , epsilon = this_eps )
		c2 <- svyby(~ eqincome, by = ~hsize, design = dbd_eusilc, FUN = svyrenyi , epsilon = this_eps )

		dbRemoveTable( conn , 'eusilc' )
		dbDisconnect( conn )

		test_that("database svyrenyi",{
		  expect_equal(coef(a1), coef(c1))
		  expect_equal(coef(a2), coef(c2))
		  expect_equal(SE(a1), SE(c1))
		  expect_equal(SE(a2), SE(c2))
		})


	# compare subsetted objects to svyby objects
	sub_des <- svyrenyi( ~eqincome , design = subset( des_eusilc , hsize == 1 ) , epsilon = this_eps )
	sby_des <- svyby( ~eqincome, by = ~hsize, design = des_eusilc, FUN = svyrenyi , epsilon = this_eps )
	sub_rep <- svyrenyi( ~eqincome , design = subset( des_eusilc_rep , hsize == 1) , epsilon = this_eps )
	sby_rep <- svyby( ~eqincome, by = ~hsize, design = des_eusilc_rep, FUN = svyrenyi , epsilon = this_eps )

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

		dbd_eusilc <- subset( dbd_eusilc , eqincome > 0 )

		# create a hacky database-backed svrepdesign object
		# mirroring des_eusilc_rep_save
		dbd_eusilc_rep <-
			svrepdesign(
				weights = ~ rb050,
				repweights = des_eusilc_rep_save$repweights ,
				scale = des_eusilc_rep_save$scale ,
				rscales = des_eusilc_rep_save$rscales ,
				type = "bootstrap" ,
				data = "eusilc" ,
				dbtype="SQLite" ,
				dbname = dbfile ,
				combined.weights = FALSE
			)

		dbd_eusilc_rep <- convey_prep( dbd_eusilc_rep )

		dbd_eusilc_rep <- subset( dbd_eusilc_rep , eqincome > 0 )

		sub_dbd <- svyrenyi( ~eqincome , design = subset( dbd_eusilc , hsize == 1 ) , epsilon = this_eps )
		sby_dbd <- svyby( ~eqincome, by = ~hsize, design = dbd_eusilc, FUN = svyrenyi , epsilon = this_eps )
		sub_dbr <- svyrenyi( ~eqincome , design = subset( dbd_eusilc_rep , hsize == 1 ) , epsilon = this_eps )
		sby_dbr <- svyby( ~eqincome, by = ~hsize, design = dbd_eusilc_rep, FUN = svyrenyi , epsilon = this_eps )

		dbRemoveTable( conn , 'eusilc' )
		dbDisconnect( conn )


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


}


