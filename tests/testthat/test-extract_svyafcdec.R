context("svyafcdec output survey.design and svyrep.design")
library(laeken)
library(survey)


data(api)
apistrat[ , sapply( apistrat, is.integer ) ] <- apply( apistrat[ , sapply( apistrat, is.integer ) ], 2, as.numeric )
dstrat1<-convey_prep(svydesign(id=~1,data=apistrat))
test_that("svyafcdec works on unweighted designs",{
  for (this_dimw in list( NULL, c(.25, .75) )) {
    for ( this_k in c( .5 , 1 ) ){
      for ( this_g in c( 0 , 1 , 2 ) ) {
        svyafcdec( ~api00+pcttest, ~stype , design=dstrat1, cutoffs = list( 400, 90 ), g = this_g, k = this_k, dimw = this_dimw, na.rm = TRUE )
      }
    }
  }
})


test_that("output svyafcdec",{
  skip_on_cran()

  data(eusilc) ; names( eusilc ) <- tolower( names( eusilc ) )
  eusilc[ , sapply( eusilc, is.integer ) ] <- apply( eusilc[ , sapply( eusilc, is.integer ) ], 2, as.numeric )

  des_eusilc <- svydesign(ids = ~rb030, strata =~db040,  weights = ~rb050, data = eusilc)
  des_eusilc <- convey_prep(des_eusilc)
  des_eusilc_rep <-as.svrepdesign(des_eusilc, type= "bootstrap")
  des_eusilc_rep <- convey_prep(des_eusilc_rep)


  data(eusilc) ; names( eusilc ) <- tolower( names( eusilc ) )
  eusilc[ , sapply( eusilc, is.integer ) ] <- apply( eusilc[ , sapply( eusilc, is.integer ) ], 2, as.numeric )

  des_eusilc <- svydesign(ids = ~rb030, strata =~db040,  weights = ~rb050, data = eusilc)
  des_eusilc <- convey_prep(des_eusilc)
  des_eusilc_rep <-as.svrepdesign(des_eusilc, type= "bootstrap")
  des_eusilc_rep <- convey_prep(des_eusilc_rep)

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


  for (this_dimw in list( NULL, c(.25, .75) )) {
    for ( this_k in seq( 1/3, 1, 1/3 ) ){
      for ( this_g in c( 0 , 1 ) ) {

        a1 <- svyafcdec( ~eqincome+hy050n, ~db040 , design=des_eusilc, cutoffs = list( 7000, 3000 ), g = this_g, k = this_k, dimw = this_dimw, na.rm = FALSE )

        b1 <- svyafcdec( ~eqincome+hy050n, ~db040 , design=des_eusilc_rep, cutoffs = list( 7000, 3000 ), g = this_g, k = this_k, dimw = this_dimw, na.rm = FALSE )


        se_dif1 <- abs(SE(a1$overall)-SE(b1$overall))
        se_dif2 <- max(abs(SE(a1$`raw headcount ratio`)-SE(b1$`raw headcount ratio`)))
        se_dif3 <- max(abs(SE(a1$`censored headcount ratio`)-SE(b1$`censored headcount ratio`)))
        se_dif4 <- max(abs(SE(a1$`percentual contribution per dimension`)-SE(b1$`percentual contribution per dimension`)))
        se_dif5 <- max(abs(SE(a1$`subgroup alkire-foster estimates`)-SE(b1$`subgroup alkire-foster estimates`)))
        se_dif6 <- max(abs(SE(a1$`percentual contribution per subgroup`)-SE(b1$`percentual contribution per subgroup`)))

        expect_is(coef(a1[[1]]),"numeric")
        expect_is(coef(a1[[2]]),"numeric")
        expect_is(coef(a1[[3]]),"numeric")
        expect_is(coef(a1[[4]]),"numeric")
        expect_is(coef(a1[[5]]),"matrix")
        expect_is(coef(a1[[6]]),"matrix")
        expect_is(coef(b1[[1]]),"numeric")
        expect_is(coef(b1[[2]]),"numeric")
        expect_is(coef(b1[[3]]),"numeric")
        expect_is(coef(b1[[4]]),"numeric")
        expect_is(coef(b1[[5]]),"matrix")
        expect_is(coef(b1[[6]]),"matrix")
        expect_equal(coef(a1[[1]]), coef(b1[[1]]))
        expect_equal(coef(a1[[2]]), coef(b1[[2]]))
        expect_equal(coef(a1[[3]]), coef(b1[[3]]))
        expect_equal(coef(a1[[4]]), coef(b1[[4]]))
        expect_equal(coef(a1[[5]]), coef(b1[[5]]))
        expect_equal(coef(a1[[6]]), coef(b1[[6]]))

        expect_lte(se_dif1, coef(a1[[1]]) * 0.05 ) # the difference between CVs should be less than 5% of the coefficient, otherwise manually set it
        expect_lte(se_dif2, max( coef(a1[[2]]) ) * 0.1 ) # the difference between CVs should be less than 10% of the maximum coefficient, otherwise manually set it
        expect_lte(se_dif3, max( coef(a1[[3]]) ) * 0.1 ) # the difference between CVs should be less than 10% of the maximum coefficient, otherwise manually set it
        expect_lte(se_dif4, max( coef(a1[[4]]) ) * 0.1 ) # the difference between CVs should be less than 10% of the maximum coefficient, otherwise manually set it
        expect_lte( se_dif5, max( coef(a1[[5]])[,1] ) * 0.1 ) # the difference between CVs should be less than 10% of the maximum coefficient, otherwise manually set it
        expect_lte( se_dif6, max( coef(a1[[6]])[,1] ) * 0.1 ) # the difference between CVs should be less than 10% of the maximum coefficient, otherwise manually set it

        expect_is(SE(a1[[1]]),"matrix")
        expect_is(SE(a1[[2]]),"numeric")
        expect_is(SE(a1[[3]]),"numeric")
        expect_is(SE(a1[[4]]),"numeric")
        expect_is(SE(a1[[5]]),"matrix")
        expect_is(SE(b1[[1]]),"numeric")
        expect_is(SE(b1[[2]]),"numeric")
        expect_is(SE(b1[[3]]),"numeric")
        expect_is(SE(b1[[4]]),"numeric")
        expect_is(SE(b1[[5]]),"numeric")

        expect_lte(confint(a1[[1]])[1], coef(a1[[1]]))
        expect_gte(confint(a1[[1]])[2], coef(a1[[1]]))
        expect_lte( max( confint(a1[[2]])[,1] - coef(a1[[2]]) ) , 0 )
        expect_gte( min( confint(a1[[2]])[,2] - coef(a1[[2]]) ) , 0 )
        expect_lte( max( confint(a1[[3]])[,1] - coef(a1[[3]]) ) , 0 )
        expect_gte( min( confint(a1[[3]])[,2] - coef(a1[[3]]) ) , 0 )
        expect_lte( max( confint(a1[[4]])[,1] - coef(a1[[4]]) ) , 0 )
        expect_gte( min( confint(a1[[4]])[,2] - coef(a1[[4]]) ) , 0 )
        expect_lte( max( confint(a1[[5]])[,1] - coef(a1[[5]]) ) , 0 )
        expect_gte( min( confint(a1[[5]])[,2] - coef(a1[[5]]) ) , 0 )
        expect_lte( max( confint(a1[[6]])[,1] - coef(a1[[6]]) ) , 0 )
        expect_gte( min( confint(a1[[6]])[,2] - coef(a1[[6]]) ) , 0 )
        expect_lte(confint(b1[[1]])[1], coef(b1[[1]]))
        expect_gte(confint(b1[[1]])[2], coef(b1[[1]]))
        expect_lte( max( confint(b1[[2]])[,1] - coef(b1[[2]]) ) , 0 )
        expect_gte( min( confint(b1[[2]])[,2] - coef(b1[[2]]) ) , 0 )
        expect_lte( max( confint(b1[[3]])[,1] - coef(b1[[3]]) ) , 0 )
        expect_gte( min( confint(b1[[3]])[,2] - coef(b1[[3]]) ) , 0 )
        expect_lte( max( confint(b1[[4]])[,1] - coef(b1[[4]]) ) , 0 )
        expect_gte( min( confint(b1[[4]])[,2] - coef(b1[[4]]) ) , 0 )
        expect_lte( max( confint(b1[[5]])[,1] - coef(b1[[5]]) ) , 0 )
        expect_gte( min( confint(b1[[5]])[,2] - coef(b1[[5]]) ) , 0 )
        expect_lte( max( confint(b1[[6]])[,1] - coef(b1[[6]]) ) , 0 )
        expect_gte( min( confint(b1[[6]])[,2] - coef(b1[[6]]) ) , 0 )

        expect_equal(sum(confint(a1[[1]])[,1]<= coef(a1[[1]])),length(coef(a1[[1]])))
        expect_equal(sum(confint(a1[[1]])[,2]>= coef(a1[[1]])),length(coef(a1[[1]])))
        expect_equal(sum(confint(a1[[2]])[,1]<= coef(a1[[2]])),length(coef(a1[[2]])))
        expect_equal(sum(confint(a1[[2]])[,2]>= coef(a1[[2]])),length(coef(a1[[2]])))
        expect_equal(sum(confint(a1[[3]])[,1]<= coef(a1[[3]])),length(coef(a1[[3]])))
        expect_equal(sum(confint(a1[[3]])[,2]>= coef(a1[[3]])),length(coef(a1[[3]])))
        expect_equal(sum(confint(a1[[4]])[,1]<= coef(a1[[4]])),length(coef(a1[[4]])))
        expect_equal(sum(confint(a1[[4]])[,2]>= coef(a1[[4]])),length(coef(a1[[4]])))
        expect_equal(sum(confint(a1[[5]])[,1]<= coef(a1[[5]])),length(coef(a1[[5]])))
        expect_equal(sum(confint(a1[[5]])[,2]>= coef(a1[[5]])),length(coef(a1[[5]])))
        expect_equal(sum(confint(a1[[6]])[,1]<= coef(a1[[6]])),length(coef(a1[[6]])))
        expect_equal(sum(confint(a1[[6]])[,2]>= coef(a1[[6]])),length(coef(a1[[6]])))

        expect_equal(sum(confint(b1[[1]])[,1]<= coef(b1[[1]])),length(coef(b1[[1]])))
        expect_equal(sum(confint(b1[[1]])[,2]>= coef(b1[[1]])),length(coef(b1[[1]])))
        expect_equal(sum(confint(b1[[2]])[,1]<= coef(b1[[2]])),length(coef(b1[[2]])))
        expect_equal(sum(confint(b1[[2]])[,2]>= coef(b1[[2]])),length(coef(b1[[2]])))
        expect_equal(sum(confint(b1[[3]])[,1]<= coef(b1[[3]])),length(coef(b1[[3]])))
        expect_equal(sum(confint(b1[[3]])[,2]>= coef(b1[[3]])),length(coef(b1[[3]])))
        expect_equal(sum(confint(b1[[4]])[,1]<= coef(b1[[4]])),length(coef(b1[[4]])))
        expect_equal(sum(confint(b1[[4]])[,2]>= coef(b1[[4]])),length(coef(b1[[4]])))
        expect_equal(sum(confint(b1[[5]])[,1]<= coef(b1[[5]])),length(coef(b1[[5]])))
        expect_equal(sum(confint(b1[[5]])[,2]>= coef(b1[[5]])),length(coef(b1[[5]])))
        expect_equal(sum(confint(b1[[6]])[,1]<= coef(b1[[6]])),length(coef(b1[[6]])))
        expect_equal(sum(confint(b1[[6]])[,2]>= coef(b1[[6]])),length(coef(b1[[6]])))



        # database-backed design
        c1 <- svyafcdec( ~eqincome+hy050n, ~db040 , design=dbd_eusilc, cutoffs = list( 7000, 3000 ), g = this_g, k = this_k, dimw = this_dimw, na.rm = FALSE )

        # database svyafcdec"
        expect_equal(coef(a1[[1]]), coef(c1[[1]]))
        expect_equal(coef(a1[[2]]), coef(c1[[2]]))
        expect_equal(coef(a1[[3]]), coef(c1[[3]]))
        expect_equal(coef(a1[[4]]), coef(c1[[4]]))
        expect_equal(coef(a1[[5]]), coef(c1[[5]]))
        expect_equal(coef(a1[[6]]), coef(c1[[6]]))

        expect_equal(SE(a1[[1]]), SE(c1[[1]]))
        expect_equal(SE(a1[[2]]), SE(c1[[2]]))
        expect_equal(SE(a1[[3]]), SE(c1[[3]]))
        expect_equal(SE(a1[[4]]), SE(c1[[4]]))
        expect_equal(SE(a1[[5]]), SE(c1[[5]]))
        expect_equal(SE(a1[[6]]), SE(c1[[6]]))



        # database-backed replcate design
        c2 <- svyafcdec( ~eqincome+hy050n, ~db040 , design=dbd_eusilc_rep, cutoffs = list( 7000, 3000 ), g = this_g, k = this_k, dimw = this_dimw, na.rm = FALSE )

        # compare database-backed designs to non-database-backed designs
        # dbi subsets equal non-dbi subsets"
        expect_equal(coef(c2[[1]]), coef(b1[[1]]))
        expect_equal(coef(c2[[2]]), coef(b1[[2]]))
        expect_equal(coef(c2[[3]]), coef(b1[[3]]))
        expect_equal(coef(c2[[4]]), coef(b1[[4]]))
        expect_equal(coef(c2[[5]]), coef(b1[[5]]))
        expect_equal(coef(c2[[6]]), coef(b1[[6]]))

        expect_equal(SE(c2[[1]]), SE(b1[[1]]))
        expect_equal(SE(c2[[2]]), SE(b1[[2]]))
        expect_equal(SE(c2[[3]]), SE(b1[[3]]))
        expect_equal(SE(c2[[4]]), SE(b1[[4]]))
        expect_equal(SE(c2[[5]]), SE(b1[[5]]))
        expect_equal(SE(c2[[6]]), SE(b1[[6]]))

      }
    }
  }

  dbRemoveTable( conn , 'eusilc' )
  dbDisconnect( conn )

} )
