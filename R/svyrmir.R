#' Relative median income ratio
#'
#' Estimate the ratio between the median income of people with age above 65 and the median income of people with age below 65.
#'
#'
#' @param formula a formula specifying the income variable
#' @param design a design object of class \code{survey.design} or class \code{svyrep.design} from the \code{survey} library.
#' @param age formula defining the variable age
#' @param agelim the age cutpoint, the default is 65
#' @param quantiles income quantile, usually .5 (median)
#' @param na.rm Should cases with missing values be dropped?
#' @param med_old return the median income of people older than agelim
#' @param med_young return the median income of people younger than agelim
#' @param ... arguments passed on to `survey::svyquantile`
#'
#' @details you must run the \code{convey_prep} function on your survey design object immediately after creating it with the \code{svydesign} or \code{svrepdesign} function.
#'
#' @return Object of class "\code{cvystat}", which are vectors with a "\code{var}" attribute giving the variance and a "\code{statistic}" attribute giving the name of the statistic.
#'
#' @author Djalma Pessoa and Anthony Damico
#' @seealso \code{\link{svyarpt}}
#'
#' @references Guillaume Osier (2009). Variance estimation for complex indicators
#' of poverty and inequality. \emph{Journal of the European Survey Research
#' Association}, Vol.3, No.3, pp. 167-195,
#' ISSN 1864-3361, URL \url{http://ojs.ub.uni-konstanz.de/srm/article/view/369}.
#'
#' Jean-Claude Deville (1999). Variance estimation for complex statistics and estimators:
#' linearization and residual techniques. Survey Methodology, 25, 193-203,
#' URL \url{http://www5.statcan.gc.ca/bsolc/olc-cel/olc-cel?lang=eng&catno=12-001-X19990024882}.
#'
#' @keywords survey
#'
#' @examples
#' library(survey)
#' library(laeken)
#' data(eusilc) ; names( eusilc ) <- tolower( names( eusilc ) )
#'
#' # missing completely at random, missingness rate = .20
#' ind_miss <- rbinom(nrow(eusilc), 1, .20 )
#' eusilc$eqincome_miss <- eusilc$eqincome
#' is.na(eusilc$eqincome_miss)<- ind_miss==1
#'
#' # linearized design
#' des_eusilc <- svydesign( ids = ~rb030 , strata = ~db040 ,  weights = ~rb050 , data = eusilc )
#' des_eusilc <- convey_prep(des_eusilc)
#'
#' svyrmir( ~eqincome , design = des_eusilc , age = ~age, med_old = TRUE )
#'
#' # replicate-weighted design
#' des_eusilc_rep <- as.svrepdesign( des_eusilc , type = "bootstrap" )
#' des_eusilc_rep <- convey_prep(des_eusilc_rep)
#'
#' svyrmir( ~eqincome , design = des_eusilc_rep, age= ~age, med_old = TRUE )
#'
#' \dontrun{
#'
#' # linearized design using a variable with missings
#' svyrmir( ~ eqincome_miss , design = des_eusilc,age= ~age)
#' svyrmir( ~ eqincome_miss , design = des_eusilc , age= ~age, na.rm = TRUE )
#' # replicate-weighted design using a variable with missings
#' svyrmir( ~ eqincome_miss , design = des_eusilc_rep,age= ~age )
#' svyrmir( ~ eqincome_miss , design = des_eusilc_rep ,age= ~age, na.rm = TRUE )
#'
#' # database-backed design
#' library(RSQLite)
#' library(DBI)
#' dbfile <- tempfile()
#' conn <- dbConnect( RSQLite::SQLite() , dbfile )
#' dbWriteTable( conn , 'eusilc' , eusilc )
#'
#' dbd_eusilc <-
#' 	svydesign(
#' 		ids = ~rb030 ,
#' 		strata = ~db040 ,
#' 		weights = ~rb050 ,
#' 		data="eusilc",
#' 		dbname=dbfile,
#' 		dbtype="SQLite"
#' 	)
#'
#' dbd_eusilc <- convey_prep( dbd_eusilc )
#'
#' svyrmir( ~eqincome , design = dbd_eusilc , age = ~age )
#'
#' dbRemoveTable( conn , 'eusilc' )
#'
#' dbDisconnect( conn , shutdown = TRUE )
#'
#' }
#'
#' @export
svyrmir <-
	function(formula, design, ...) {

		if( length( attr( terms.formula( formula ) , "term.labels" ) ) > 1 ) stop( "convey package functions currently only support one variable in the `formula=` argument" )

		UseMethod("svyrmir", design)

	}

#' @rdname svyrmir
#' @export
svyrmir.survey.design  <-
  function(formula, design, age, agelim = 65, quantiles=0.5, na.rm=FALSE, med_old = FALSE, med_young = FALSE,...){

    if (is.null(attr(design, "full_design"))) stop("you must run the ?convey_prep function on your linearized survey design object immediately after creating it with the svydesign() function.")

    incvar <- model.frame(formula, design$variables, na.action = na.pass)[[1]]
    agevar <- model.frame(age, design$variables, na.action = na.pass)[[1]]
    x <- cbind(incvar,agevar)

    if(na.rm){

      nas<-rowSums(is.na(x))
      design<-design[nas==0,]

      if (length(nas) > length(design$prob)){
        incvar <- incvar[nas == 0]
        agevar <- agevar[nas==0]
      } else{
        incvar[nas > 0] <- 0
        agevar[nas > 0] <- 0
      }
    }
    if( is.null( names( design$prob ) ) ) names(design$prob) <- as.character( seq( length( design$prob ) ) )
    w <- 1/design$prob
    N <- sum(w)
    h <- h_fun(incvar,w)
    age.name <- terms.formula(age)[[2]]

    dsub1 <- eval( substitute( within_function_subset( design , subset = age < agelim ) , list( age = age.name, agelim = agelim ) ) )
    if( nrow( dsub1 ) == 0 ) stop( "zero records in the set of non-elderly people" )

	if( "DBIsvydesign" %in% class( dsub1 ) ) {
		ind1<- names(design$prob) %in% which(dsub1$prob!=Inf)
	} else{
		ind1<- names(design$prob) %in% names(dsub1$prob)
	}


    q_alpha1 <- survey::svyquantile(x = formula, design = dsub1, quantiles = quantiles,method = "constant", na.rm = na.rm,...)
    q_alpha1 <- as.vector(q_alpha1)

    Fprime1 <- densfun(formula = formula, design = dsub1, q_alpha1, h=h, FUN = "F", na.rm=na.rm)
    N1 <- sum(w*ind1)
    linquant1 <- -( 1 / ( N1 * Fprime1 ) ) *ind1* ( ( incvar <= q_alpha1 ) - quantiles )


    dsub2 <- eval( substitute( within_function_subset( design , subset = age >= agelim ) , list( age = age.name, agelim = agelim ) ) )

    if( nrow( dsub2 ) == 0 ) stop( "zero records in the set of elderly people" )

	if( "DBIsvydesign" %in% class( dsub2 ) ) {
		ind2<- names(design$prob) %in% which(dsub2$prob!=Inf)
	} else{
		ind2<- names(design$prob) %in% names(dsub2$prob)
	}



    q_alpha2 <- survey::svyquantile(x = formula, design = dsub2, quantiles = quantiles, method = "constant", na.rm = na.rm,...)
    q_alpha2 <- as.vector(q_alpha2)

    Fprime2 <- densfun(formula = formula, design = dsub2, q_alpha2, h=h, FUN = "F", na.rm=na.rm)
    N2 <- sum(w*ind2)

    linquant2 <- -( 1 / ( N2 * Fprime2 ) ) *ind2* ( ( incvar <= q_alpha2 ) - quantiles )
    # linearize ratio of medians

    MED1 <- list(value = q_alpha1 , lin = linquant1 )
    MED2 <- list(value = q_alpha2 , lin = linquant2 )
    list_all<- list(MED1=MED1, MED2=MED2)

    RMED <- contrastinf(quote(MED2/MED1),list_all)
    rval <- as.vector(RMED$value)
    lin <- RMED$lin

    variance <- survey::svyrecvar(lin/design$prob, design$cluster, design$strata, design$fpc, postStrata = design$postStrata)

    colnames( variance ) <- rownames( variance ) <-  names( rval ) <- strsplit( as.character( formula )[[2]] , ' \\+ ' )[[1]]
    class(rval) <- c( "cvystat" , "svystat" )
    attr( rval , "var" ) <- variance
    attr(rval, "lin") <- lin
    attr( rval , "statistic" ) <- "rmir"
    if (med_old) attr( rval, "med_old") <- q_alpha2
    if (med_young) attr( rval, "med_young") <- q_alpha1

    rval
  }


#' @rdname svyrmir
#' @export
svyrmir.svyrep.design <-
	function(formula, design, age, agelim = 65, quantiles = 0.5, na.rm=FALSE, med_old = FALSE, med_young = FALSE,...) {

		if (is.null(attr(design, "full_design"))) stop("you must run the ?convey_prep function on your replicate-weighted survey design object immediately after creating it with the svrepdesign() function.")

		df <- model.frame(design)
		incvar <- model.frame(formula, design$variables, na.action = na.pass)[[1]]
		agevar <- model.frame(age, design$variables, na.action = na.pass)[[1]]
		x <- cbind(incvar,agevar)

		if(na.rm){
			nas<-rowSums(is.na(x))
			design<-design[nas==0,]
			df <- model.frame(design)
			incvar <- incvar[nas==0]
			agevar<- agevar[nas==0]
		}

		ComputeRmir <-
			function(x, w, quantiles, age, agelim) {
				indb <- age < agelim
				quant_below <- computeQuantiles(x[indb], w[indb], p = quantiles)
				inda <-  age >= agelim
				quant_above <- computeQuantiles(x[inda], w[inda], p = quantiles)
				c(quant_above, quant_below, quant_above/quant_below)
			}

		ws <- weights(design, "sampling")

		Rmir_val <- ComputeRmir(x = incvar, w = ws, quantiles = quantiles, age= agevar, agelim = agelim)

		rval <- Rmir_val[3]

		ww <- weights(design, "analysis")
		qq <- apply(ww, 2, function(wi) ComputeRmir(incvar, wi, quantiles = quantiles, age= agevar, agelim = agelim)[3])
		if(anyNA(qq))variance <- NA
		else 	variance <- survey::svrVar(qq, design$scale, design$rscales, mse = design$mse, coef = rval)

		variance <- as.matrix( variance )

		colnames( variance ) <- rownames( variance ) <-  names( rval ) <- strsplit( as.character( formula )[[2]] , ' \\+ ' )[[1]]

		class(rval) <- c( "cvystat" , "svrepstat" )
		attr( rval , "var" ) <- variance
		attr(rval, "lin") <- NA
		attr( rval , "statistic" ) <- "rmir"
		if (med_old) attr( rval, "med_old") <- Rmir_val[1]
		if (med_young) attr( rval, "med_young") <- Rmir_val[2]

		rval
}

#' @rdname svyrmir
#' @export
svyrmir.DBIsvydesign <-
	function (formula, design, age, ...){

		if (!( "logical" %in% class(attr(design, "full_design"))) ){

			full_design <- attr( design , "full_design" )

			full_design$variables <-
				cbind(
					getvars(formula, attr( design , "full_design" )$db$connection, attr( design , "full_design" )$db$tablename, updates = attr( design , "full_design" )$updates, subset = attr( design , "full_design" )$subset),

					getvars(age, attr( design , "full_design" )$db$connection, attr( design , "full_design" )$db$tablename,
					updates = attr( design , "full_design" )$updates, subset = attr( design , "full_design" )$subset)
				)

			attr( design , "full_design" ) <- full_design

			rm( full_design )

		}

		design$variables <-
			cbind(
				getvars(formula, design$db$connection, design$db$tablename, updates = design$updates, subset = design$subset),

				getvars(age, design$db$connection, design$db$tablename, updates = design$updates, subset = design$subset)
			)

		NextMethod("svyrmir", design)
	}
