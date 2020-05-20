#' Gini coefficient
#'
#' Estimate the Gini coefficient, a measure of inequalty
#'
#' @param formula a formula specifying the income variable
#' @param design a design object of class \code{survey.design} or class \code{svyrep.design} from the \code{survey} library.
#' @param na.rm Should cases with missing values be dropped?
#' @param ... future expansion
#'
#' @details you must run the \code{convey_prep} function on your survey design object immediately after creating it with the \code{svydesign} or \code{svrepdesign} function.
#'
#' @return Object of class "\code{cvystat}", which are vectors with a "\code{var}" attribute giving the variance and a "\code{statistic}" attribute giving the name of the statistic.
#'
#' @author Djalma Pessoa and Anthony Damico
#'
#' @seealso \code{\link{svyarpr}}
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
#' # linearized design
#' des_eusilc <- svydesign( ids = ~rb030 , strata = ~db040 ,  weights = ~rb050 , data = eusilc )
#' des_eusilc <- convey_prep(des_eusilc)
#'
#' svygini( ~eqincome , design = des_eusilc )
#'
#' # replicate-weighted design
#' des_eusilc_rep <- as.svrepdesign( des_eusilc , type = "bootstrap" )
#' des_eusilc_rep <- convey_prep(des_eusilc_rep)
#'
#' svygini( ~eqincome , design = des_eusilc_rep )
#'
#' \dontrun{
#'
#' # linearized design using a variable with missings
#' svygini( ~ py010n , design = des_eusilc )
#' svygini( ~ py010n , design = des_eusilc , na.rm = TRUE )
#' # replicate-weighted design using a variable with missings
#' svygini( ~ py010n , design = des_eusilc_rep )
#' svygini( ~ py010n , design = des_eusilc_rep , na.rm = TRUE )
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
#' svygini( ~ eqincome , design = dbd_eusilc )
#'
#' dbRemoveTable( conn , 'eusilc' )
#'
#' dbDisconnect( conn , shutdown = TRUE )
#'
#' }
#'
#' @export
svygini <-
	function(formula, design, ...) {

		if( length( attr( terms.formula( formula ) , "term.labels" ) ) > 1 ) stop( "convey package functions currently only support one variable in the `formula=` argument" )

		UseMethod("svygini", design)

}


#' @rdname svygini
#' @export
svygini.survey.design <-
	function(formula, design, na.rm=FALSE, ...) {

		if (is.null(attr(design, "full_design"))) stop("you must run the ?convey_prep function on your linearized survey design object immediately after creating it with the svydesign() function.")

		incvar <- model.frame(formula, design$variables, na.action = na.pass)[[1]]

		if (na.rm) {
			nas <- is.na(incvar)
			design <- design[nas == 0, ]
			if (length(nas) > length(design$prob)) incvar <- incvar[nas == 0] else incvar[nas > 0] <- 0
		}

		w <- 1/design$prob

		ordincvar <- order(incvar)
		w <- w[ordincvar]
		incvar <- incvar[ordincvar]

		# population size
		N <- sum(w)

		# total income
		Y <- sum(incvar * w)

		# cumulative weight
		r <- cumsum(w)

		# partial weighted function
		G <- cumsum(incvar * w)
		T2<- list(value=sum(incvar*w), lin=incvar)
		T3<- list(value= sum(w), lin=rep(1, length(incvar)))

		# get T1
		T1val <- sum( r * incvar * w )
		T1lin <-  Y - G + incvar * w + r * incvar
		T1 <- list( value = T1val , lin = T1lin )
		list_all <- list(T1 = T1, T2 = T2, T3 = T3)
		GINI <- contrastinf( quote( ( 2 * T1 - T2 ) / ( T2 * T3 ) - 1 ) , list_all )
		lingini <- as.vector( GINI$lin )
		if(sum(w==0) > 0)  lingini <- lingini*(w!=0)
		lingini <- lingini[order(ordincvar)]
		rval <- GINI$value

		variance <- survey::svyrecvar(lingini/design$prob, design$cluster,design$strata, design$fpc, postStrata = design$postStrata)

		colnames( variance ) <- rownames( variance ) <-  names( rval ) <- strsplit( as.character( formula )[[2]] , ' \\+ ' )[[1]]
		class(rval) <- c( "cvystat" , "svystat" )
		attr(rval, "var") <- variance
		attr(rval, "statistic") <- "gini"
		attr(rval,"lin")<- lingini

		rval
	}

#' @rdname svygini
#' @export
svygini.svyrep.design <-
	function(formula, design,na.rm=FALSE, ...) {

		if (is.null(attr(design, "full_design"))) stop("you must run the ?convey_prep function on your replicate-weighted survey design object immediately after creating it with the svrepdesign() function.")

		df <- model.frame(design)
		incvar <- model.frame(formula, design$variables, na.action = na.pass)[[1]]

		if(na.rm){
			nas<-is.na(incvar)
			design<-design[!nas,]
			df <- model.frame(design)
			incvar <- incvar[!nas]
		}


		ComputeGini <-
			function(x, w) {
				w <- w[order(x)]
				x <- x[order(x)]
				N <- sum(w)
				n <- length(x)
				big_t <- sum(x * w)
				r <- cumsum(w)
				Num <- sum((2 * r - 1) * x * w)
				Den <- N * big_t
				(Num/Den) - 1
			}

		ws <- weights(design, "sampling")

		rval <- ComputeGini(incvar, ws)

		ww <- weights(design, "analysis")

		qq <- apply(ww, 2, function(wi) ComputeGini(incvar, wi))
		if(anyNA(qq))variance <- NA
		else variance <- survey::svrVar(qq, design$scale, design$rscales, mse = design$mse, coef = rval)

		variance <- as.matrix( variance )

		colnames( variance ) <- rownames( variance ) <-  names( rval ) <- strsplit( as.character( formula )[[2]] , ' \\+ ' )[[1]]
		class(rval) <- c( "cvystat" , "svrepstat" ) 
		attr(rval, "var") <- variance
		attr(rval, "statistic") <- "gini"

		rval
	}


#' @rdname svygini
#' @export
svygini.DBIsvydesign <-
	function (formula, design, ...){

		if (!( "logical" %in% class(attr(design, "full_design"))) ){

			full_design <- attr( design , "full_design" )

			full_design$variables <-
				getvars(
					formula,
					attr( design , "full_design" )$db$connection,
					attr( design , "full_design" )$db$tablename,
					updates = attr( design , "full_design" )$updates,
					subset = attr( design , "full_design" )$subset
				)

			attr( design , "full_design" ) <- full_design

			rm( full_design )

		}

		design$variables <-
			getvars(
				formula,
				design$db$connection,
				design$db$tablename,
				updates = design$updates,
				subset = design$subset
			)

		NextMethod("svygini", design)
	}
