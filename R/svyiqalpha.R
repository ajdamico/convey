#' Linearization of a variable quantile
#'
#' Computes the linearized variable of a quantile of variable.
#'
#' @param formula a formula specifying the income variable
#' @param design a design object of class \code{survey.design} or class \code{svyrep.design} from the \code{survey} library.
#' @param alpha the order of the quantile
#' @return Object of class "\code{cvystat}", which are vectors with a "\code{var}" attribute giving the variance and a "\code{statistic}" attribute giving the name of the statistic.
#' @param na.rm Should cases with missing values be dropped?
#' @param ... arguments passed on to `survey::svyquantile`
#'
#' @details you must run the \code{convey_prep} function on your survey design object immediately after creating it with the \code{svydesign} or \code{svrepdesign} function.
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
#'
#' library(laeken)
#' data(eusilc) ; names( eusilc ) <- tolower( names( eusilc ) )
#' library(survey)
#' # linearized design
#' des_eusilc <- svydesign( ids = ~rb030 , strata = ~db040 ,  weights = ~rb050 , data = eusilc )
#' des_eusilc <- convey_prep(des_eusilc)
#'
#' svyiqalpha( ~eqincome , design = des_eusilc, .50 )
#'
#' # replicate-weighted design
#' des_eusilc_rep <- as.svrepdesign( des_eusilc , type = "bootstrap" )
#' des_eusilc_rep <- convey_prep(des_eusilc_rep)
#'
#' svyiqalpha( ~eqincome , design = des_eusilc_rep, .50 )
#'
#' \dontrun{
#'
#' # linearized design using a variable with missings
#' svyiqalpha( ~ py010n , design = des_eusilc, .50 )
#' svyiqalpha( ~ py010n , design = des_eusilc , .50, na.rm = TRUE )
#' # replicate-weighted design using a variable with missings
#' svyiqalpha( ~ py010n , design = des_eusilc_rep, .50 )
#' svyiqalpha( ~ py010n , design = des_eusilc_rep ,.50, na.rm = TRUE )
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
#' svyiqalpha( ~ eqincome , design = dbd_eusilc, .50 )
#'
#' dbRemoveTable( conn , 'eusilc' )
#'
#' dbDisconnect( conn , shutdown = TRUE )
#'
#' }
#'
#' @export
svyiqalpha <-
	function(formula, design, ...) {

		if( length( attr( terms.formula( formula ) , "term.labels" ) ) > 1 ) stop( "convey package functions currently only support one variable in the `formula=` argument" )

		UseMethod("svyiqalpha", design)

	}

#' @rdname svyiqalpha
#' @export
svyiqalpha.survey.design <-
	function(formula, design, alpha, na.rm=FALSE, ...) {

		if (is.null(attr(design, "full_design"))) stop("you must run the ?convey_prep function on your linearized survey design object immediately after creating it with the svydesign() function.")

		incvar <- model.frame(formula, design$variables, na.action = na.pass)[[1]]

		if(na.rm){
			nas<-is.na(incvar)
			design<-design[!nas,]

			if (length(nas) > length(design$prob)) incvar <- incvar[!nas] else incvar[nas] <- 0

		}

		ind <- names(design$prob)
		w <- 1/design$prob
		N <- sum(w)

		q_alpha <- survey::svyquantile(x = formula, design = design, quantiles = alpha, method = "constant", na.rm = na.rm,...)

		q_alpha <- as.vector(q_alpha)

		rval <- q_alpha

		h <- h_fun(incvar, w)

		Fprime <- densfun(formula = formula, design = design, q_alpha, h=h, FUN = "F", na.rm=na.rm)

		iq <- -(1/(N * Fprime)) * ((incvar <= q_alpha) - alpha)

		variance <- survey::svyrecvar(iq/design$prob, design$cluster, design$strata, design$fpc, postStrata = design$postStrata)

		colnames( variance ) <- rownames( variance ) <-  names( rval ) <- strsplit( as.character( formula )[[2]] , ' \\+ ' )[[1]]

		class(rval) <- c( "cvystat" , "svystat" )
		attr(rval, "lin") <- iq
		attr(rval, "var") <- variance
		attr(rval, "statistic") <- "quantile"

		rval
	}

#' @rdname svyiqalpha
#' @export
#'
svyiqalpha.svyrep.design <-
	function(formula, design, alpha, na.rm=FALSE, ...) {

		if (is.null(attr(design, "full_design"))) stop("you must run the ?convey_prep function on your replicate-weighted survey design object immediately after creating it with the svrepdesign() function.")

		incvar <- model.frame(formula, design$variables, na.action = na.pass)[[1]]

		if(na.rm){
			nas<-is.na(incvar)
			design<-design[!nas,]
			if (length(nas) > length(design$prob)) incvar <- incvar[!nas] else incvar[nas] <- 0
		}

		w <- weights(design, "sampling")
		quant_val <- computeQuantiles(incvar, w, p = alpha)
		quant_val <- as.vector(quant_val)
		rval <- quant_val
		ww <- weights(design, "analysis")
		qq <- apply(ww, 2, function(wi)  computeQuantiles(incvar, wi, p = alpha))

		if(anyNA(qq))variance <- NA
		else variance <- survey::svrVar(qq, design$scale, design$rscales, mse = design$mse, coef = rval)

		variance <- as.matrix( variance )

		colnames( variance ) <- rownames( variance ) <-  names( rval ) <- strsplit( as.character( formula )[[2]] , ' \\+ ' )[[1]]
		class(rval) <- c( "cvystat" , "svrepstat" )
		attr(rval, "var") <- variance
		attr(rval, "statistic") <- "quantile"

		rval
	}


#' @rdname svyiqalpha
#' @export
svyiqalpha.DBIsvydesign <-
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

		NextMethod("svyiqalpha", design)
	}
