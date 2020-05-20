#' Quintile Share Ratio
#'
#' Estimate ratio of the total income received by the highest earners to the total income received by lowest earners, defaulting to 20%.
#'
#'
#' @param formula a formula specifying the income variable
#' @param design a design object of class \code{survey.design} or class \code{svyrep.design} from the \code{survey} library.
#' @param alpha1 order of the lower quintile
#' @param alpha2 order of the upper quintile
#' @param na.rm Should cases with missing values be dropped?
#' @param upper_quant return the lower bound of highest earners
#' @param lower_quant return the upper bound of lowest earners
#' @param upper_tot return the highest earners total
#' @param lower_tot return the lowest earners total
#' @param ... future expansion
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
#'
#' library(survey)
#' library(laeken)
#' data(eusilc) ; names( eusilc ) <- tolower( names( eusilc ) )
#'
#' # linearized design
#' des_eusilc <- svydesign( ids = ~rb030 , strata = ~db040 ,  weights = ~rb050 , data = eusilc )
#' des_eusilc <- convey_prep( des_eusilc )
#'
#' svyqsr( ~eqincome , design = des_eusilc, upper_tot = TRUE, lower_tot = TRUE )
#'
#' # replicate-weighted design
#' des_eusilc_rep <- as.svrepdesign( des_eusilc , type = "bootstrap" )
#' des_eusilc_rep <- convey_prep( des_eusilc_rep )
#'
#' svyqsr( ~eqincome , design = des_eusilc_rep, upper_tot = TRUE, lower_tot = TRUE )
#'
#' \dontrun{
#'
#' # linearized design using a variable with missings
#' svyqsr( ~ db090 , design = des_eusilc )
#' svyqsr( ~ db090 , design = des_eusilc , na.rm = TRUE )
#' # replicate-weighted design using a variable with missings
#' svyqsr( ~ db090 , design = des_eusilc_rep )
#' svyqsr( ~ db090 , design = des_eusilc_rep , na.rm = TRUE )
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
#' svyqsr( ~ eqincome , design = dbd_eusilc )
#'
#' dbRemoveTable( conn , 'eusilc' )
#'
#' dbDisconnect( conn , shutdown = TRUE )
#'
#' }
#'
#' @export
svyqsr <-
	function(formula, design, ...) {

		if( length( attr( terms.formula( formula ) , "term.labels" ) ) > 1 ) stop( "convey package functions currently only support one variable in the `formula=` argument" )

		if( 'alpha' %in% names( list(...) ) && list(...)[["alpha"]] > 0.5 ) stop( "alpha= cannot be larger than 0.5 (50%)" )

		UseMethod("svyqsr", design)

	}

#' @rdname svyqsr
#' @export
svyqsr.survey.design <-
	function(formula, design, alpha1 = 0.2 , alpha2 = ( 1 - alpha1 ) , na.rm=FALSE, upper_quant = FALSE, lower_quant = FALSE, upper_tot = FALSE, lower_tot = FALSE, ...) {

		if (is.null(attr(design, "full_design"))) stop("you must run the ?convey_prep function on your linearized survey design object immediately after creating it with the svydesign() function.")

		incvar <- model.frame(formula, design$variables, na.action = na.pass)[[1]]

		if(na.rm){
			nas<-is.na(incvar)
			design<-design[!nas,]
			if (length(nas) > length(design$prob)) incvar <- incvar[!nas] else incvar[nas] <- 0
		}

		w <- 1/design$prob
		ind <- names(design$prob)

		# Linearization of S20
		S20 <- svyisq(formula = formula, design = design, alpha1, na.rm=na.rm)
		qS20 <- attr(S20, "quantile")
		totS20 <- coef(S20)
		attributes(totS20) <- NULL
		S20 <- list(value= coef(S20), lin=attr(S20,"lin"))

		if( S20$value == 0 ) stop( paste0( "division by zero. the alpha1=" , alpha1 , " percentile cannot be zero or svyqsr would return Inf" ) )

		# Linearization of S80
		S80 <- svyisq(formula = formula, design = design, alpha2 , na.rm=na.rm)
		qS80 <- attr(S80, "quantile")
		totS80 <- coef(S80)
		attributes(totS80) <- NULL
		S80 <- list(value= coef(S80), lin=attr(S80,"lin"))

		names(incvar)<-ind
		TOT <- list(value=sum(incvar*w), lin=incvar)
		# LINEARIZED VARIABLE OF THE SHARE RATIO

		list_all <- list(TOT=TOT, S20 = S20, S80 = S80)
		QSR <- contrastinf( quote((TOT-S80)/S20), list_all)
		rval <- QSR$value

		attributes (rval) <- NULL
		lin <- as.vector(QSR$lin)
		variance <- survey::svyrecvar(lin/design$prob, design$cluster,design$strata, design$fpc, postStrata = design$postStrata)

		colnames( variance ) <- rownames( variance ) <-  names( rval ) <- strsplit( as.character( formula )[[2]] , ' \\+ ' )[[1]]
		class(rval) <- c( "cvystat" , "svystat" )
		attr(rval, "var") <- variance
		attr(rval, "statistic") <- "qsr"
		attr(rval, "lin") <- lin
		if(upper_quant)  attr(rval, "upper_quant") <- qS80
		if(lower_quant)  attr(rval, "lower_quant") <- qS20
		if(upper_tot)  attr(rval, "upper_tot") <- TOT$value-totS80
		if(lower_tot)  attr(rval, "lower_tot") <- totS20

		rval
	}

#' @rdname svyqsr
#' @export
svyqsr.svyrep.design <-
	function(formula, design, alpha1 = 0.2 , alpha2 = ( 1 - alpha1 ) , na.rm=FALSE, upper_quant = FALSE, lower_quant = FALSE, upper_tot = FALSE, lower_tot = FALSE, ...) {

		if (is.null(attr(design, "full_design"))) stop("you must run the ?convey_prep function on your replicate-weighted survey design object immediately after creating it with the svrepdesign() function.")

		# if the class of the full_design attribute is just a TRUE, then the design is
		# already the full design.  otherwise, pull the full_design from that attribute.
		if ("logical" %in% class(attr(design, "full_design"))) full_design <- design else full_design <- attr(design, "full_design")

		df <- model.frame(design)
		incvar <- model.frame(formula, design$variables, na.action = na.pass)[[1]]

		if(na.rm){
			nas<-is.na(incvar)
			design<-design[!nas,]
			df <- model.frame(design)
			incvar <- incvar[!nas]
		}


		ComputeQsr <-
			function(x, w, alpha1, alpha2) {
				quant_inf <- computeQuantiles(x, w, p = alpha1)
				quant_sup <- computeQuantiles(x, w, p = alpha2)
				rich <- (x > quant_sup) * x
				S80 <- sum(rich * w)
				poor <- (x <= quant_inf) * x
				S20 <- sum(poor * w)
				c( quant_sup, quant_inf, S80, S20, S80/S20)
			}

		ws <- weights(design, "sampling")
		Qsr_val <- ComputeQsr(incvar, ws, alpha1 = alpha1, alpha2= alpha2)

		if( Qsr_val[4] == 0 ) stop( paste0( "division by zero. the alpha1=" , alpha1 , " percentile cannot be zero or svyqsr would return Inf" ) )

		rval <- Qsr_val[5]

		ww <- weights(design, "analysis")
		qq <- apply(ww, 2, function(wi) ComputeQsr(incvar, w = wi, alpha1 = alpha1, alpha2 = alpha2)[5])

		if(anyNA(qq))variance <- NA
		else variance <- survey::svrVar(qq, design$scale, design$rscales, mse = design$mse, coef = rval)

		variance <- as.matrix( variance )

		colnames( variance ) <- rownames( variance ) <-  names( rval ) <- strsplit( as.character( formula )[[2]] , ' \\+ ' )[[1]]
		class(rval) <- c( "cvystat" , "svrepstat" )
		attr(rval, "var") <- variance
		attr(rval, "statistic") <- "qsr"
		attr(rval, "lin") <- NA
		if(upper_quant)  attr(rval, "upper_quant") <- Qsr_val[1]
		if(lower_quant)  attr(rval, "lower_quant") <- Qsr_val[2]
		if(upper_tot)  attr(rval, "upper_tot") <- Qsr_val[3]
		if(lower_tot)  attr(rval, "lower_tot") <- Qsr_val[4]

		rval
		}

#' @rdname svyqsr
#' @export
svyqsr.DBIsvydesign <-
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

		NextMethod("svyqsr", design)
	}
