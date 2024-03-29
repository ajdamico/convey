% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/svygeidec.R
\name{svygeidec}
\alias{svygeidec}
\alias{svygeidec.survey.design}
\alias{svygeidec.svyrep.design}
\alias{svygeidec.DBIsvydesign}
\title{Generalized Entropy Index Decomposition}
\usage{
svygeidec(formula, subgroup, design, ...)

\method{svygeidec}{survey.design}(
  formula,
  subgroup,
  design,
  epsilon = 1,
  na.rm = FALSE,
  deff = FALSE,
  linearized = FALSE,
  influence = FALSE,
  ...
)

\method{svygeidec}{svyrep.design}(
  formula,
  subgroup,
  design,
  epsilon = 1,
  na.rm = FALSE,
  deff = FALSE,
  linearized = FALSE,
  return.replicates = FALSE,
  ...
)

\method{svygeidec}{DBIsvydesign}(formula, subgroup, design, ...)
}
\arguments{
\item{formula}{a formula specifying the income variable}

\item{subgroup}{a formula specifying the group variable}

\item{design}{a design object of class \code{survey.design} or class \code{svyrep.design} from the \code{survey} library.}

\item{...}{future expansion}

\item{epsilon}{a parameter that determines the sensivity towards inequality in the top of the distribution. Defaults to epsilon = 1.}

\item{na.rm}{Should cases with missing values be dropped? Observations containing missing values in income or group variables will be dropped.}

\item{deff}{Return the design effect (see \code{survey::svymean})}

\item{linearized}{Should a matrix of linearized variables be returned}

\item{influence}{Should a matrix of (weighted) influence functions be returned? (for compatibility with \code{\link[survey]{svyby}})}

\item{return.replicates}{Return the replicate estimates?}
}
\value{
Object of class "\code{cvydstat}", which are vectors with a "\code{var}" attribute giving the variance-covariance matrix and a "\code{statistic}" attribute giving the name of the statistic.
}
\description{
Estimates the group decomposition of the generalized entropy index
}
\details{
you must run the \code{convey_prep} function on your survey design object immediately after creating it with the \code{svydesign} or \code{svrepdesign} function.

This measure only allows for strictly positive variables.
}
\examples{
library(survey)
library(laeken)
data(eusilc) ; names( eusilc ) <- tolower( names( eusilc ) )

# linearized design
des_eusilc <- svydesign( ids = ~rb030 , strata = ~db040 ,  weights = ~rb050 , data = eusilc )
des_eusilc <- convey_prep(des_eusilc)

# replicate-weighted design
des_eusilc_rep <- as.svrepdesign( des_eusilc , type = "bootstrap" )
des_eusilc_rep <- convey_prep(des_eusilc_rep)

# linearized design
svygeidec( ~eqincome , ~rb090 , subset( des_eusilc, eqincome > 0 ) , epsilon = 0 )
svygeidec( ~eqincome , ~rb090 , subset( des_eusilc, eqincome > 0 ) , epsilon = .5 )
svygeidec( ~eqincome , ~rb090 , subset( des_eusilc, eqincome > 0 ) , epsilon = 1 )
svygeidec( ~eqincome , ~rb090 , subset( des_eusilc, eqincome > 0 ) , epsilon = 2 )

# replicate-weighted design
svygeidec( ~eqincome , ~rb090 , subset( des_eusilc_rep, eqincome > 0 ) , epsilon = 0 )
svygeidec( ~eqincome , ~rb090 , subset( des_eusilc_rep, eqincome > 0 ) , epsilon = .5 )
svygeidec( ~eqincome , ~rb090 , subset( des_eusilc_rep, eqincome > 0 ) , epsilon = 1 )
svygeidec( ~eqincome , ~rb090 , subset( des_eusilc_rep, eqincome > 0 ) , epsilon = 2 )

\dontrun{

# linearized design using a variable with missings
sub_des_eusilc <- subset(des_eusilc, py010n > 0 | is.na(py010n) )
svygeidec( ~py010n , ~rb090 , sub_des_eusilc , epsilon = 0 )
svygeidec( ~py010n , ~rb090 , sub_des_eusilc , epsilon = 0, na.rm = TRUE )
svygeidec( ~py010n , ~rb090 , sub_des_eusilc , epsilon = 1 )
svygeidec( ~py010n , ~rb090 , sub_des_eusilc , epsilon = 1, na.rm = TRUE )

# replicate-weighted design using a variable with missings
sub_des_eusilc_rep <- subset(des_eusilc_rep, py010n > 0 | is.na(py010n) )
svygeidec( ~py010n , ~rb090 , sub_des_eusilc_rep , epsilon = 0 )
svygeidec( ~py010n , ~rb090 , sub_des_eusilc_rep , epsilon = 0, na.rm = TRUE )
svygeidec( ~py010n , ~rb090 , sub_des_eusilc_rep , epsilon = 1 )
svygeidec( ~py010n , ~rb090 , sub_des_eusilc_rep , epsilon = 1, na.rm = TRUE )

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

# database-backed linearized design
svygeidec( ~eqincome , ~rb090 , subset(dbd_eusilc, eqincome > 0) , epsilon = 0 )
svygeidec( ~eqincome , ~rb090 , subset(dbd_eusilc, eqincome > 0) , epsilon = .5 )
svygeidec( ~eqincome , ~rb090 , subset(dbd_eusilc, eqincome > 0) , epsilon = 1 )
svygeidec( ~eqincome , ~rb090 , subset(dbd_eusilc, eqincome > 0) , epsilon = 2 )

# database-backed linearized design using a variable with missings
sub_dbd_eusilc <- subset(dbd_eusilc, py010n > 0 | is.na(py010n) )
svygeidec( ~py010n , ~rb090 , sub_dbd_eusilc , epsilon = 0 )
svygeidec( ~py010n , ~rb090 , sub_dbd_eusilc , epsilon = 0, na.rm = TRUE )
svygeidec( ~py010n , ~rb090 , sub_dbd_eusilc , epsilon = .5 )
svygeidec( ~py010n , ~rb090 , sub_dbd_eusilc , epsilon = .5, na.rm = TRUE )
svygeidec( ~py010n , ~rb090 , sub_dbd_eusilc , epsilon = 1 )
svygeidec( ~py010n , ~rb090 , sub_dbd_eusilc , epsilon = 1, na.rm = TRUE )
svygeidec( ~py010n , ~rb090 , sub_dbd_eusilc , epsilon = 2 )
svygeidec( ~py010n , ~rb090 , sub_dbd_eusilc , epsilon = 2, na.rm = TRUE )

dbRemoveTable( conn , 'eusilc' )

dbDisconnect( conn , shutdown = TRUE )

}

}
\references{
Anthony F. Shorrocks (1984). Inequality decomposition groups population subgroups.
\emph{Econometrica}, v. 52, n. 6, 1984, pp. 1369-1385.
DOI \doi{10.2307/1913511}.

Martin Biewen and Stephen Jenkins (2002). Estimation of Generalized Entropy
and Atkinson Inequality Indices from Complex Survey Data. \emph{DIW Discussion Papers},
No.345,
URL \url{https://www.diw.de/documents/publikationen/73/diw_01.c.40394.de/dp345.pdf}.
}
\seealso{
\code{\link{svygei}}
}
\author{
Guilherme Jacob, Djalma Pessoa and Anthony Damico
}
\keyword{survey}
