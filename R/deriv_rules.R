
##' @name funs
##' @title Auxiliary functions Implement the rules for Influence functions in Osier's paper


#' @rdname funs
#' @export
# 1. influence function of a constant: formula (28)
iconst <- function(formula, design) {
  incvar <- model.frame(formula, design$variables, na.action = na.pass)[[1]]
    list(value = 0, lin = rep(0, length(incvar)))
}

#' @rdname funs
#' @export
# 1.  infuence function of a total: formula (34)
itot <- function(formula, design) {
  incvar <- model.frame(formula, design$variables, na.action = na.pass)[[1]]
    value <- coef(survey::svytotal(x = formula, design = design))
    lin <- incvar
    list(value = value, lin = lin)
}


#' @rdname funs
#' @export

cl_inf <- function(a, b, big_t, big_s) {
    lin <- a * big_t$lin + b * big_s$lin
    value <- a * big_t$value + b * big_s$value
    list(value = value, lin = lin)
}

#' @rdname funs
#' @export
# product of of two functionals: formula (30)
prod_inf <- function(big_t, big_s) {

    value <- big_t$value * big_s$value
    lin <- big_t$value * big_s$lin + big_s$value * big_t$lin
    list(value = value, lin = lin)
}

#' @rdname funs
#' @export
# ratio of functionals: formula (31)
ratio_inf <- function(big_t, big_s) {
    value <- big_t$value/big_s$value
    lin <- (big_s$value * big_t$lin - big_t$value * big_s$lin)/((big_s$value)^2)
    list(value = value, lin = lin)
}

#' @rdname funs
#' @export
# composition of two functionals: formula (32)
comp_inf <- function(big_t, big_s) {
    itsm <- rep(big_s$value, length(big_t$lin))
    itsm * big_s$lin
}

#' @rdname funs
#' @export
# function of a functional: (33) F and Fprime are function names
fun_par_inf <- function(big_s, F, Fprime, ...) {
    dots <- list(...)
    if (is.null(big_s$lin)) {
        value <- big_s$value
        lin <- -(do.call(F, c(x = big_s$value, dots))$lin)/(do.call(Fprime, c(x = big_s$value,
            dots)))
    } else {
        value <- do.call(F, c(x = big_s$value, dots))$value
        lin <- do.call(F, c(x = big_s$value, dots))$lin + do.call(Fprime, c(x = big_s$value,
            dots)) * big_s$lin
    }
    list(value = value, lin = lin)
}


#' @rdname funs
#' @export
fun_par_inf <- function(big_s, F, Fprime, ...) {
    dots <- list(...)
    if (is.null(big_s$lin)) {
        value <- big_s$value
        lin <- -do.call(F, c(x = big_s$value, dots))$lin/do.call(Fprime, c(x = big_s$value,
            dots))
    } else {
        value <- do.call(F, c(x = big_s$value, dots))$value
        lin <- do.call(F, c(x = big_s$value, dots))$lin + do.call(Fprime, c(x = big_s$value,
            dots)) * big_s$lin
    }
    list(value = value, lin = lin)
}


