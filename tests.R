###################################### tests: convey vs vardpoor

library(vardpoor)
data(eusilc)
dati = data.frame(1:nrow(eusilc), eusilc)
colnames(dati)[1] <- "IDd"
library(survey)
# create a design object
des_eusilc <- svydesign(ids = ~db040, weights = ~rb050, data = eusilc)
library(convey)
des_eusilc <- convey_prep(des_eusilc)


#'                     1. ARPT
#' Whole sample
#'
#' library vardpoor
dati <- data.frame(IDd = 1:nrow(eusilc), eusilc)
vardpoor_arptw <- linarpt(Y = "eqIncome", id = "IDd", weight = "rb050", Dom = NULL,
    dataset = dati, percentage = 60, order_quant = 50)
#  point estimate
vardpoor_arptw$value
#  se estimate
SE_lin2(vardpoor_arptw$lin$lin_arpt, des_eusilc)


#' library convey

fun_arptw <- svyarpt(~eqIncome, design = des_eusilc, 0.5, 0.6)
fun_arptw

#'                      Domains
#'
#'  library vardpoor
vardpoor_arptd <- linarpt(Y = "eqIncome", id = "IDd", weight = "rb050", Dom = "db040",
    dataset = dati, percentage = 60, order_quant = 50)

# point estimates
unlist(vardpoor_arptd$value$threshold)
# se estimates
sapply(data.frame(vardpoor_arptd$lin)[,2:10],function(t) SE_lin2(t,des_eusilc))


#' library convey
fun_arptd <- svyby(~eqIncome, by = ~db040, design = des_eusilc, FUN = svyarpt, order = 0.5, percent = 0.6, deff = FALSE, keep.names = TRUE)
# show results from library convey 1.2.2.1 point estimates
fun_arptd

#'                 2. ARPR

#' Whole sample
#'
#' library vardpoor
vardpoor_arprw <- linarpr(Y = "eqIncome", id = "IDd", weight = "rb050", Dom = NULL,
    dataset = dati, percentage = 60, order_quant = 50)


#  point estimate
vardpoor_arprw$value
#  se estimate
SE_lin2(vardpoor_arprw$lin$lin_arpr, des_eusilc)

#' library convey
fun_arprw <- svyarpr(~eqIncome, des_eusilc, 0.5, 0.6)
# show results from convey 2.2.1.1 point estimate
fun_arprw

#' Domains
#'
#'  library vardpoor
vardpoor_arprd <- linarpr(Y = "eqIncome", id = "IDd", weight = "rb050", Dom = "db040",
    dataset = dati, percentage = 60, order_quant = 50)
#  point estimates
unlist(vardpoor_arprd$value$arpr)
#  se estimates
sapply(data.frame(vardpoor_arprd$lin)[,2:10],function(t) SE_lin2(t,des_eusilc))

#'
#' library convey
fun_arprd <- svyby(~eqIncome, by = ~db040, design = des_eusilc, FUN = svyarpr, order = 0.5, percent = 0.6,deff = FALSE)
fun_arprd


#'                3. RMPG
#'
#'                Whole sample
#'
#'  library vardpoor
dati <- data.frame(IDd = 1:nrow(eusilc), eusilc)
vardpoor_rmpgw <- linrmpg(Y = "eqIncome", id = "IDd", weight = "rb050", Dom = NULL,
    dataset = dati, percentage = 60, order_quant = 50)

#  point estimate
vardpoor_rmpgw$value
#  se estimate
SE_lin2(vardpoor_rmpgw$lin$lin_rmpg, des_eusilc)

#' library convey
fun_rmpgw <- svyrmpg(~eqIncome, design = des_eusilc, 0.5, 0.6)
# show results from convey 3.2.1.1 point estimate
fun_rmpgw

#'Domains
#'
#' library vardpoor
vardpoor_rmpgd <- linrmpg(Y = "eqIncome", id = "IDd", weight = "rb050", Dom = "db040",
    dataset = dati, percentage = 60, order_quant = 50)

#  point estimates
unlist(vardpoor_rmpgd$value$rmpg)
#  se estimates
sapply(data.frame(vardpoor_rmpgd$lin)[,2:10],function(t) SE_lin2(t,des_eusilc))


#' library convey
fun_rmpgd <- svyby(~eqIncome, by = ~db040, design = des_eusilc, FUN = svyrmpg, order = 0.5, percent = 0.6, deff = FALSE, keep.names = TRUE)
# show results from library convey 3.2.2.1 point estimates
fun_rmpgd


#'           4. S80/S20 (qsr)
#'
#'  Whole sample
#'
#' library vardpoor
dati <- data.frame(IDd = 1:nrow(eusilc), eusilc)
vardpoor_qsrw <- linqsr(Y = "eqIncome", id = "IDd", weight = "rb050", Dom = NULL,
    dataset = dati, alpha = 20)

#  point estimate
vardpoor_qsrw$value
#  se estimate
SE_lin2(vardpoor_qsrw$lin$lin_qsr, des_eusilc)

#' library convey
fun_qsrw <- svyqsr(~eqIncome, des_eusilc, 0.2)
# show results from convey 4.2.1.1 point estimate
fun_qsrw

#' Domains
#'
#'  library vardpoor
vardpoor_qsrd <- linqsr(Y = "eqIncome", id = "IDd", weight = "rb050", Dom = "db040",
    dataset = dati, alpha = 20)
#  point estimates
unlist(vardpoor_qsrd$value$QSR)
#  se estimates
sapply(data.frame(vardpoor_qsrd$lin)[,2:10],function(t) SE_lin2(t,des_eusilc))

#' library convey
fun_qsrd <- svyby(~eqIncome, by = ~db040, design = des_eusilc, FUN = svyqsr,
  alpha = 0.2, deff = FALSE)
fun_qsrd



#'                 5. GINI
#'
#'  Whole sample
#'
#'  library vardpoor
dati <- data.frame(IDd = 1:nrow(eusilc), eusilc)
vardpoor_giniw <- lingini(Y = "eqIncome", id = "IDd", weight = "rb050", dataset = dati)

#  point estimate
vardpoor_giniw$value
#  se estimate
SE_lin2(vardpoor_giniw$lin$lin_gini, des_eusilc)


#' library convey
fun_giniw <- svygini(~eqIncome, des_eusilc)
# show results from convey 1.2.1.1 point estimate
fun_giniw

#'  Domains
#'
#'  library vardpoor
vardpoor_ginid <- lingini(Y = "eqIncome", id = "IDd", weight = "rb050", Dom = c("db040"),    dataset = dati)


#  point estimates
unlist(vardpoor_ginid$value$Gini)
#  se estimates
sapply(data.frame(vardpoor_ginid$lin)[,2:10],function(t) SE_lin2(t,des_eusilc))

#' library convey
fun_ginid <- svyby(~eqIncome, by = ~db040, design = des_eusilc, FUN = svygini, deff = FALSE)

fun_ginid





