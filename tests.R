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

################ 1. ARPT

# linearize the indicator arpt: whole sample library vardpoor
dati <- data.frame(IDd = 1:nrow(eusilc), eusilc)
vardpoor_arptw <- linarpt(Y = "eqIncome", id = "IDd", weight = "rb050", Dom = NULL,
    dataset = dati, percentage = 60, order_quant = 50)
# show results from vardpoor summary list
vardpoor_arptwl <- list(value = vardpoor_arptw$value, lin = vardpoor_arptw$lin$lin_arpt)
# 1.1.1.1 point estimate
vardpoor_arptw$value
# linearized arpt for whole sample 1.1.1.2 se estimate
SE_lin(vardpoor_arptwl, des_eusilc)

# library convey whole sample

fun_arptw <- svyarpt(~eqIncome, design = des_eusilc, 0.5, 0.6)
fun_arptw
# show results from convey 1.2.1.1 point estimate
fun_arptw$value

# domains library vardpoor
vardpoor_arptd <- linarpt(Y = "eqIncome", id = "IDd", weight = "rb050", Dom = "db040",
    dataset = dati, percentage = 60, order_quant = 50)
# summary list
vardpoor_arptdl <- list(domain = data.frame(vardpoor_arptd$value)[, "db040"], value = as.list(as.data.frame(vardpoor_arptd$value)[,
    2]), statistic.lin = as.list(as.data.frame(vardpoor_arptd$lin)[, 2:10]))
# 1.1.2.1 point estimates
unlist(vardpoor_arptdl$value)
# 1.1.2.2 se estimates
unlist(SE_lin(vardpoor_arptdl, des_eusilc))

# library convey
fun_arptd <- svyby(~eqIncome, by = ~db040, design = des_eusilc, FUN = svyarpt, order = 0.5, percent = 0.6, deff = FALSE, keep.var = FALSE, keep.names = TRUE)
# show results from library convey 1.2.2.1 point estimates
fun_arptd # check point estimates

####################### 2. ARPR

# linearize the indicator arpr: whole sample vardpoor
vardpoor_arprw <- linarpr(Y = "eqIncome", id = "IDd", weight = "rb050", Dom = NULL,
    dataset = dati, percentage = 60, order_quant = 50)

# summary list
vardpoor_arprwl <- list(value = vardpoor_arprw$value, lin = vardpoor_arprw$lin$lin_arpr)
# 2.1.1.1 point estimate
vardpoor_arprw$value
# 2.1.1.2 se estimate
SE_lin(vardpoor_arprwl, des_eusilc)

# library convey whole sample
fun_arprw <- svyarpr(~eqIncome, des_eusilc, 0.5, 0.6)
# show results from convey 2.2.1.1 point estimate
fun_arprw


# domains library vardpoor
vardpoor_arprd <- linarpr(Y = "eqIncome", id = "IDd", weight = "rb050", Dom = "db040",
    dataset = dati, percentage = 60, order_quant = 50)
# summary list
vardpoor_arprdl <- list(domain = data.frame(vardpoor_arprd$value)[, "db040"], value = as.list(as.data.frame(vardpoor_arprd$value)[,
    2]), statistic.lin = as.list(as.data.frame(vardpoor_arprd$lin)[, 2:10]))
# 2.1.2.1 point estimates
unlist(vardpoor_arprdl$value)
# 2.1.2.2 se estimates
unlist(SE_lin(vardpoor_arprdl, des_eusilc))

# library convey
fun_arprd <- svyby(~eqIncome, by = ~db040, design = des_eusilc, FUN = svyarpr, order = 0.5, percent = 0.6,deff = FALSE, keep.var = FALSE)
fun_arprd


################## RMPG

# linearize the indicator rmpg: whole sample library vardpoor
dati <- data.frame(IDd = 1:nrow(eusilc), eusilc)
vardpoor_rmpgw <- linrmpg(Y = "eqIncome", id = "IDd", weight = "rb050", Dom = NULL,
    dataset = dati, percentage = 60, order_quant = 50)
# show results from vardpoor summary list
vardpoor_rmpgwl <- list(value = vardpoor_rmpgw$value, lin = vardpoor_rmpgw$lin$lin_rmpg)
# 3.1.1.1 point estimate
vardpoor_rmpgw$value
# linearized rmpg for whole sample 3.1.1.2 se estimate
SE_lin(vardpoor_rmpgwl, des_eusilc)

# library convey whole sample
fun_rmpgw <- svyrmpg(~eqIncome, design = des_eusilc, 0.5, 0.6)
# show results from convey 3.2.1.1 point estimate
fun_rmpgw


# domains library vardpoor
vardpoor_rmpgd <- linrmpg(Y = "eqIncome", id = "IDd", weight = "rb050", Dom = "db040",
    dataset = dati, percentage = 60, order_quant = 50)
# summary list
vardpoor_rmpgdl <- list(domain = data.frame(vardpoor_rmpgd$value)[, "db040"], value = as.list(as.data.frame(vardpoor_rmpgd$value)[,
    2]), statistic.lin = as.list(as.data.frame(vardpoor_rmpgd$lin)[, 2:10]))
# 3.1.2.1 point estimates
unlist(vardpoor_rmpgdl$value)
# 3.1.2.2 se estimates
unlist(SE_lin(vardpoor_rmpgdl, des_eusilc))

# library convey
fun_rmpgd <- svyby(~eqIncome, by = ~db040, design = des_eusilc, FUN = svyrmpg, order = 0.5, percent = 0.6, deff = FALSE, keep.var = FALSE, keep.names = TRUE)
# show results from library convey 3.2.2.1 point estimates
fun_rmpgd


################### S80/S20 linearize the indicator qsr: whole sample library vardpoor
dati <- data.frame(IDd = 1:nrow(eusilc), eusilc)
vardpoor_qsrw <- linqsr(Y = "eqIncome", id = "IDd", weight = "rb050", Dom = NULL,
    dataset = dati, alpha = 20)
# show results from vardpoor summary list
vardpoor_qsrwl <- list(value = vardpoor_qsrw$value, lin = vardpoor_qsrw$lin$lin_qsr)
# 4.1.1.1 point estimate
vardpoor_qsrw$value
# linearized qsr for whole sample 4.1.1.2 se estimate
SE_lin(vardpoor_qsrwl, des_eusilc)

# library convey whole sample
fun_qsrw <- svyqsr(~eqIncome, des_eusilc, 0.2)
# show results from convey 4.2.1.1 point estimate
fun_qsrw


# domains library vardpoor
vardpoor_qsrd <- linqsr(Y = "eqIncome", id = "IDd", weight = "rb050", Dom = "db040",
    dataset = dati, alpha = 20)
# summary list
vardpoor_qsrdl <- list(domain = data.frame(vardpoor_qsrd$value)[, "db040"], value = as.list(as.data.frame(vardpoor_qsrd$value)[,
    2]), statistic.lin = as.list(as.data.frame(vardpoor_qsrd$lin)[, 2:10]))
# 4.1.2.1 point estimates
unlist(vardpoor_qsrdl$value)
# 4.1.2.2 se estimates
unlist(SE_lin(vardpoor_qsrdl, des_eusilc))

# library convey
fun_qsrd <- svyby(~eqIncome, by = ~db040, design = des_eusilc, FUN = svyqsr,
  alpha = 0.2, deff = FALSE,keep.var = FALSE)
# show results from library convey 4.2.2.1 qsr estimates
fun_qsrd



################## GINI linearize the indicator gini: whole sample library vardpoor
dati <- data.frame(IDd = 1:nrow(eusilc), eusilc)
vardpoor_giniw <- lingini(Y = "eqIncome", id = "IDd", weight = "rb050", dataset = dati)
# show results from vardpoor summary list
vardpoor_giniwl <- list(value = vardpoor_giniw$value, lin = vardpoor_giniw$lin$lin_gini)
# 1.1.1.1 point estimate
vardpoor_giniw$value
# linearized gini for whole sample 1.1.1.2 se estimate
SE_lin(vardpoor_giniwl, des_eusilc)


# library convey whole sample

fun_giniw <- svygini(~eqIncome, des_eusilc)
# show results from convey 1.2.1.1 point estimate
fun_giniw


# domains library vardpoor
vardpoor_ginid <- lingini(Y = "eqIncome", id = "IDd", weight = "rb050", Dom = c("db040"),
    dataset = dati)
# summary list
vardpoor_ginidl <- list(domain = data.frame(vardpoor_ginid$value)[, "db040"], value = as.list(as.data.frame(vardpoor_ginid$value)[,
    2]), statistic.lin = as.list(as.data.frame(vardpoor_ginid$lin)[, 2:10]))
# 1.1.2.1 point estimates
unlist(vardpoor_ginidl$value)
# 1.1.2.2 se estimates
unlist(SE_lin(vardpoor_ginidl, des_eusilc))

# library convey
fun_ginid <- svyby(~eqIncome, by = ~db040, design = des_eusilc, FUN = svygini, deff = FALSE, keep.var = FALSE)

fun_ginid


