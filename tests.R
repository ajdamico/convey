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
htot <- h_fun(eusilc$eqIncome, eusilc$rb050)
fun_giniw <- svygini(~eqIncome, des_eusilc, ncom = rownames(eusilc), comp = TRUE)
# show results from convey 1.2.1.1 point estimate
fun_giniw$gini_coef
# 1.2.1.2 se estimate
SE_lin(fun_giniw, des_eusilc)


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
fun_ginid <- svyby(~eqIncome, by = ~db040, design = des_eusilc, FUN = svygini, ncom = rownames(eusilc),
    comp = TRUE, deff = FALSE, keep.var = FALSE)
# show results from library convey 1.2.2.1 point estimates
unlist(fun_ginid$statistic.gini_coef)
# 1.2.2.2 se estimates
unlist(SE_lin(fun_ginid, des_eusilc))


########################################################### se estimation

# indicator rmpg, whole sample library vardpoor
data(eusilc)
dataset <- data.frame(1:nrow(eusilc), eusilc)
colnames(dataset)[1] <- "IDd"
dataset1 <- dataset[1:1000, ]

aa <- varpoord(Y = "eqIncome", w_final = "rb050", Y_thres = NULL, wght_thres = NULL,
    ID_household = "db030", id = "IDd", H = "db040", PSU = "rb030", N_h = NULL, sort = NULL,
    Dom = NULL, gender = NULL, X = NULL, X_ID_household = NULL, g = NULL, datasetX = NULL,
    q = rep(1, if (is.null(datasetX)) nrow(as.data.frame(H)) else nrow(datasetX)),
    dataset = dataset1, percentage = 60, order_quant = 50, alpha = 20, confidence = 0.95,
    outp_lin = TRUE, outp_res = FALSE, several.ok = FALSE, type = "linarpt")

# Show results from library vardpoor arpt estimate
aa$all_result$value
# se of the arpt estimate
aa$all_result$se

# library convey
dataset1 <- eusilc[1:1000, ]

# introduce fpc in the design object:

N_h <- aggregate(dataset1[, "rb050"], list(db040 = dataset1$db040), FUN = sum)
names(N_h)[2] <- "Nh"
n_h <- aggregate(dataset1[, "rb050"], list(db040 = dataset1$db040), FUN = length)
names(n_h)[2] <- "nh"
f_h <- merge(N_h, n_h)
dataset1 <- merge(dataset1, f_h)


des_eusilc0 <- svydesign(id = ~rb030, strata = ~db040, weights = ~rb050, data = dataset1,
    fpc = ~I(nh/Nh), nest = TRUE)
des_eusilc0 <- convey_prep(des_eusilc0)

# estimate bandwdth using the whole sample
htot <- h_fun(dataset1$eqIncome, dataset1$rb050)

test_arpt <- svyarpt(~eqIncome, des_eusilc0, 0.5, 0.6, h = htot, ncom = rownames(dataset1),
    comp = TRUE)
# show results from convey arpt estimate
test_arpt$value
# se of the arpt estimate
SE_lin(test_arpt, des_eusilc0)


## indicator arpt, domains

## vardpoor
data(eusilc)
dataset <- data.frame(1:nrow(eusilc), eusilc)
colnames(dataset)[1] <- "IDd"
dataset1 <- dataset[1:1000, ]
aa1 <- varpoord(Y = "eqIncome", w_final = "rb050", Y_thres = NULL, wght_thres = NULL,
    ID_household = "db030", id = "IDd", H = "db040", PSU = "rb030", N_h = NULL, sort = NULL,
    Dom = "db040", gender = NULL, X = NULL, X_ID_household = NULL, g = NULL, datasetX = NULL,
    q = rep(1, if (is.null(datasetX)) nrow(as.data.frame(H)) else nrow(datasetX)),
    dataset = dataset1, percentage = 60, order_quant = 50, alpha = 20, confidence = 0.95,
    outp_lin = TRUE, outp_res = FALSE, several.ok = FALSE, type = "linarpt")

# Show results from vardpoor
res_vardpoor_arpt <- as.data.frame(aa1$all_result)
res_vardpoor_arpt <- res_vardpoor_arpt[, c("type", "db040", "value", "se")]
as.data.frame(as.data.frame(aa1$all_result))[, c("type", "db040", "value", "se")]

## convey

test_arpt_dom1 <- svyby(~eqIncome, by = ~db040, FUN = svyarpt, design = des_eusilc0,
    0.5, 0.6, h = htot, ncom = rownames(dataset1), comp = TRUE, deff = FALSE, keep.var = FALSE)
se_arpt_dom1 <- SE_lin(test_arpt_dom1, des_eusilc0)

# show resuts from convey
res_convey_arpt <- data.frame(domain = test_arpt_dom1$db040, type = rep("arpt", length(se_arpt_dom1)),
    value = unlist(test_arpt_dom1$statistic.value), se = unlist(se_arpt_dom1))


# Note: the linearized variable by convey and vardpoor coincide the difference in
# the se value is due to the difference in the estimation of the of variance of
# the total


# all indicators, domains

# vardpoor

aa2 <- varpoord(Y = "eqIncome", w_final = "rb050", Y_thres = NULL, wght_thres = NULL,
    ID_household = "db030", id = "IDd", H = "db040", PSU = "rb030", N_h = NULL, sort = NULL,
    Dom = "db040", gender = NULL, X = NULL, X_ID_household = NULL, g = NULL, datasetX = NULL,
    q = rep(1, if (is.null(datasetX)) nrow(as.data.frame(H)) else nrow(datasetX)),
    dataset = dataset, percentage = 60, order_quant = 50, alpha = 20, confidence = 0.95,
    outp_lin = TRUE, outp_res = TRUE, several.ok = FALSE, type = "all_choices")

# Show results from vardpoorresult for ARPT from example 3:

# convey
N_h <- aggregate(eusilc[, "rb050"], list(db040 = eusilc$db040), FUN = sum)
names(N_h)[2] <- "Nh"
n_h <- aggregate(eusilc[, "rb050"], list(db040 = eusilc$db040), FUN = length)
names(n_h)[2] <- "nh"
f_h <- merge(N_h, n_h)
eusilc <- merge(eusilc, f_h)


des_eusilc <- svydesign(id = ~rb030, strata = ~db040, weights = ~rb050, data = eusilc,
    fpc = ~I(nh/Nh), nest = TRUE)
des_eusilc <- convey_prep(des_eusilc)


htot <- h_fun(eusilc$eqIncome, eusilc$rb050)
arptw <- svyarpt(~eqIncome, design = des_eusilc, 0.5, 0.6, h = htot, ncom = rownames(eusilc),
    comp = TRUE)


# arpt domains
test_arpt_dom <- svyby(~eqIncome, by = ~db040, FUN = svyarpt, design = des_eusilc,
    order = 0.5, percent = 0.6, h = htot, ncom = rownames(eusilc), comp = TRUE, deff = FALSE,
    keep.var = FALSE)
se_arpt_dom <- SE_lin(test_arpt_dom, des_eusilc)
# organize results for arpt
frame_arpt <- data.frame(domain = test_arpt_dom$db040, type = rep("arpt", length(se_arpt_dom)),
    value = unlist(test_arpt_dom$statistic.value), se = unlist(se_arpt_dom))

# arpr domains
test_arpr_dom <- svyby(~eqIncome, by = ~db040, FUN = svyarpr, design = des_eusilc,
    order = 0.5, percent = 0.6, h = htot, ARPT = arptw, ncom = rownames(eusilc),
    comp = TRUE, deff = FALSE, keep.var = FALSE)
se_arpr_dom <- SE_lin(test_arpr_dom, des_eusilc)
# organize results for arpr
frame_arpr <- data.frame(domain = test_arpr_dom$db040, type = rep("arpr", length(se_arpr_dom)),
    value = unlist(test_arpr_dom$statistic.value), se = unlist(se_arpr_dom))

# rmpg domains
test_rmpg_dom <- svyby(~eqIncome, by = ~db040, FUN = svyrmpg, design = des_eusilc,
    order = 0.5, percent = 0.6, h = htot, ncom = rownames(eusilc), comp = TRUE, ARPT = test_arpt,
    deff = FALSE, keep.var = FALSE)
se_rmpg_dom <- SE_lin(test_rmpg_dom, des_eusilc)
# organize results for rmpg
frame_rmpg <- data.frame(domain = test_rmpg_dom$db040, type = rep("rmpg", length(se_rmpg_dom)),
    value = unlist(test_rmpg_dom$statistic.value), se = unlist(se_rmpg_dom))

# qsr domains
test_qsr_dom <- svyby(~eqIncome, by = ~db040, FUN = svyqsr, design = des_eusilc,
    alpha = 0.2, ncom = rownames(eusilc), comp = TRUE, incvec = eusilc$eqIncome,
    deff = FALSE, keep.var = FALSE)
se_qsr_dom <- SE_lin(test_qsr_dom, des_eusilc)
# organize results for qsr
frame_qsr <- data.frame(domain = test_qsr_dom$db040, type = rep("qsr", length(se_arpr_dom)),
    value = unlist(test_qsr_dom$statistic.value), se = unlist(se_qsr_dom))

# gini domains
test_gini_dom <- svyby(~eqIncome, by = ~db040, FUN = svygini, design = des_eusilc,
    ncom = rownames(eusilc), comp = TRUE, deff = FALSE, keep.var = FALSE)
se_gini_dom <- SE_lin(test_gini_dom, des_eusilc)
# organize results for gini
frame_gini <- data.frame(domain = test_gini_dom$db040, type = rep("gini", length(se_gini_dom)),
    value = unlist(test_gini_dom$statistic.gini_coef), se = unlist(se_gini_dom))


# compare results from vardpoor and convey arpt
frame_arpt
subset(as.data.frame(aa2$all_result)[, c("type", "db040", "value", "se")], type ==
    "ARPT")

# arpr
frame_arpr
subset(as.data.frame(aa2$all_result)[, c("type", "db040", "value", "se")], type ==
    "ARPR")


# rmpg
frame_rmpg
subset(as.data.frame(aa2$all_result)[, c("type", "db040", "value", "se")], type ==
    "RMPG")


# qsr
frame_qsr
subset(as.data.frame(aa2$all_result)[, c("type", "db040", "value", "se")], type ==
    "QSR")

# gini
frame_gini
subset(as.data.frame(aa2$all_result)[, c("type", "db040", "value", "se")], type ==
    "GINI")


################################################ tests for functions in the file deriv.rules.R Example 1:

# Create two influence objects for totals
set.seed(1)
income1 <- rchisq(100, 10)
income2 <- rchisq(100, 20)
weights <- rep(c(2, 3), rep(50, 2))

X <- list(value = sum(income1 * weights), lin = income1)
Y <- list(value = sum(income2 * weights), lin = income2)
# cria lista de objects
list.XY <- list(X = X, Y = Y)
datalist <- lapply(list.XY, function(t) t$value)
lixo <- contrastinf(quote(Y/X), list.XY)
# usando ratio_inf
lixo1 <- ratio_inf(Y, X)

## replicate output of function svygpg

set.seed(1)
y <- c(rchisq(10, 10), rchisq(10, 20))
H <- rep(c("str1", "str2"), c(10, 10))
PSU <- rep(rep(c(1, 2), c(5, 5)), 2)
weights <- rep(2, 20)

# create data frame
testset <- data.frame(y = y, H = H, psu = PSU, w = weights)
testset$sex <- rep(c(1, 2), c(10, 10))
testset <- transform(testset, sex = factor(sex, labels = c("female", "male")))
testset$im <- 1 * (testset$sex == "male")
testset$ifm <- 1 * (testset$sex == "female")
testset$ym <- testset$y * (testset$sex == "male")
testset$yfm <- testset$y * (testset$sex == "female")
library(survey)
des <- svydesign(id = ~psu, strata = ~H, weights = ~w, data = testset, nest = TRUE)
des <- convey_prep(des)


a <- svytotal(~ym + yfm + im + ifm, des)

# using the function svycontrast from the library survey

svycontrast(a, quote((ym/im - yfm/im)/(ym/im)))
# using svygpg:
library(convey)
lin_gpg <- svygpg(~y, des, ~sex)
lin_gpg$value
SE_lin(lin_gpg, des)
# use function contrastinf

# create listas of linearization objects
IM <- list(value = sum((testset$sex == "male") * testset$w), lin = 1 * (testset$sex ==
    "male"))
IFM <- list(value = sum((testset$sex == "female") * testset$w), lin = 1 * (testset$sex ==
    "female"))
YM <- list(value = sum(testset$y * (testset$sex == "male") * testset$w), lin = testset$y *
    (testset$sex == "male"))
YFM <- list(value = sum(testset$y * (testset$sex == "female") * testset$w), lin = testset$y *
    (testset$sex == "female"))

list_all_fun <- list(IM = IM, IFM = IFM, YM = YM, YFM = YFM)

# use function contrastinf

gpg_contrastinf <- contrastinf(quote((YM/IM - YFM/IFM)/(YM/IM)), list_all_fun)

gpg_contrastinf$value
SE_lin(gpg_contrastinf, des)

# example 3.  get output of svyarpr using fun_par_inf

library(vardpoor)
data(eusilc)
dati = data.frame(1:nrow(eusilc), eusilc)
colnames(dati)[1] <- "IDd"
library(survey)
# create a design object
des_eusilc <- svydesign(ids = ~db040, weights = ~rb050, data = eusilc)
des_eusilc <- convey_prep(des_eusilc)


htot <- h_fun(eusilc$eqIncome, eusilc$rb050)

# use get the poverty treshold
arpt_eqIncome <- svyarpt(~eqIncome, design = des_eusilc, 0.5, 0.6, h = htot, ncom = rownames(eusilc),
    comp = TRUE)

# poor people ratio
arpr_eqIncome <- svyarpr(~eqIncome, design = des_eusilc, 0.5, 0.6, h = htot, ARPT = arpt_eqIncome,
    ncom = rownames(eusilc), comp = TRUE)


# get the object of linearization of the poor people ratio using fun_par_inf
arpr_eqIncome1 <- fun_par_inf(arpt_eqIncome, "icdf", "densfun", formula = ~eqIncome,
    design = des_eusilc, ncom = rownames(eusilc), comp = TRUE, htot = NULL, fun = "F")


## using
arpr_eqIncome2 <- svyarpr(~eqIncome, design = des_eusilc, 0.5, 0.6, h = htot, ARPT = arpt_eqIncome,
    ncom = rownames(eusilc), comp = TRUE)


## get the output for domains.

# library convey convey
fun_arprd <- svyby(~eqIncome, by = ~db040, design = des_eusilc, FUN = svyarpr, order = 0.5,
    percent = 0.6, h = htot, ARPT = arpt_eqIncome, ncom = rownames(des_eusilc$variables),
    comp = TRUE, deff = FALSE, keep.var = FALSE)
# show results from convey arpr estimates
unlist(fun_arprd$statistic.value) * 100
# linearized arpr for Burgenland
table(fun_arprd$statistic.lin[[1]] * 100)
unlist(SE_lin(fun_arprd, des_eusilc))

#
fun_arprd1 <- svyby(~eqIncome, by = ~db040, design = des_eusilc, FUN = svyarpr, h = htot,
    ARPT = arpt_eqIncome, ncom = rownames(eusilc), comp = TRUE, deff = FALSE, keep.var = FALSE)
unlist(fun_arprd1$statistic.value) * 100
table(fun_arprd1$statistic.lin[[1]] * 100)

unlist(SE_lin(fun_arprd1, des_eusilc))


############################################################## example 4.  compare ratio_inf and icdf
library(vardpoor)
library(survey)
data(eusilc)
des_eusilc <- svydesign(id = ~rb030, strata = ~db040, weights = ~rb050, data = eusilc,
    nest = TRUE)
des_eusilc <- convey_prep(des_eusilc)

# create variables
des_eusilc <- update(des_eusilc, num = (eqIncome <= 20000) * 1)
des_eusilc <- update(des_eusilc, den = rep(1, nrow(eusilc)))

# get infuence function of numerator and denominator
NUM <- itot(~num, des_eusilc)
DEN <- itot(~den, des_eusilc)

# influence function of ratio
result <- ratio_inf(NUM, DEN)


# directly using function icdf

resul_icdf <- icdf(~eqIncome, des_eusilc, 20000, ncom = rownames(eusilc), comp = TRUE)






###################################################################################### Example 1.  dataset test 3
set.seed(1)
y <- c(rchisq(10, 10), rchisq(10, 20))
H <- rep(c("str1", "str2"), c(10, 10))
PSU <- rep(rep(c(1, 2), c(5, 5)), 2)
weights <- rep(2, 20)

# create data frame
testset <- data.frame(y = y, H = H, psu = PSU, w = weights)
testset$sex <- rep(c(1, 2), c(10, 10))
testset <- transform(testset, sex = factor(sex, labels = c("female", "male")))

# using svycontrast
testset$im <- 1 * (testset$sex == "male")
testset$ifm <- 1 * (testset$sex == "female")
testset$ym <- testset$y * (testset$sex == "male")
testset$yfm <- testset$y * (testset$sex == "female")


des <- svydesign(id = ~psu, strata = ~H, weights = ~w, data = testset, nest = TRUE)
des <- convey_prep(des)

a <- svytotal(~ym + yfm + im + ifm, des)

svycontrast(a, quote((ym/im - yfm/im)/(ym/im)))

# using svygpg.survey.design:

des <- svydesign(id = ~psu, strata = ~H, weights = ~w, data = testset, nest = TRUE)
des <- convey_prep(des)

lin_test_gpg <- svygpg(~y, des, ~sex)
lin_test_gpg$value
testset$lingpg <- lin_test_gpg$lin
des <- svydesign(id = ~psu, strata = ~H, weights = ~w, data = testset, nest = TRUE)
SE(svytotal(~lingpg, des))
# Note: results by svycontrast and svygpg.survey.design match


## using bootstrap

des_rep <- as.svrepdesign(des, type = "bootstrap", replicates = 500)
svygpg(~y, des_rep, ~sex)


################################################# Example 2.  dataset from library vardpoor 1 . using library vardpoor
library(vardpoor)
data(ses)

dati <- data.table(ID = 1:nrow(ses), ses)
setnames(dati, "sex", "sexf")
dati[sexf == "male", `:=`(sex, 1)]
dati[sexf == "female", `:=`(sex, 2)]
gpgs1 <- lingpg(Y = "earningsHour", gender = "sex", id = "ID", weight = "weights",
    dataset = dati)
des_ses <- svydesign(id = ~1, weights = ~weights, data = ses, variables = ~weights +
    sex + earningsHour + location)
des_ses <- convey_prep(des_ses)

# result in %

gpgs1$value
summary(gpgs1$lin$lin_gpg)
# se using vardpoor
des_ses <- update(des_ses, lingpgv = gpgs1$lin$lin_gpg)
svytotal(~lingpgv, des_ses)

# 2.  using convey with object of class survey.design

des_ses <- svydesign(id = ~1, weights = ~weights, data = ses, variables = ~weights +
    sex + earningsHour)
names(des_ses$variable)


ses_gpg <- svygpg(~earningsHour, des_ses, ~sex)
# show results
ses_gpg$value
summary(100 * ses_gpg$lin)
# estimate se
des_ses <- update(des_ses, lingpgc = ses_gpg$lin)
svytotal(~lingpgc, des_ses)



# 3. using the function svycontrast of the library survey:

ses$one <- rep(1, nrow(ses))
ses$one1 <- ses$one * (ses$sex == "male")
ses$one2 <- ses$one * (ses$sex == "female")
ses$earningsHour1 <- ses$earningsHour * (ses$sex == "male")
ses$earningsHour2 <- ses$earningsHour * (ses$sex == "female")


des_ses <- svydesign(id = ~1, weights = ~weights, data = ses)
des_ses <- convey_prep(des_ses)

a <- svytotal(~earningsHour1 + earningsHour2 + one1 + one2, des_ses)
# results for svycontrast
svycontrast(a, quote((earningsHour1/one1 - earningsHour2/one2)/(earningsHour1/one1)))



# 4. using convey with object of class svyrep.design type bootstrap
library(survey)
des_ses <- svydesign(id = ~1, weights = ~weights, data = ses, variables = ~weights +
    sex + earningsHour)
syrepdes_ses <- as.svrepdesign(des_ses, type = "bootstrap", replicates = 500)

# result in %
gpg_rep <- svygpg(~earningsHour, syrepdes_ses, ~sex)
gpg_rep

# Remarks: values of the indicator are the same from 1,2,3,4 values of se are the
# same from 2 and 3 value of se from 4 is appr. equal to values from 2 and 3
# value of se from 1 differs from all the others. The linearized variable is
# different from vardpoor and convey. Is it wrong???  verificação da vardpoor

library(vardpoor)

data(ses)

# lin vardpoor
Im <- 1 * (ses$sex == "male")
Xm <- sum(ses$earningsHour * Im * ses$weights)
Nm <- sum(Im * ses$weights)
If <- 1 * (ses$sex == "female")
Xf <- sum(ses$earningsHour * If * ses$weights)
Nf <- sum(If * ses$weights)
Xbarm <- Xm/Nm
Xbarf <- Xf/Nf
gpg <- 1 - Xbarf/Xbarm

x <- ses$earningsHour
# linearization by vardpoor

lingpg <- (1 - gpg) * (Im/Nf - Im/Nm + x * Im/Xm - x * If/Xf)

# correction

lingpg1 <- (1 - gpg) * (If/Nf - Im/Nm + x * Im/Xm - x * If/Xf)

summary(lingpg1)
