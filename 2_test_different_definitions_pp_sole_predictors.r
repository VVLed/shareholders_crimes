library(data.table)
library(ggplot2)
library(fst)
library(fixest)
library(stringr); library(stringi)
options(scipen = 999)

extract_estimates <- function(x, regular_expression = "roa", model_id = "name") {
  data.frame(
    estimate = x$coefficients[str_detect(names(x$coefficients), regular_expression)][1],
    # estimate_2 = x$coefficients[str_detect(names(x$coefficients), regular_expression)][2],
    # estimate_3 = x$coefficients[str_detect(names(x$coefficients), regular_expression)][3],
    se = x$se[str_detect(names(x$se), regular_expression)][1],
    # se_2 = x$se[str_detect(names(x$se), regular_expression)][2],
    # se_3 = x$se[str_detect(names(x$se), regular_expression)][3],
    model = model_id
  )
}

create_formula <- function(dep_var) {
  as.formula(paste(dep_var, "~", 
                   paste(control_vars, collapse = " + "),
                   "|", 
                   paste(c("year", "inn"), collapse = " + ")))
}

path <- Sys.getenv("SHAREHOLDERS_CRIMES_PATH")
setwd(path)

# Load the data ====

llcs_panel_full <- read_fst("panel_disp_vs_strain.fst", as.data.table = TRUE)

cols_to_transform <- grep("roa", colnames(llcs_panel_full), value = TRUE)
llcs_panel_full[, (cols_to_transform) := lapply(.SD, function (x) asinh(x)), .SDcols = cols_to_transform]

cols_to_log <- grep("market_share|total_assets|revenue", colnames(llcs_panel_full))
llcs_panel_full[, (cols_to_log) := lapply(.SD, function (x) log(x)), .SDcols = cols_to_log]

cols_to_square <- grep("age", colnames(llcs_panel_full), value = TRUE)
llcs_panel_full[, (paste0(cols_to_square, "_squared")) := lapply(.SD, function (x) x^2), .SDcols = cols_to_square]

gc()

# PP t-1 =====

llcs_panel <- llcs_panel_full[roa_b1 != 0 & !is.na(roa_b1) & !is.na(roa_adj_ind_b1) & !is.na(roa_adj_hist_b1) & 
                           !is.na(market_share_b1) & !is.na(total_assets_b1) & !is.na(revenue_b1) & !is.na(lr_ind_year_b1) & !is.na(age_b1) & !is.na(family_element_b1) &
                           revenue_b1 > 0 & sole_shareholder_only == 0 & year >= 2014, ]

cols_to_scale <- grep("(_case_)|(roa)", colnames(llcs_panel), value = TRUE)
llcs_panel[, (cols_to_scale) := lapply(.SD, function (x) scale(x, center = TRUE, scale = TRUE)), .SDcols = cols_to_scale]

# PP models for ROA ====

control_vars <- c("roa_b1", "market_share_b1", 
                  "total_assets_b1", "revenue_b1", "lr_ind_year_b1", "age_b1", "age_b1_squared",
                  "n_participants_b1", "family_element_b1", "zero_interest_loan_b1",
                  "paid_zero_interest_b1")

model_roa_any <- feols(
  create_formula("pp_case_any"),
  cluster = ~ inn,
  panel.id = ~inn + year,
  data = llcs_panel,
  mem.clean = TRUE, verbose = 3, nthreads = 16)

model_roa_satisfied <- feols(
  create_formula("pp_case_satisfied"),
  cluster = ~ inn,
  panel.id = ~inn + year,
  data = llcs_panel,
  mem.clean = TRUE, verbose = 3, nthreads = 16)

model_roa_any_key <- feols(
  create_formula("pp_case_any_key"),
  cluster = ~ inn,
  panel.id = ~inn + year,
  data = llcs_panel,
  mem.clean = TRUE, verbose = 3, nthreads = 16)

model_roa_satisfied_key <- feols(
  create_formula("pp_case_satisfied_key"),
  cluster = ~ inn,
  panel.id = ~inn + year,
  data = llcs_panel,
  mem.clean = TRUE, verbose = 3, nthreads = 16)

models_names <- c("model_roa_any", "model_roa_satisfied", "model_roa_any_key", "model_roa_satisfied_key")

models_list <- mget(models_names)

results_roa_lag1 <- rbindlist(Map(
  function(m, n) {extract_estimates(m, "roa", model_id = n)}, 
  models_list, 
  names(models_list)))

results_roa_lag1$model <- forcats::fct_relevel(results_roa_lag1$model, "model_roa_any", "model_roa_satisfied", "model_roa_any_key", "model_roa_satisfied_key")

results_roa_lag1$order <- "1"
results_roa_lag1$strain <- "absolute"

gc()

# PP models for ROA industry adjusted ====

control_vars <- c("roa_adj_ind_b1", "market_share_b1", 
                  "total_assets_b1", "revenue_b1", "lr_ind_year_b1", "age_b1", "age_b1_squared",
                  "n_participants_b1", "family_element_b1", "zero_interest_loan_b1",
                  "paid_zero_interest_b1")

model_roa_any <- feols(
  create_formula("pp_case_any"),
  cluster = ~ inn,
  panel.id = ~inn + year,
  data = llcs_panel,
  mem.clean = TRUE, verbose = 3, nthreads = 16)

model_roa_satisfied <- feols(
  create_formula("pp_case_satisfied"),
  cluster = ~ inn,
  panel.id = ~inn + year,
  data = llcs_panel,
  mem.clean = TRUE, verbose = 3, nthreads = 16)

model_roa_any_key <- feols(
  create_formula("pp_case_any_key"),
  cluster = ~ inn,
  panel.id = ~inn + year,
  data = llcs_panel,
  mem.clean = TRUE, verbose = 3, nthreads = 16)

model_roa_satisfied_key <- feols(
  create_formula("pp_case_satisfied_key"),
  cluster = ~ inn,
  panel.id = ~inn + year,
  data = llcs_panel,
  mem.clean = TRUE, verbose = 3, nthreads = 16)

models_names <- c("model_roa_any", "model_roa_satisfied", "model_roa_any_key", "model_roa_satisfied_key")

models_list <- mget(models_names)

results_roa_ind_lag1 <- rbindlist(Map(
  function(m, n) {extract_estimates(m, "roa", model_id = n)}, 
  models_list, 
  names(models_list)))

results_roa_ind_lag1$model <- forcats::fct_relevel(results_roa_ind_lag1$model, "model_roa_any", "model_roa_satisfied", "model_roa_any_key", "model_roa_satisfied_key")

results_roa_ind_lag1$order <- "1"
results_roa_ind_lag1$strain <- "ind"

gc()


# PP models for ROA history adjusted ====

control_vars <- c("roa_adj_hist_b1", "market_share_b1", 
                  "total_assets_b1", "revenue_b1", "lr_ind_year_b1", "age_b1", "age_b1_squared",
                  "n_participants_b1", "family_element_b1", "zero_interest_loan_b1",
                  "paid_zero_interest_b1")

model_roa_any <- feols(
  create_formula("pp_case_any"),
  cluster = ~ inn,
  panel.id = ~inn + year,
  data = llcs_panel,
  mem.clean = TRUE, verbose = 3, nthreads = 16)

model_roa_satisfied <- feols(
  create_formula("pp_case_satisfied"),
  cluster = ~ inn,
  panel.id = ~inn + year,
  data = llcs_panel,
  mem.clean = TRUE, verbose = 3, nthreads = 16)

model_roa_any_key <- feols(
  create_formula("pp_case_any_key"),
  cluster = ~ inn,
  panel.id = ~inn + year,
  data = llcs_panel,
  mem.clean = TRUE, verbose = 3, nthreads = 16)

model_roa_satisfied_key <- feols(
  create_formula("pp_case_satisfied_key"),
  cluster = ~ inn,
  panel.id = ~inn + year,
  data = llcs_panel,
  mem.clean = TRUE, verbose = 3, nthreads = 16)

models_names <- c("model_roa_any", "model_roa_satisfied", "model_roa_any_key", "model_roa_satisfied_key")

models_list <- mget(models_names)

results_roa_hist_lag1 <- rbindlist(Map(
  function(m, n) {extract_estimates(m, "roa", model_id = n)}, 
  models_list, 
  names(models_list)))

results_roa_hist_lag1$model <- forcats::fct_relevel(results_roa_hist_lag1$model, "model_roa_any", "model_roa_satisfied", "model_roa_any_key", "model_roa_satisfied_key")

results_roa_hist_lag1$order <- "1"
results_roa_hist_lag1$strain <- "hist"

gc(); gc()

results_lag1 <- rbind(results_roa_lag1, results_roa_ind_lag1, results_roa_hist_lag1)
results_lag1$n_cases_any <- llcs_panel[pp_case_any > 0, .N]
results_lag1$n_cases_satisfied <- llcs_panel[pp_case_satisfied > 0, .N]
results_lag1$n_cases_any_key <- llcs_panel[pp_case_any_key > 0, .N]
results_lag1$n_cases_satisfied_key <- llcs_panel[pp_case_satisfied_key > 0, .N]
results_lag1$n_cases_lasting <- llcs_panel[pp_case_lasting > 0, .N]

# PP t-2 ====


llcs_panel <- llcs_panel_full[roa_b2 != 0 & !is.na(roa_b2) & !is.na(roa_adj_ind_b2) & !is.na(roa_adj_hist_b2) & 
                                !is.na(market_share_b2) & !is.na(total_assets_b2) & !is.na(revenue_b2) & !is.na(lr_ind_year_b2) & !is.na(age_b2) & !is.na(family_element_b2) &
                                revenue_b2 > 0 & sole_shareholder_only == 0 & year >= 2014, ]

# PP models for ROA ====

control_vars <- c("roa_b2", "market_share_b2", 
                  "total_assets_b2", "revenue_b2", "lr_ind_year_b2", "age_b2", "age_b2_squared",
                  "n_participants_b2", "family_element_b2", "zero_interest_loan_b2",
                  "paid_zero_interest_b2")

model_roa_any <- feols(
  create_formula("pp_case_any"),
  cluster = ~ inn,
  panel.id = ~inn + year,
  data = llcs_panel,
  mem.clean = TRUE, verbose = 3, nthreads = 16)

model_roa_satisfied <- feols(
  create_formula("pp_case_satisfied"),
  cluster = ~ inn,
  panel.id = ~inn + year,
  data = llcs_panel,
  mem.clean = TRUE, verbose = 3, nthreads = 16)

model_roa_any_key <- feols(
  create_formula("pp_case_any_key"),
  cluster = ~ inn,
  panel.id = ~inn + year,
  data = llcs_panel,
  mem.clean = TRUE, verbose = 3, nthreads = 16)

model_roa_satisfied_key <- feols(
  create_formula("pp_case_satisfied_key"),
  cluster = ~ inn,
  panel.id = ~inn + year,
  data = llcs_panel,
  mem.clean = TRUE, verbose = 3, nthreads = 16)

models_names <- c("model_roa_any", "model_roa_satisfied", "model_roa_any_key", "model_roa_satisfied_key")

models_list <- mget(models_names)

results_roa_lag2 <- rbindlist(Map(
  function(m, n) {extract_estimates(m, "roa", model_id = n)}, 
  models_list, 
  names(models_list)))

results_roa_lag2$model <- forcats::fct_relevel(results_roa_lag2$model, "model_roa_any", "model_roa_satisfied", "model_roa_any_key", "model_roa_satisfied_key")

results_roa_lag2$order <- "2"
results_roa_lag2$strain <- "absolute"

gc()

# PP models for ROA industry adjusted ====

control_vars <- c("roa_adj_ind_b2", "market_share_b2", 
                  "total_assets_b2", "revenue_b2", "lr_ind_year_b2", "age_b2", "age_b2_squared",
                  "n_participants_b2", "family_element_b2", "zero_interest_loan_b2",
                  "paid_zero_interest_b2")

model_roa_any <- feols(
  create_formula("pp_case_any"),
  cluster = ~ inn,
  panel.id = ~inn + year,
  data = llcs_panel,
  mem.clean = TRUE, verbose = 3, nthreads = 16)

model_roa_satisfied <- feols(
  create_formula("pp_case_satisfied"),
  cluster = ~ inn,
  panel.id = ~inn + year,
  data = llcs_panel,
  mem.clean = TRUE, verbose = 3, nthreads = 16)

model_roa_any_key <- feols(
  create_formula("pp_case_any_key"),
  cluster = ~ inn,
  panel.id = ~inn + year,
  data = llcs_panel,
  mem.clean = TRUE, verbose = 3, nthreads = 16)

model_roa_satisfied_key <- feols(
  create_formula("pp_case_satisfied_key"),
  cluster = ~ inn,
  panel.id = ~inn + year,
  data = llcs_panel,
  mem.clean = TRUE, verbose = 3, nthreads = 16)

models_names <- c("model_roa_any", "model_roa_satisfied", "model_roa_any_key", "model_roa_satisfied_key")

models_list <- mget(models_names)

results_roa_ind_lag2 <- rbindlist(Map(
  function(m, n) {extract_estimates(m, "roa", model_id = n)}, 
  models_list, 
  names(models_list)))

results_roa_ind_lag2$model <- forcats::fct_relevel(results_roa_ind_lag2$model, "model_roa_any", "model_roa_satisfied", "model_roa_any_key", "model_roa_satisfied_key")

results_roa_ind_lag2$order <- "2"
results_roa_ind_lag2$strain <- "ind"

gc()


# PP models for ROA history adjusted ====

control_vars <- c("roa_adj_hist_b2", "market_share_b2", 
                  "total_assets_b2", "revenue_b2", "lr_ind_year_b2", "age_b2", "age_b2_squared",
                  "n_participants_b2", "family_element_b2", "zero_interest_loan_b2",
                  "paid_zero_interest_b2")

model_roa_any <- feols(
  create_formula("pp_case_any"),
  cluster = ~ inn,
  panel.id = ~inn + year,
  data = llcs_panel,
  mem.clean = TRUE, verbose = 3, nthreads = 16)

model_roa_satisfied <- feols(
  create_formula("pp_case_satisfied"),
  cluster = ~ inn,
  panel.id = ~inn + year,
  data = llcs_panel,
  mem.clean = TRUE, verbose = 3, nthreads = 16)

model_roa_any_key <- feols(
  create_formula("pp_case_any_key"),
  cluster = ~ inn,
  panel.id = ~inn + year,
  data = llcs_panel,
  mem.clean = TRUE, verbose = 3, nthreads = 16)

model_roa_satisfied_key <- feols(
  create_formula("pp_case_satisfied_key"),
  cluster = ~ inn,
  panel.id = ~inn + year,
  data = llcs_panel,
  mem.clean = TRUE, verbose = 3, nthreads = 16)

models_names <- c("model_roa_any", "model_roa_satisfied", "model_roa_any_key", "model_roa_satisfied_key")

models_list <- mget(models_names)

results_roa_hist_lag2 <- rbindlist(Map(
  function(m, n) {extract_estimates(m, "roa", model_id = n)}, 
  models_list, 
  names(models_list)))

results_roa_hist_lag2$model <- forcats::fct_relevel(results_roa_hist_lag2$model, "model_roa_any", "model_roa_satisfied", "model_roa_any_key", "model_roa_satisfied_key")

results_roa_hist_lag2$order <- "2"
results_roa_hist_lag2$strain <- "hist"

gc(); gc()

results_lag2 <- rbind(results_roa_lag2, results_roa_ind_lag2, results_roa_hist_lag2)
results_lag2$n_cases_any <- llcs_panel[pp_case_any > 0, .N]
results_lag2$n_cases_satisfied <- llcs_panel[pp_case_satisfied > 0, .N]
results_lag2$n_cases_any_key <- llcs_panel[pp_case_any_key > 0, .N]
results_lag2$n_cases_satisfied_key <- llcs_panel[pp_case_satisfied_key > 0, .N]
results_lag2$n_cases_lasting <- llcs_panel[pp_case_lasting > 0, .N]

# PP t-3 ====

llcs_panel <- llcs_panel_full[roa_b3 != 0 & !is.na(roa_b3) & !is.na(roa_adj_ind_b3) & !is.na(roa_adj_hist_b3) & 
                                !is.na(market_share_b3) & !is.na(total_assets_b3) & !is.na(revenue_b3) & !is.na(lr_ind_year_b3) & !is.na(age_b3) & !is.na(family_element_b3) &
                                revenue_b3 > 0 & sole_shareholder_only == 0 & year >= 2014, ]

llcs_panel[, .N] # 1194447

# PP models for ROA ====

control_vars <- c("roa_b3", "market_share_b3", 
                  "total_assets_b3", "revenue_b3", "lr_ind_year_b3", "age_b3", "age_b3_squared",
                  "n_participants_b3", "family_element_b3", "zero_interest_loan_b3",
                  "paid_zero_interest_b3")

model_roa_any <- feols(
  create_formula("pp_case_any"),
  cluster = ~ inn,
  panel.id = ~inn + year,
  data = llcs_panel,
  mem.clean = TRUE, verbose = 3, nthreads = 16)

model_roa_satisfied <- feols(
  create_formula("pp_case_satisfied"),
  cluster = ~ inn,
  panel.id = ~inn + year,
  data = llcs_panel,
  mem.clean = TRUE, verbose = 3, nthreads = 16)

model_roa_any_key <- feols(
  create_formula("pp_case_any_key"),
  cluster = ~ inn,
  panel.id = ~inn + year,
  data = llcs_panel,
  mem.clean = TRUE, verbose = 3, nthreads = 16)

model_roa_satisfied_key <- feols(
  create_formula("pp_case_satisfied_key"),
  cluster = ~ inn,
  panel.id = ~inn + year,
  data = llcs_panel,
  mem.clean = TRUE, verbose = 3, nthreads = 16)

models_names <- c("model_roa_any", "model_roa_satisfied", "model_roa_any_key", "model_roa_satisfied_key")


models_list <- mget(models_names)

results_roa_lag3 <- rbindlist(Map(
  function(m, n) {extract_estimates(m, "roa", model_id = n)}, 
  models_list, 
  names(models_list)))

results_roa_lag3$model <- forcats::fct_relevel(results_roa_lag3$model, "model_roa_any", "model_roa_satisfied", "model_roa_any_key", "model_roa_satisfied_key")

results_roa_lag3$order <- "3"
results_roa_lag3$strain <- "absolute"

gc()

# PP models for ROA industry adjusted ====

control_vars <- c("roa_adj_ind_b3", "market_share_b3", 
                  "total_assets_b3", "revenue_b3", "lr_ind_year_b3", "age_b3", "age_b3_squared",
                  "n_participants_b3", "family_element_b3", "zero_interest_loan_b3",
                  "paid_zero_interest_b3")

model_roa_any <- feols(
  create_formula("pp_case_any"),
  cluster = ~ inn,
  panel.id = ~inn + year,
  data = llcs_panel,
  mem.clean = TRUE, verbose = 3, nthreads = 16)

model_roa_satisfied <- feols(
  create_formula("pp_case_satisfied"),
  cluster = ~ inn,
  panel.id = ~inn + year,
  data = llcs_panel,
  mem.clean = TRUE, verbose = 3, nthreads = 16)

model_roa_any_key <- feols(
  create_formula("pp_case_any_key"),
  cluster = ~ inn,
  panel.id = ~inn + year,
  data = llcs_panel,
  mem.clean = TRUE, verbose = 3, nthreads = 16)

model_roa_satisfied_key <- feols(
  create_formula("pp_case_satisfied_key"),
  cluster = ~ inn,
  panel.id = ~inn + year,
  data = llcs_panel,
  mem.clean = TRUE, verbose = 3, nthreads = 16)

models_names <- c("model_roa_any", "model_roa_satisfied", "model_roa_any_key", "model_roa_satisfied_key")

models_list <- mget(models_names)

results_roa_ind_lag3 <- rbindlist(Map(
  function(m, n) {extract_estimates(m, "roa", model_id = n)}, 
  models_list, 
  names(models_list)))

results_roa_ind_lag3$model <- forcats::fct_relevel(results_roa_ind_lag3$model, "model_roa_any", "model_roa_satisfied", "model_roa_any_key", "model_roa_satisfied_key")

results_roa_ind_lag3$order <- "3"
results_roa_ind_lag3$strain <- "ind"

gc()


# PP models for ROA history adjusted ====
control_vars <- c("roa_adj_hist_b3", "market_share_b3", 
                  "total_assets_b3", "revenue_b3", "lr_ind_year_b3", "age_b3", "age_b3_squared",
                  "n_participants_b3", "family_element_b3", "zero_interest_loan_b3",
                  "paid_zero_interest_b3")

model_roa_any <- feols(
  create_formula("pp_case_any"),
  cluster = ~ inn,
  panel.id = ~inn + year,
  data = llcs_panel,
  mem.clean = TRUE, verbose = 3, nthreads = 16)

model_roa_satisfied <- feols(
  create_formula("pp_case_satisfied"),
  cluster = ~ inn,
  panel.id = ~inn + year,
  data = llcs_panel,
  mem.clean = TRUE, verbose = 3, nthreads = 16)

model_roa_any_key <- feols(
  create_formula("pp_case_any_key"),
  cluster = ~ inn,
  panel.id = ~inn + year,
  data = llcs_panel,
  mem.clean = TRUE, verbose = 3, nthreads = 16)

model_roa_satisfied_key <- feols(
  create_formula("pp_case_satisfied_key"),
  cluster = ~ inn,
  panel.id = ~inn + year,
  data = llcs_panel,
  mem.clean = TRUE, verbose = 3, nthreads = 16)

models_names <- c("model_roa_any", "model_roa_satisfied", "model_roa_any_key", "model_roa_satisfied_key")

models_list <- mget(models_names)

results_roa_hist_lag3 <- rbindlist(Map(
  function(m, n) {extract_estimates(m, "roa", model_id = n)}, 
  models_list, 
  names(models_list)))

results_roa_hist_lag3$model <- forcats::fct_relevel(results_roa_hist_lag3$model, "model_roa_any", "model_roa_satisfied", "model_roa_any_key", "model_roa_satisfied_key")

results_roa_hist_lag3$order <- "3"
results_roa_hist_lag3$strain <- "hist"

gc(); gc()

results_lag3 <- rbind(results_roa_lag3, results_roa_ind_lag3, results_roa_hist_lag3)
results_lag3$n_cases_any <- llcs_panel[pp_case_any > 0, .N]
results_lag3$n_cases_satisfied <- llcs_panel[pp_case_satisfied > 0, .N]
results_lag3$n_cases_any_key <- llcs_panel[pp_case_any_key > 0, .N]
results_lag3$n_cases_satisfied_key <- llcs_panel[pp_case_satisfied_key > 0, .N]
results_lag3$n_cases_lasting <- llcs_panel[pp_case_lasting > 0, .N]

# PP t-4 ====


llcs_panel <- llcs_panel_full[roa_b4 != 0 & !is.na(roa_b4) & !is.na(roa_adj_ind_b4) & !is.na(roa_adj_hist_b4) & 
                                !is.na(market_share_b4) & !is.na(total_assets_b4) & !is.na(revenue_b4) & !is.na(lr_ind_year_b4) & !is.na(age_b4) & !is.na(family_element_b4) &
                                revenue_b4 > 0 & sole_shareholder_only == 0 & year >= 2014, ]

llcs_panel[, .N] # 738969

# PP models for ROA ====
control_vars <- c("roa_b4", "market_share_b4", 
                  "total_assets_b4", "revenue_b4", "lr_ind_year_b4", "age_b4", "age_b4_squared",
                  "n_participants_b4", "family_element_b4", "zero_interest_loan_b4",
                  "paid_zero_interest_b4")

model_roa_any <- feols(
  create_formula("pp_case_any"),
  cluster = ~ inn,
  panel.id = ~inn + year,
  data = llcs_panel,
  mem.clean = TRUE, verbose = 3, nthreads = 16)

model_roa_satisfied <- feols(
  create_formula("pp_case_satisfied"),
  cluster = ~ inn,
  panel.id = ~inn + year,
  data = llcs_panel,
  mem.clean = TRUE, verbose = 3, nthreads = 16)

model_roa_any_key <- feols(
  create_formula("pp_case_any_key"),
  cluster = ~ inn,
  panel.id = ~inn + year,
  data = llcs_panel,
  mem.clean = TRUE, verbose = 3, nthreads = 16)

model_roa_satisfied_key <- feols(
  create_formula("pp_case_satisfied_key"),
  cluster = ~ inn,
  panel.id = ~inn + year,
  data = llcs_panel,
  mem.clean = TRUE, verbose = 3, nthreads = 16)

models_names <- c("model_roa_any", "model_roa_satisfied", "model_roa_any_key", "model_roa_satisfied_key")


models_list <- mget(models_names)

results_roa_lag4 <- rbindlist(Map(
  function(m, n) {extract_estimates(m, "roa", model_id = n)}, 
  models_list, 
  names(models_list)))

results_roa_lag4$model <- forcats::fct_relevel(results_roa_lag4$model, "model_roa_any", "model_roa_satisfied", "model_roa_any_key", "model_roa_satisfied_key")

results_roa_lag4$order <- "4"
results_roa_lag4$strain <- "absolute"

gc()

# PP models for ROA industry adjusted ====

control_vars <- c("roa_adj_ind_b4", "market_share_b4", 
                  "total_assets_b4", "revenue_b4", "lr_ind_year_b4", "age_b4", "age_b4_squared",
                  "n_participants_b4", "family_element_b4", "zero_interest_loan_b4",
                  "paid_zero_interest_b4")

model_roa_any <- feols(
  create_formula("pp_case_any"),
  cluster = ~ inn,
  panel.id = ~inn + year,
  data = llcs_panel,
  mem.clean = TRUE, verbose = 3, nthreads = 16)

model_roa_satisfied <- feols(
  create_formula("pp_case_satisfied"),
  cluster = ~ inn,
  panel.id = ~inn + year,
  data = llcs_panel,
  mem.clean = TRUE, verbose = 3, nthreads = 16)

model_roa_any_key <- feols(
  create_formula("pp_case_any_key"),
  cluster = ~ inn,
  panel.id = ~inn + year,
  data = llcs_panel,
  mem.clean = TRUE, verbose = 3, nthreads = 16)

model_roa_satisfied_key <- feols(
  create_formula("pp_case_satisfied_key"),
  cluster = ~ inn,
  panel.id = ~inn + year,
  data = llcs_panel,
  mem.clean = TRUE, verbose = 3, nthreads = 16)

models_names <- c("model_roa_any", "model_roa_satisfied", "model_roa_any_key", "model_roa_satisfied_key")

models_list <- mget(models_names)

results_roa_ind_lag4 <- rbindlist(Map(
  function(m, n) {extract_estimates(m, "roa", model_id = n)}, 
  models_list, 
  names(models_list)))

results_roa_ind_lag4$model <- forcats::fct_relevel(results_roa_ind_lag4$model, "model_roa_any", "model_roa_satisfied", "model_roa_any_key", "model_roa_satisfied_key")

results_roa_ind_lag4$order <- "4"
results_roa_ind_lag4$strain <- "ind"

gc()

# PP models for ROA history adjusted ====
control_vars <- c("roa_adj_hist_b4", "market_share_b4", 
                  "total_assets_b4", "revenue_b4", "lr_ind_year_b4", "age_b4", "age_b4_squared",
                  "n_participants_b4", "family_element_b4", "zero_interest_loan_b4",
                  "paid_zero_interest_b4")

model_roa_any <- feols(
  create_formula("pp_case_any"),
  cluster = ~ inn,
  panel.id = ~inn + year,
  data = llcs_panel,
  mem.clean = TRUE, verbose = 3, nthreads = 16)

model_roa_satisfied <- feols(
  create_formula("pp_case_satisfied"),
  cluster = ~ inn,
  panel.id = ~inn + year,
  data = llcs_panel,
  mem.clean = TRUE, verbose = 3, nthreads = 16)

model_roa_any_key <- feols(
  create_formula("pp_case_any_key"),
  cluster = ~ inn,
  panel.id = ~inn + year,
  data = llcs_panel,
  mem.clean = TRUE, verbose = 3, nthreads = 16)

model_roa_satisfied_key <- feols(
  create_formula("pp_case_satisfied_key"),
  cluster = ~ inn,
  panel.id = ~inn + year,
  data = llcs_panel,
  mem.clean = TRUE, verbose = 3, nthreads = 16)

models_names <- c("model_roa_any", "model_roa_satisfied", "model_roa_any_key", "model_roa_satisfied_key")

models_list <- mget(models_names)

results_roa_hist_lag4 <- rbindlist(Map(
  function(m, n) {extract_estimates(m, "roa", model_id = n)}, 
  models_list, 
  names(models_list)))

results_roa_hist_lag4$model <- forcats::fct_relevel(results_roa_hist_lag4$model, "model_roa_any", "model_roa_satisfied", "model_roa_any_key", "model_roa_satisfied_key")

results_roa_hist_lag4$order <- "4"
results_roa_hist_lag4$strain <- "hist"

gc(); gc()

results_lag4 <- rbind(results_roa_lag4, results_roa_ind_lag4, results_roa_hist_lag4)
results_lag4$n_cases_any <- llcs_panel[pp_case_any > 0, .N]
results_lag4$n_cases_satisfied <- llcs_panel[pp_case_satisfied > 0, .N]
results_lag4$n_cases_any_key <- llcs_panel[pp_case_any_key > 0, .N]
results_lag4$n_cases_satisfied_key <- llcs_panel[pp_case_satisfied_key > 0, .N]
results_lag4$n_cases_lasting <- llcs_panel[pp_case_lasting > 0, .N]

# Merge


# Merge the results ====
pp_models_results <- rbind(results_lag1, results_lag2, results_lag3, results_lag4)

gc()

fwrite(pp_models_results, paste0("plots_tables/estimation_results/disp_strain_pp_models_results_eight_transformed_sole_predictors.csv"))


