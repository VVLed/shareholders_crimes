library(data.table)
library(ggplot2)
library(fst)
library(fixest)
library(MatchIt); library(WeightIt)
library(cobalt)  
library(marginaleffects) 
library(ggpubr)
library(stringi); library(stringr)
options(scipen = 999)

extract_estimates <- function(x, regular_expression_1 = "roa_b", regular_expression_2 = "roa_adj_hist", model_id = "name") {
  data.table(
    roa_b1_slope = x$coefficients[str_detect(names(x$coefficients), regular_expression_1)][1],
    roa_hist_b2_slope = x$coefficients[str_detect(names(x$coefficients), regular_expression_2)][1],
    roa_b2_slope = x$coefficients[str_detect(names(x$coefficients), regular_expression_1)][2],
#    roa_hist_b2_slope = x$coefficients[str_detect(names(x$coefficients), regular_expression_2)][2],
    roa_b1_se = x$se[str_detect(names(x$se), regular_expression_1)][1],
    roa_hist_b2_se = x$se[str_detect(names(x$se), regular_expression_2)][1],
    roa_b2_se = x$se[str_detect(names(x$se), regular_expression_1)][2],
#    roa_hist_b2_se = x$se[str_detect(names(x$se), regular_expression_2)][2],
    firm_fe = sum(all.vars(x$fml_all$fixef) == "inn"),
    dv = all.vars(x$call$fml)[1],
    indv = all.vars(x$call$fml)[2],
    n_obs = x$nobs
  )
}


estimates <- data.frame()

extract_balance <- function(x,
                            cem_names = c("delete", 
                                                "n_all_ess_control", "n_all_unweighted_control", "n_matched_ess_control", "n_matched_unweighted_control", "n_unmatched_control", 
                                                "n_all_ess_treated", "n_all_unweighted_treated", "n_matched_ess_treated", "n_matched_unweighted_treated", "n_unmatched_treated"),
                            ebal_names = c("delete", "n_matched_ess_control", "n_all_unweighted_control", "n_matched_ess_treated", "n_all_unweighted_treated")
                            ) {
  baltable <- data.frame(x$Balance)
  baltable <- baltable[baltable$Type == "Contin.", ]
  baltable$vars <- rownames(baltable)
  baltable <- data.table(baltable)
  baltable <- baltable[, .(vars, sd_mean_diff  = round(Diff.Adj, 2), var_ratio = round(V.Ratio.Adj, 2), ks_stat = round(KS.Adj, 2))]
  obstable <- data.frame(x$Observations)
  obstable$vars <- rownames(obstable)
  obstable <- data.table(obstable)
  obstable <- dcast(obstable, . ~ vars, value.var = c("Control", "Treated"))
  if (x$call$method == "cem") {
    colnames(obstable) <- cem_names
  } else {
    colnames(obstable) <- ebal_names
  }
  obstable[, delete := NULL] 
  matching_stats_table <- cbind(baltable, obstable)
  matching_stats_table$dv <- all.vars(formula(x$call))[1]
  matching_stats_table$indv <- all.vars(formula(x$call))[2]
  matching_stats_table$method <- x$call$method
  return(matching_stats_table)
} 

matching_stats_table <- data.frame()

path <- Sys.getenv("SHAREHOLDERS_CRIMES_PATH")
setwd(path)

# Load the data ====

llcs_panel_full <- read_fst("panel_disp_vs_strain.fst", as.data.table = TRUE)

# cols_to_scale <- grep("(_case_)|(roa)", colnames(llcs_panel_full), value = TRUE)
# llcs_panel_full[, (cols_to_scale) := lapply(.SD, function (x) scale(x, center = TRUE, scale = TRUE)), .SDcols = cols_to_scale]
# gc()

cols_to_transform <- grep("roa", colnames(llcs_panel_full), value = TRUE)
llcs_panel_full[, (cols_to_transform) := lapply(.SD, function (x) asinh(x)), .SDcols = cols_to_transform]

cols_to_log <- c("total_assets_b3", "revenue_b3", "market_share_b3", "age_b3", "n_participants_b3")

llcs_panel_full[, paste0(cols_to_log, "_ln") := lapply(.SD, function (x) log(abs(x))), .SDcols = cols_to_log]

llcs_panel <- llcs_panel_full[roa_b4 != 0 & !is.na(roa_b4) & 
                              revenue_b3 != 0 & total_assets_b3 != 0 & !is.na(total_assets_b3) & !is.na(revenue_b3) &
                                !is.na(market_share_b3)  & !is.na(lr_ind_year_b3) & !is.na(age_b3) & !is.na(family_element_b3) &
                                sole_shareholder_only == 0 & year >= 2014 , ] # & 

## PP any ====

## CEM ====

matching_output <- matchit(pp_case_any ~ roa_b4 + revenue_b3_ln + total_assets_b3_ln + age_b3_ln + n_participants_b3_ln + family_element_b3 + okved_2dig,
                           data = llcs_panel, 
                           method = "cem",
                           estimand = "ATT",  
                           cutpoints = list(
                             roa_b4 = "q5", 
                             revenue_b3_ln = "q7", 
                             total_assets_b3_ln = "q7", 
                             age_b3_ln = c(0, 2.5)),
                           verbose = TRUE)

balance_table <-  bal.tab(matching_output, stats = c("m", "ks.statistics", "v"), binary = "std", continuous = "std")
matching_stats_table_interim <- extract_balance(balance_table)
matching_stats_table <- rbindlist(list(matching_stats_table, matching_stats_table_interim), fill = TRUE)

data_matched <- match_data(matching_output, drop.unmatched = TRUE)
cols_to_scale <- grep("(_case_)|(roa)", colnames(data_matched), value = TRUE)
data_matched[, (cols_to_scale) := lapply(.SD, function (x) scale(x, center = TRUE, scale = TRUE)), .SDcols = cols_to_scale]
gc()

data_matched[, n_by_inn := .N, by = inn]
data_matched[n_by_inn > 1, .N, pp_case_any]
#    pp_case_any     N
#          <num> <int>
# 1:           0 71433
# 2:           1  1493
match_model_no_fe <- feols(pp_case_any ~ roa_b4 + revenue_b3_ln + total_assets_b3_ln + age_b3_ln + lr_ind_year_b3 + n_participants_b3 + family_element_b3 | okved_2dig, data = data_matched, weights = data_matched$weights) 

etable(match_model_no_fe,
       fitstat = c("n", "r2"))

model_no_fe <- feols(
  pp_case_any ~ roa_b1 + roa_b2 + roa_adj_hist_b2 + log(market_share_b1) + log(total_assets_b1) + log(revenue_b1) + lr_ind_year_b1 + n_participants_b1 + family_element_b1 + zero_interest_loan_b1 + paid_zero_interest_b1 + log(market_share_b2) + log(total_assets_b2) + log(revenue_b2) + poly(age_b2, 2) + lr_ind_year_b2 + n_participants_b2 + family_element_b2 + zero_interest_loan_b2 + paid_zero_interest_b2 | year + okved_2dig,
  cluster = ~ inn,
  panel.id = ~inn + year,
  data = data_matched,
  weights = data_matched$weights,
  mem.clean = TRUE, verbose = 3, nthreads = 16)

model_with_fe <- feols(
  pp_case_any ~ roa_b1 + roa_b2 + roa_adj_hist_b2 + log(market_share_b1) + log(total_assets_b1) + log(revenue_b1) + lr_ind_year_b1 + n_participants_b1 + family_element_b1 + zero_interest_loan_b1 + paid_zero_interest_b1 + log(market_share_b2) + log(total_assets_b2) + log(revenue_b2) + poly(age_b2, 2) + lr_ind_year_b2 + n_participants_b2 + family_element_b2 + zero_interest_loan_b2 + paid_zero_interest_b2 | year + inn,
  cluster = ~ inn,
  panel.id = ~inn + year,
  data = data_matched,
  weights = data_matched$weights,
  mem.clean = TRUE, verbose = 3, nthreads = 16)

interim_estimates <- rbind(extract_estimates(model_no_fe), extract_estimates(model_with_fe))
interim_estimates$matching_method <- "cem"
estimates <- rbind(estimates, interim_estimates)

gc()

## EBAL ====

matching_output <- weightit(pp_case_any ~ roa_b4 + revenue_b3_ln + total_assets_b3_ln + age_b3_ln + n_participants_b3_ln + family_element_b3 + okved_2dig,
                       data = llcs_panel, 
                       method = "ebal",
                       estimand = "ATT",  
                       quantile = list(
                         roa_b4 = c(0.33, 0.66),
                         
                         revenue_b3_ln = c(0.25, 0.5, 0.75, 0.95),
                         total_assets_b3_ln = c(0.25, 0.5, 0.75, 0.95),
                         age_b3_ln = c(0.5, 0.66),
                         n_participants_b3_ln = c(0.66)
                       ),
                       d.moments = 3,
                       maxit = 1000)

balance_table <-  bal.tab(matching_output, stats = c("m", "ks.statistics", "v"), binary = "std", continuous = "std")
matching_stats_table_interim <- extract_balance(balance_table)
matching_stats_table <- rbindlist(list(matching_stats_table, matching_stats_table_interim), fill = TRUE)

llcs_panel$w_ent <- matching_output$weights

cols_to_scale <- grep("(_case_)|(roa)", colnames(llcs_panel), value = TRUE)
llcs_panel[, (cols_to_scale) := lapply(.SD, function (x) scale(x, center = TRUE, scale = TRUE)), .SDcols = cols_to_scale]

gc()

model_no_fe <- feols(
  pp_case_any ~ roa_b1 + roa_b2 + roa_adj_hist_b2 + log(market_share_b1) + log(total_assets_b1) + log(revenue_b1) + lr_ind_year_b1 + n_participants_b1 + family_element_b1 + zero_interest_loan_b1 + paid_zero_interest_b1 + log(market_share_b2) + log(total_assets_b2) + log(revenue_b2) + poly(age_b2, 2) + lr_ind_year_b2 + n_participants_b2 + family_element_b2 + zero_interest_loan_b2 + paid_zero_interest_b2 | year + okved_2dig,
  cluster = ~ inn,
  panel.id = ~inn + year,
  data = llcs_panel,
  weights = llcs_panel$w_ent,
  mem.clean = TRUE, verbose = 3, nthreads = 16)

model_with_fe <- feols(
  pp_case_any ~ roa_b1 + roa_b2 + roa_adj_hist_b2 + log(market_share_b1) + log(total_assets_b1) + log(revenue_b1) + lr_ind_year_b1 + n_participants_b1 + family_element_b1 + zero_interest_loan_b1 + paid_zero_interest_b1 + log(market_share_b2) + log(total_assets_b2) + log(revenue_b2) + poly(age_b2, 2) + lr_ind_year_b2 + n_participants_b2 + family_element_b2 + zero_interest_loan_b2 + paid_zero_interest_b2 | year + inn,
  cluster = ~ inn,
  panel.id = ~inn + year,
  data = llcs_panel,
  weights = llcs_panel$w_ent,
  mem.clean = TRUE, verbose = 3, nthreads = 16)

interim_estimates <- rbind(extract_estimates(model_no_fe), extract_estimates(model_with_fe))
interim_estimates$matching_method <- "ebal"
estimates <- rbind(estimates, interim_estimates)

gc()

## PP satisfied ====

llcs_panel <- llcs_panel_full[roa_b4 != 0 & !is.na(roa_b4) & 
                                revenue_b3 != 0 & total_assets_b3 != 0 & !is.na(total_assets_b3) & !is.na(revenue_b3) &
                                !is.na(market_share_b3)  & !is.na(lr_ind_year_b3) & !is.na(age_b3) & !is.na(family_element_b3) &
                                sole_shareholder_only == 0 & year >= 2014 , ] # & 

## CEM ====

matching_output <- matchit(pp_case_satisfied ~ roa_b4 + revenue_b3_ln + total_assets_b3_ln + age_b3_ln + n_participants_b3_ln + family_element_b3 + okved_2dig,
                           data = llcs_panel, 
                           method = "cem",
                           estimand = "ATT",  
                           cutpoints = list(
                             roa_b4 = "q5", 
                             
                             revenue_b3_ln = "q7", 
                             total_assets_b3_ln = "q7", 
                             age_b3_ln = c(0, 2.5)),
                           verbose = TRUE)

balance_table <-  bal.tab(matching_output, stats = c("m", "ks.statistics", "v"), binary = "std", continuous = "std")
balance_table
matching_stats_table_interim <- extract_balance(balance_table)
matching_stats_table <- rbindlist(list(matching_stats_table, matching_stats_table_interim), fill = TRUE)


data_matched <- match_data(matching_output, drop.unmatched = TRUE)
cols_to_scale <- grep("(_case_)|(roa)", colnames(data_matched), value = TRUE)
data_matched[, (cols_to_scale) := lapply(.SD, function (x) scale(x, center = TRUE, scale = TRUE)), .SDcols = cols_to_scale]

match_model_no_fe <- feols(pp_case_satisfied ~ roa_b4 + revenue_b3_ln + total_assets_b3_ln + age_b3_ln + lr_ind_year_b3 + n_participants_b3 + family_element_b3 | okved_2dig, data = data_matched, weights = data_matched$weights) 

etable(match_model_no_fe,
       fitstat = c("n", "r2"))

model_no_fe <- feols(
  pp_case_satisfied ~ roa_b1 + roa_b2 + roa_adj_hist_b2 + log(market_share_b1) + log(total_assets_b1) + log(revenue_b1) + lr_ind_year_b1 + n_participants_b1 + family_element_b1 + zero_interest_loan_b1 + paid_zero_interest_b1 + log(market_share_b2) + log(total_assets_b2) + log(revenue_b2) + poly(age_b2, 2) + lr_ind_year_b2 + n_participants_b2 + family_element_b2 + zero_interest_loan_b2 + paid_zero_interest_b2 | year + okved_2dig,
  cluster = ~ inn,
  panel.id = ~inn + year,
  data = data_matched,
  weights = data_matched$weights,
  mem.clean = TRUE, verbose = 3, nthreads = 16)

model_with_fe <- feols(
  pp_case_satisfied ~ roa_b1 + roa_b2 + roa_adj_hist_b2 + log(market_share_b1) + log(total_assets_b1) + log(revenue_b1) + lr_ind_year_b1 + n_participants_b1 + family_element_b1 + zero_interest_loan_b1 + paid_zero_interest_b1 + log(market_share_b2) + log(total_assets_b2) + log(revenue_b2) + poly(age_b2, 2) + lr_ind_year_b2 + n_participants_b2 + family_element_b2 + zero_interest_loan_b2 + paid_zero_interest_b2 | year + inn,
  cluster = ~ inn,
  panel.id = ~inn + year,
  data = data_matched,
  weights = data_matched$weights,
  mem.clean = TRUE, verbose = 3, nthreads = 16)

interim_estimates <- rbind(extract_estimates(model_no_fe), extract_estimates(model_with_fe))
interim_estimates$matching_method <- "cem"
estimates <- rbind(estimates, interim_estimates)

gc()

## EBAL ====

matching_output <- weightit(pp_case_satisfied ~ roa_b4 + revenue_b3_ln + total_assets_b3_ln + age_b3_ln + n_participants_b3_ln + family_element_b3 + okved_2dig,
                       data = llcs_panel, 
                       method = "ebal",
                       estimand = "ATT",  
                       quantile = list(
                         roa_b4 = c(0.33, 0.66),
                         
                         revenue_b3_ln = c(0.25, 0.5, 0.75, 0.95),
                         total_assets_b3_ln = c(0.25, 0.5, 0.75, 0.95),
                         age_b3_ln = c(0.5, 0.66),
                         n_participants_b3_ln = c(0.66)
                       ),
                       d.moments = 3,
                       maxit = 1000)

balance_table <-  bal.tab(matching_output, stats = c("m", "ks.statistics", "v"), binary = "std", continuous = "std")

matching_stats_table_interim <- extract_balance(balance_table)
matching_stats_table <- rbindlist(list(matching_stats_table, matching_stats_table_interim), fill = TRUE)


llcs_panel$w_ent <- matching_output$weights
cols_to_scale <- grep("(_case_)|(roa)", colnames(llcs_panel), value = TRUE)
llcs_panel[, (cols_to_scale) := lapply(.SD, function (x) scale(x, center = TRUE, scale = TRUE)), .SDcols = cols_to_scale]

model_no_fe <- feols(
  pp_case_satisfied ~ roa_b1 + roa_b2 + roa_adj_hist_b2 + log(market_share_b1) + log(total_assets_b1) + log(revenue_b1) + lr_ind_year_b1 + n_participants_b1 + family_element_b1 + zero_interest_loan_b1 + paid_zero_interest_b1 + log(market_share_b2) + log(total_assets_b2) + log(revenue_b2) + poly(age_b2, 2) + lr_ind_year_b2 + n_participants_b2 + family_element_b2 + zero_interest_loan_b2 + paid_zero_interest_b2 | year + okved_2dig,
  cluster = ~ inn,
  panel.id = ~inn + year,
  data = llcs_panel,
  weights = llcs_panel$w_ent,
  mem.clean = TRUE, verbose = 3, nthreads = 16)

model_with_fe <- feols(
  pp_case_satisfied ~ roa_b1 + roa_b2 + roa_adj_hist_b2 + log(market_share_b1) + log(total_assets_b1) + log(revenue_b1) + lr_ind_year_b1 + n_participants_b1 + family_element_b1 + zero_interest_loan_b1 + paid_zero_interest_b1 + log(market_share_b2) + log(total_assets_b2) + log(revenue_b2) + poly(age_b2, 2) + lr_ind_year_b2 + n_participants_b2 + family_element_b2 + zero_interest_loan_b2 + paid_zero_interest_b2 | year + inn,
  cluster = ~ inn,
  panel.id = ~inn + year,
  data = llcs_panel,
  weights = llcs_panel$w_ent,
  mem.clean = TRUE, verbose = 3, nthreads = 16)

interim_estimates <- rbind(extract_estimates(model_no_fe), extract_estimates(model_with_fe))
interim_estimates$matching_method <- "ebal"
estimates <- rbind(estimates, interim_estimates)
gc()

## PP key ====

llcs_panel <- llcs_panel_full[roa_b4 != 0 & !is.na(roa_b4) & 
                                revenue_b3 != 0 & total_assets_b3 != 0 & !is.na(total_assets_b3) & !is.na(revenue_b3) &
                                !is.na(market_share_b3)  & !is.na(lr_ind_year_b3) & !is.na(age_b3) & !is.na(family_element_b3) &
                                sole_shareholder_only == 0 & year >= 2014 , ] # & 

## CEM ====

matching_output <- matchit(pp_case_any_key ~ roa_b4 + revenue_b3_ln + total_assets_b3_ln + age_b3_ln + n_participants_b3_ln + family_element_b3 + okved_2dig,
                           data = llcs_panel, 
                           method = "cem",
                           estimand = "ATT",  
                           cutpoints = list(
                             roa_b4 = "q5", 
                             
                             revenue_b3_ln = "q7", 
                             total_assets_b3_ln = "q7", 
                             age_b3_ln = c(0, 2.5)),
                           verbose = TRUE)

balance_table <-  bal.tab(matching_output, stats = c("m", "ks.statistics", "v"), binary = "std", continuous = "std")
balance_table
matching_stats_table_interim <- extract_balance(balance_table)
matching_stats_table <- rbindlist(list(matching_stats_table, matching_stats_table_interim), fill = TRUE)

data_matched <- match_data(matching_output, drop.unmatched = TRUE)
cols_to_scale <- grep("(_case_)|(roa)", colnames(data_matched), value = TRUE)
data_matched[, (cols_to_scale) := lapply(.SD, function (x) scale(x, center = TRUE, scale = TRUE)), .SDcols = cols_to_scale]

match_model_no_fe <- feols(pp_case_any_key ~ roa_b4 + revenue_b3_ln + total_assets_b3_ln + age_b3_ln + lr_ind_year_b3 + n_participants_b3 + family_element_b3 | okved_2dig, data = data_matched, weights = data_matched$weights) 

etable(match_model_no_fe,
       fitstat = c("n", "r2"))

model_no_fe <- feols(
  pp_case_any_key ~ roa_b1 + roa_b2 + roa_adj_hist_b2 + log(market_share_b1) + log(total_assets_b1) + log(revenue_b1) + lr_ind_year_b1 + n_participants_b1 + family_element_b1 + zero_interest_loan_b1 + paid_zero_interest_b1 + log(market_share_b2) + log(total_assets_b2) + log(revenue_b2) + poly(age_b2, 2) + lr_ind_year_b2 + n_participants_b2 + family_element_b2 + zero_interest_loan_b2 + paid_zero_interest_b2 | year + okved_2dig,
  cluster = ~ inn,
  panel.id = ~inn + year,
  data = data_matched,
  weights = data_matched$weights,
  mem.clean = TRUE, verbose = 3, nthreads = 16)

model_with_fe <- feols(
  pp_case_any_key ~ roa_b1 + roa_b2 + roa_adj_hist_b2 + log(market_share_b1) + log(total_assets_b1) + log(revenue_b1) + lr_ind_year_b1 + n_participants_b1 + family_element_b1 + zero_interest_loan_b1 + paid_zero_interest_b1 + log(market_share_b2) + log(total_assets_b2) + log(revenue_b2) + poly(age_b2, 2) + lr_ind_year_b2 + n_participants_b2 + family_element_b2 + zero_interest_loan_b2 + paid_zero_interest_b2 | year + inn,
  cluster = ~ inn,
  panel.id = ~inn + year,
  data = data_matched,
  weights = data_matched$weights,
  mem.clean = TRUE, verbose = 3, nthreads = 16)

interim_estimates <- rbind(extract_estimates(model_no_fe), extract_estimates(model_with_fe))
interim_estimates$matching_method <- "cem"
estimates <- rbind(estimates, interim_estimates)

gc()

## EBAL ====

matching_output <- weightit(pp_case_any_key ~ roa_b4 + revenue_b3_ln + total_assets_b3_ln + age_b3_ln + n_participants_b3_ln + family_element_b3 + okved_2dig,
                       data = llcs_panel, 
                       method = "ebal",
                       estimand = "ATT",  
                       quantile = list(
                         roa_b4 = c(0.33, 0.66),
                         
                         revenue_b3_ln = c(0.25, 0.5, 0.75, 0.95),
                         total_assets_b3_ln = c(0.25, 0.5, 0.75, 0.95),
                         age_b3_ln = c(0.5, 0.66),
                         n_participants_b3_ln = c(0.66)
                       ),
                       d.moments = 3,
                       maxit = 1000)

balance_table <- bal.tab(matching_output, stats = c("m", "ks.statistics", "v"), binary = "std", continuous = "std")

matching_stats_table_interim <- extract_balance(balance_table)
matching_stats_table <- rbindlist(list(matching_stats_table, matching_stats_table_interim), fill = TRUE)


llcs_panel$w_ent <- matching_output$weights
cols_to_scale <- grep("(_case_)|(roa)", colnames(llcs_panel), value = TRUE)
llcs_panel[, (cols_to_scale) := lapply(.SD, function (x) scale(x, center = TRUE, scale = TRUE)), .SDcols = cols_to_scale]

model_no_fe <- feols(
  pp_case_any_key ~ roa_b1 + roa_b2 + roa_adj_hist_b2 + log(market_share_b1) + log(total_assets_b1) + log(revenue_b1) + lr_ind_year_b1 + n_participants_b1 + family_element_b1 + zero_interest_loan_b1 + paid_zero_interest_b1 + log(market_share_b2) + log(total_assets_b2) + log(revenue_b2) + poly(age_b2, 2) + lr_ind_year_b2 + n_participants_b2 + family_element_b2 + zero_interest_loan_b2 + paid_zero_interest_b2 | year + okved_2dig,
  cluster = ~ inn,
  panel.id = ~inn + year,
  data = llcs_panel,
  weights = llcs_panel$w_ent,
  mem.clean = TRUE, verbose = 3, nthreads = 16)

model_with_fe <- feols(
  pp_case_any_key ~ roa_b1 + roa_b2 + roa_adj_hist_b2 + log(market_share_b1) + log(total_assets_b1) + log(revenue_b1) + lr_ind_year_b1 + n_participants_b1 + family_element_b1 + zero_interest_loan_b1 + paid_zero_interest_b1 + log(market_share_b2) + log(total_assets_b2) + log(revenue_b2) + poly(age_b2, 2) + lr_ind_year_b2 + n_participants_b2 + family_element_b2 + zero_interest_loan_b2 + paid_zero_interest_b2 | year + inn,
  cluster = ~ inn,
  panel.id = ~inn + year,
  data = llcs_panel,
  weights = llcs_panel$w_ent,
  mem.clean = TRUE, verbose = 3, nthreads = 16)

interim_estimates <- rbind(extract_estimates(model_no_fe), extract_estimates(model_with_fe))
interim_estimates$matching_method <- "ebal"
estimates <- rbind(estimates, interim_estimates)

gc()

## PP satisfied key ====

llcs_panel <- llcs_panel_full[roa_b4 != 0 & !is.na(roa_b4) & 
                                revenue_b3 != 0 & total_assets_b3 != 0 & !is.na(total_assets_b3) & !is.na(revenue_b3) &
                                !is.na(market_share_b3)  & !is.na(lr_ind_year_b3) & !is.na(age_b3) & !is.na(family_element_b3) &
                                sole_shareholder_only == 0 & year >= 2014 , ] # & 

## CEM ====

matching_output <- matchit(pp_case_satisfied_key ~ roa_b4 + revenue_b3_ln + total_assets_b3_ln + age_b3_ln + n_participants_b3_ln + family_element_b3 + okved_2dig,
                           data = llcs_panel, 
                           method = "cem",
                           estimand = "ATT",  
                           cutpoints = list(
                             roa_b4 = "q5", 
                             
                             revenue_b3_ln = "q7", 
                             total_assets_b3_ln = "q7", 
                             age_b3_ln = c(0, 2.5)),
                           verbose = TRUE)

balance_table <-  bal.tab(matching_output, stats = c("m", "ks.statistics", "v"), binary = "std", continuous = "std")
matching_stats_table_interim <- extract_balance(balance_table)
matching_stats_table <- rbindlist(list(matching_stats_table, matching_stats_table_interim), fill = TRUE)


data_matched <- match_data(matching_output, drop.unmatched = TRUE)
cols_to_scale <- grep("(_case_)|(roa)", colnames(data_matched), value = TRUE)
data_matched[, (cols_to_scale) := lapply(.SD, function (x) scale(x, center = TRUE, scale = TRUE)), .SDcols = cols_to_scale]


match_model_no_fe <- feols(pp_case_satisfied_key ~ roa_b4 + revenue_b3_ln + total_assets_b3_ln + age_b3_ln + lr_ind_year_b3 + n_participants_b3 + family_element_b3 | okved_2dig, data = data_matched, weights = data_matched$weights) 

etable(match_model_no_fe,
       fitstat = c("n", "r2"))

model_no_fe <- feols(
  pp_case_satisfied_key ~ roa_b1 + roa_b2 + roa_adj_hist_b2 + log(market_share_b1) + log(total_assets_b1) + log(revenue_b1) + lr_ind_year_b1 + n_participants_b1 + family_element_b1 + zero_interest_loan_b1 + paid_zero_interest_b1 + log(market_share_b2) + log(total_assets_b2) + log(revenue_b2) + poly(age_b2, 2) + lr_ind_year_b2 + n_participants_b2 + family_element_b2 + zero_interest_loan_b2 + paid_zero_interest_b2 | year + okved_2dig,
  cluster = ~ inn,
  panel.id = ~inn + year,
  data = data_matched,
  weights = data_matched$weights,
  mem.clean = TRUE, verbose = 3, nthreads = 16)

model_with_fe <- feols(
  pp_case_satisfied_key ~ roa_b1 + roa_b2 + roa_adj_hist_b2 + log(market_share_b1) + log(total_assets_b1) + log(revenue_b1) + lr_ind_year_b1 + n_participants_b1 + family_element_b1 + zero_interest_loan_b1 + paid_zero_interest_b1 + log(market_share_b2) + log(total_assets_b2) + log(revenue_b2) + poly(age_b2, 2) + lr_ind_year_b2 + n_participants_b2 + family_element_b2 + zero_interest_loan_b2 + paid_zero_interest_b2 | year + inn,
  cluster = ~ inn,
  panel.id = ~inn + year,
  data = data_matched,
  weights = data_matched$weights,
  mem.clean = TRUE, verbose = 3, nthreads = 16)

interim_estimates <- rbind(extract_estimates(model_no_fe), extract_estimates(model_with_fe))
interim_estimates$matching_method <- "cem"
estimates <- rbind(estimates, interim_estimates)

gc()

## EBAL ====

matching_output <- weightit(pp_case_satisfied_key ~ roa_b4 + revenue_b3_ln + total_assets_b3_ln + age_b3_ln + n_participants_b3_ln + family_element_b3 + okved_2dig,
                       data = llcs_panel, 
                       method = "ebal",
                       estimand = "ATT",  
                       quantile = list(
                         roa_b4 = c(0.33, 0.66),
                         
                         revenue_b3_ln = c(0.25, 0.5, 0.75, 0.95),
                         total_assets_b3_ln = c(0.25, 0.5, 0.75, 0.95),
                         age_b3_ln = c(0.5, 0.66),
                         n_participants_b3_ln = c(0.66)
                       ),
                       d.moments = 3,
                       maxit = 1000)

balance_table <-  bal.tab(matching_output, stats = c("m", "ks.statistics", "v"), binary = "std", continuous = "std")
matching_stats_table_interim <- extract_balance(balance_table)
matching_stats_table <- rbindlist(list(matching_stats_table, matching_stats_table_interim), fill = TRUE)


llcs_panel$w_ent <- matching_output$weights
cols_to_scale <- grep("(_case_)|(roa)", colnames(llcs_panel), value = TRUE)
llcs_panel[, (cols_to_scale) := lapply(.SD, function (x) scale(x, center = TRUE, scale = TRUE)), .SDcols = cols_to_scale]

model_no_fe <- feols(
  pp_case_satisfied_key ~ roa_b1 + roa_b2 + roa_adj_hist_b2 + log(market_share_b1) + log(total_assets_b1) + log(revenue_b1) + lr_ind_year_b1 + n_participants_b1 + family_element_b1 + zero_interest_loan_b1 + paid_zero_interest_b1 + log(market_share_b2) + log(total_assets_b2) + log(revenue_b2) + poly(age_b2, 2) + lr_ind_year_b2 + n_participants_b2 + family_element_b2 + zero_interest_loan_b2 + paid_zero_interest_b2 | year + okved_2dig,
  cluster = ~ inn,
  panel.id = ~inn + year,
  data = llcs_panel,
  weights = llcs_panel$w_ent,
  mem.clean = TRUE, verbose = 3, nthreads = 16)

model_with_fe <- feols(
  pp_case_satisfied_key ~ roa_b1 + roa_b2 + roa_adj_hist_b2 + log(market_share_b1) + log(total_assets_b1) + log(revenue_b1) + lr_ind_year_b1 + n_participants_b1 + family_element_b1 + zero_interest_loan_b1 + paid_zero_interest_b1 + log(market_share_b2) + log(total_assets_b2) + log(revenue_b2) + poly(age_b2, 2) + lr_ind_year_b2 + n_participants_b2 + family_element_b2 + zero_interest_loan_b2 + paid_zero_interest_b2 | year + inn,
  cluster = ~ inn,
  panel.id = ~inn + year,
  data = llcs_panel,
  weights = llcs_panel$w_ent,
  mem.clean = TRUE, verbose = 3, nthreads = 16)

interim_estimates <- rbind(extract_estimates(model_no_fe), extract_estimates(model_with_fe))
interim_estimates$matching_method <- "ebal"
estimates <- rbind(estimates, interim_estimates)

gc()

fwrite(matching_stats_table, paste0("plots_tables/matching_results/matching_stats_raw.csv"))
fwrite(estimates, paste0("plots_tables/matching_results/matching_estimates.csv"))
