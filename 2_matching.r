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
    roa_b1_se = x$se[str_detect(names(x$se), regular_expression_1)][1],
    roa_hist_b2_se = x$se[str_detect(names(x$se), regular_expression_2)][1],
    roa_b2_se = x$se[str_detect(names(x$se), regular_expression_1)][2],
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

path <- Sys.getenv("PUBLIC_FINANCIALS_PATH")
path_d <- Sys.getenv("DRIVE_D")
setwd(path)

# Load the data ====

llcs_panel_full <- read_fst("data/arbitrazh_cases/panel_disp_vs_strain.fst", as.data.table = TRUE)

cols_to_transform <- grep("roa", colnames(llcs_panel_full), value = TRUE)
llcs_panel_full[, (cols_to_transform) := lapply(.SD, function (x) asinh(x)), .SDcols = cols_to_transform]

cols_to_log <- c("total_assets_b3", "revenue_b3", "market_share_b3", "age_b3", "n_participants_b3")

llcs_panel_full[, paste0(cols_to_log, "_ln") := lapply(.SD, function (x) log(abs(x))), .SDcols = cols_to_log]
cols_to_log <- c("total_assets_b4", "revenue_b4", "market_share_b4", "age_b4", "n_participants_b4")

llcs_panel_full[, paste0(cols_to_log, "_ln") := lapply(.SD, function (x) log(abs(x))), .SDcols = cols_to_log]

# Aggregate estimates ====

extract_joint_test_all_roa <- function(x, model_id = "name") {
  wald_results <- wald(x, 
                       keep = "roa_b2|roa_b3|roa_b4",
                       cluster = "inn",
                       print = FALSE)
  results <- data.table(
    wald_stat = wald_results$stat,
    wald_p =  wald_results$p,
    model = model_id,
    strain = "absolute"
  )
  return(results)
}

extract_cum_effect_all_roa <- function(x, model_id = "name") {
  cum_results <- hypotheses(x,
                            hypothesis = "roa_b2 + roa_b3 + roa_b4 = 0")
  results <- data.table(
    estimate = cum_results$estimate,
    se = cum_results$std.error,
    p_value =  cum_results$p.value,
    model = model_id,
    strain = "absolute"
  )
  return(results)
}

## PP satisfied ====

llcs_panel <- llcs_panel_full[roa_b5 != 0 & !is.na(roa_b5) & 
                                revenue_b4 != 0 & total_assets_b4 != 0 & !is.na(total_assets_b4) & !is.na(revenue_b4) &
                                !is.na(market_share_b4)  & !is.na(leverage_b4) & !is.na(age_b4) & !is.na(family_element_b4) &
                                sole_shareholder_only == 0 & year >= 2014 , ] # & 

## CEM ====

matching_output <- matchit(pp_case_satisfied ~ roa_b5 + revenue_b4_ln + total_assets_b4_ln + age_b4_ln + n_participants_b4_ln + family_element_b4 + okved_2dig,
                           data = llcs_panel, 
                           method = "cem",
                           estimand = "ATT",  
                           cutpoints = list(
                             roa_b5 = "q5", 
                             revenue_b4_ln = "q7", 
                             total_assets_b4_ln = "q7", 
                             age_b4_ln = c(0, 2.5)),
                           verbose = TRUE)

balance_table <-  bal.tab(matching_output, stats = c("m", "ks.statistics", "v"), binary = "std", continuous = "std")
matching_stats_table_interim <- extract_balance(balance_table)
matching_stats_table <- rbindlist(list(matching_stats_table, matching_stats_table_interim), fill = TRUE)

data_matched <- match_data(matching_output, drop.unmatched = TRUE)

# Run model without standardization
model_with_fe <- feols(
  pp_case_satisfied ~ roa_b2 + roa_b3 + roa_b4 + log(market_share_b2) + log(total_assets_b2) + log(revenue_b2) + leverage_b2 + n_participants_b2 + family_element_b2 + zero_interest_loan_b2 + paid_zero_interest_b2 + log(market_share_b3) + log(total_assets_b3) + log(revenue_b3) + log(age) + leverage_b3 + n_participants_b3 + family_element_b3 + zero_interest_loan_b3 + paid_zero_interest_b3 | year + inn,
  cluster = ~ inn,
  panel.id = ~inn + year,
  data = data_matched,
  weights = data_matched$weights,
  mem.clean = TRUE, verbose = 3, nthreads = 16)

joint_test <- extract_joint_test_all_roa(model_with_fe)
joint_test$method <-  "cem"
joint_test$model <- "pp_case_satisfied"
joint_test$standardized <- "no"
cum_effect <- extract_cum_effect_all_roa(model_with_fe)
cum_effect$method <- "cem" 
cum_effect$model <- "pp_case_satisfied" 
cum_effect$standardized <- "no"

cols_to_scale <- grep("(_case_)|(roa)", colnames(data_matched), value = TRUE)
data_matched[, (cols_to_scale) := lapply(.SD, function (x) scale(x, center = TRUE, scale = TRUE)), .SDcols = cols_to_scale]

model_with_fe <- feols(
  pp_case_satisfied ~ roa_b2 + roa_b3 + roa_b4 + log(market_share_b2) + log(total_assets_b2) + log(revenue_b2) + leverage_b2 + n_participants_b2 + family_element_b2 + zero_interest_loan_b2 + paid_zero_interest_b2 + log(market_share_b3) + log(total_assets_b3) + log(revenue_b3) + log(age) + leverage_b3 + n_participants_b3 + family_element_b3 + zero_interest_loan_b3 + paid_zero_interest_b3 | year + inn,
  cluster = ~ inn,
  panel.id = ~inn + year,
  data = data_matched,
  weights = data_matched$weights,
  mem.clean = TRUE, verbose = 3, nthreads = 16)

joint_test_interim <- extract_joint_test_all_roa(model_with_fe)
joint_test_interim$method <-  "cem"
joint_test_interim$model <- "pp_case_satisfied"
joint_test_interim$standardized <- "yes"

cum_effect_interim <- extract_cum_effect_all_roa(model_with_fe)
cum_effect_interim$method <- "cem" 
cum_effect_interim$model <- "pp_case_satisfied"
cum_effect_interim$standardized <- "yes"

joint_test <- rbind(joint_test, joint_test_interim)
cum_effect <- rbind(cum_effect, cum_effect_interim)

gc()

## EBAL ====

matching_output <- weightit(pp_case_satisfied ~ roa_b5 + revenue_b4_ln + total_assets_b4_ln + age_b4_ln + n_participants_b4_ln + family_element_b4 + okved_2dig,
                            data = llcs_panel, 
                            method = "ebal",
                            estimand = "ATT",  
                            quantile = list(
                              roa_b5 = c(0.33, 0.66),
                              revenue_b4_ln = c(0.25, 0.5, 0.75, 0.95),
                              total_assets_b4_ln = c(0.25, 0.5, 0.75, 0.95),
                              age_b4_ln = c(0.5, 0.66),
                              n_participants_b4_ln = c(0.66, 0.85)
                            ),
                            d.moments = 3,
                            maxit = 1000)

balance_table <-  bal.tab(matching_output, stats = c("m", "ks.statistics", "v"), binary = "std", continuous = "std")
matching_stats_table_interim <- extract_balance(balance_table)
matching_stats_table <- rbindlist(list(matching_stats_table, matching_stats_table_interim), fill = TRUE)

llcs_panel$w_ent <- matching_output$weights

model_with_fe <- feols(
  pp_case_satisfied ~ roa_b2 + roa_b3 + roa_b4 + log(market_share_b2) + log(total_assets_b2) + log(revenue_b2) + leverage_b2 + n_participants_b2 + family_element_b2 + zero_interest_loan_b2 + paid_zero_interest_b2 + log(market_share_b3) + log(total_assets_b3) + log(revenue_b3) + log(age) + leverage_b3 + n_participants_b3 + family_element_b3 + zero_interest_loan_b3 + paid_zero_interest_b3 | year + inn,
  cluster = ~ inn,
  panel.id = ~inn + year,
  data = llcs_panel,
  weights = llcs_panel$w_ent,
  mem.clean = TRUE, verbose = 3, nthreads = 16)

joint_test_interim <- extract_joint_test_all_roa(model_with_fe)
joint_test_interim$method <-  "ebal"
joint_test_interim$model <- "pp_case_satisfied"
joint_test_interim$standardized <- "no"

cum_effect_interim <- extract_cum_effect_all_roa(model_with_fe)
cum_effect_interim$method <- "ebal" 
cum_effect_interim$model <- "pp_case_satisfied"
cum_effect_interim$standardized <- "no"

joint_test <- rbind(joint_test, joint_test_interim)
cum_effect <- rbind(cum_effect, cum_effect_interim)

cols_to_scale <- grep("(_case_)|(roa)", colnames(llcs_panel), value = TRUE)
llcs_panel[, (cols_to_scale) := lapply(.SD, function (x) scale(x, center = TRUE, scale = TRUE)), .SDcols = cols_to_scale]

model_with_fe <- feols(
  pp_case_satisfied ~ roa_b2 + roa_b3 + roa_b4 + log(market_share_b2) + log(total_assets_b2) + log(revenue_b2) + leverage_b2 + n_participants_b2 + family_element_b2 + zero_interest_loan_b2 + paid_zero_interest_b2 + log(market_share_b3) + log(total_assets_b3) + log(revenue_b3) + log(age) + leverage_b3 + n_participants_b3 + family_element_b3 + zero_interest_loan_b3 + paid_zero_interest_b3 | year + inn,
  cluster = ~ inn,
  panel.id = ~inn + year,
  data = llcs_panel,
  weights = llcs_panel$w_ent,
  mem.clean = TRUE, verbose = 3, nthreads = 16)

joint_test_interim <- extract_joint_test_all_roa(model_with_fe)
joint_test_interim$method <-  "ebal"
joint_test_interim$model <- "pp_case_satisfied"
joint_test_interim$standardized <- "yes"

cum_effect_interim <- extract_cum_effect_all_roa(model_with_fe)
cum_effect_interim$method <- "ebal" 
cum_effect_interim$model <- "pp_case_satisfied"
cum_effect_interim$standardized <- "yes"

joint_test <- rbind(joint_test, joint_test_interim)
cum_effect <- rbind(cum_effect, cum_effect_interim)

## PP unsatisfied ====

llcs_panel <- llcs_panel_full[roa_b5 != 0 & !is.na(roa_b5) & 
                                revenue_b4 != 0 & total_assets_b4 != 0 & !is.na(total_assets_b4) & !is.na(revenue_b4) &
                                !is.na(market_share_b4)  & !is.na(leverage_b4) & !is.na(age_b4) & !is.na(family_element_b4) &
                                sole_shareholder_only == 0 & year >= 2014 , ] # & 

## CEM ====
matching_output <- matchit(pp_case_unsatisfied ~ roa_b5 + revenue_b4_ln + total_assets_b4_ln + age_b4_ln + n_participants_b4_ln + family_element_b4 + okved_2dig,
                           data = llcs_panel, 
                           method = "cem",
                           estimand = "ATT",  
                           cutpoints = list(
                             roa_b5 = "q5", 
                             revenue_b4_ln = "q7", 
                             total_assets_b4_ln = "q7", 
                             age_b4_ln = c(0, 2.5)),
                           verbose = TRUE)

balance_table <-  bal.tab(matching_output, stats = c("m", "ks.statistics", "v"), binary = "std", continuous = "std")
matching_stats_table_interim <- extract_balance(balance_table)
matching_stats_table <- rbindlist(list(matching_stats_table, matching_stats_table_interim), fill = TRUE)

data_matched <- match_data(matching_output, drop.unmatched = TRUE)

model_with_fe <- feols(
  pp_case_unsatisfied ~ roa_b2 + roa_b3 + roa_b4 + log(market_share_b2) + log(total_assets_b2) + log(revenue_b2) + leverage_b2 + n_participants_b2 + family_element_b2 + zero_interest_loan_b2 + paid_zero_interest_b2 + log(market_share_b3) + log(total_assets_b3) + log(revenue_b3) + log(age) + leverage_b3 + n_participants_b3 + family_element_b3 + zero_interest_loan_b3 + paid_zero_interest_b3 | year + inn,
  cluster = ~ inn,
  panel.id = ~inn + year,
  data = data_matched,
  weights = data_matched$weights,
  mem.clean = TRUE, verbose = 3, nthreads = 16)

joint_test_interim <- extract_joint_test_all_roa(model_with_fe)
joint_test_interim$method <-  "cem"
joint_test_interim$model <- "pp_case_unsatisfied"
joint_test_interim$standardized <- "no"

cum_effect_interim <- extract_cum_effect_all_roa(model_with_fe)
cum_effect_interim$method <- "cem" 
cum_effect_interim$model <- "pp_case_unsatisfied"
cum_effect_interim$standardized <- "no"

joint_test <- rbind(joint_test, joint_test_interim)
cum_effect <- rbind(cum_effect, cum_effect_interim)

cols_to_scale <- grep("(_case_)|(roa)", colnames(data_matched), value = TRUE)
data_matched[, (cols_to_scale) := lapply(.SD, function (x) scale(x, center = TRUE, scale = TRUE)), .SDcols = cols_to_scale]

model_with_fe <- feols(
  pp_case_unsatisfied ~ roa_b2 + roa_b3 + roa_b4 + log(market_share_b2) + log(total_assets_b2) + log(revenue_b2) + leverage_b2 + n_participants_b2 + family_element_b2 + zero_interest_loan_b2 + paid_zero_interest_b2 + log(market_share_b3) + log(total_assets_b3) + log(revenue_b3) + log(age) + leverage_b3 + n_participants_b3 + family_element_b3 + zero_interest_loan_b3 + paid_zero_interest_b3 | year + inn,
  cluster = ~ inn,
  panel.id = ~inn + year,
  data = data_matched,
  weights = data_matched$weights,
  mem.clean = TRUE, verbose = 3, nthreads = 16)

joint_test_interim <- extract_joint_test_all_roa(model_with_fe)
joint_test_interim$method <-  "cem"
joint_test_interim$model <- "pp_case_unsatisfied"
joint_test_interim$standardized <- "yes"

cum_effect_interim <- extract_cum_effect_all_roa(model_with_fe)
cum_effect_interim$method <- "cem" 
cum_effect_interim$model <- "pp_case_unsatisfied"
cum_effect_interim$standardized <- "yes"

joint_test <- rbind(joint_test, joint_test_interim)
cum_effect <- rbind(cum_effect, cum_effect_interim)

gc()

## EBAL ====

matching_output <- weightit(pp_case_unsatisfied ~ roa_b5 + revenue_b4_ln + total_assets_b4_ln + age_b4_ln + n_participants_b4_ln + family_element_b4 + okved_2dig,
                            data = llcs_panel, 
                            method = "ebal",
                            estimand = "ATT",  
                            quantile = list(
                              roa_b5 = c(0.33, 0.66),
                              revenue_b4_ln = c(0.25, 0.5, 0.75, 0.95),
                              total_assets_b4_ln = c(0.25, 0.5, 0.75, 0.95),
                              age_b4_ln = c(0.5, 0.66),
                              n_participants_b4_ln = c(0.66, 0.85)
                            ),
                            d.moments = 3,
                            maxit = 1000)

balance_table <-  bal.tab(matching_output, stats = c("m", "ks.statistics", "v"), binary = "std", continuous = "std")
matching_stats_table_interim <- extract_balance(balance_table)
matching_stats_table <- rbindlist(list(matching_stats_table, matching_stats_table_interim), fill = TRUE)

llcs_panel$w_ent <- matching_output$weights

model_with_fe <- feols(
  pp_case_unsatisfied ~ roa_b2 + roa_b3 + roa_b4 + log(market_share_b2) + log(total_assets_b2) + log(revenue_b2) + leverage_b2 + n_participants_b2 + family_element_b2 + zero_interest_loan_b2 + paid_zero_interest_b2 + log(market_share_b3) + log(total_assets_b3) + log(revenue_b3) + log(age) + leverage_b3 + n_participants_b3 + family_element_b3 + zero_interest_loan_b3 + paid_zero_interest_b3 | year + inn,
  cluster = ~ inn,
  panel.id = ~inn + year,
  data = llcs_panel,
  weights = llcs_panel$w_ent,
  mem.clean = TRUE, verbose = 3, nthreads = 16)

joint_test_interim <- extract_joint_test_all_roa(model_with_fe)
joint_test_interim$method <-  "ebal"
joint_test_interim$model <- "pp_case_unsatisfied"
joint_test_interim$standardized <- "no"

cum_effect_interim <- extract_cum_effect_all_roa(model_with_fe)
cum_effect_interim$method <- "ebal" 
cum_effect_interim$model <- "pp_case_unsatisfied"
cum_effect_interim$standardized <- "no"

joint_test <- rbind(joint_test, joint_test_interim)
cum_effect <- rbind(cum_effect, cum_effect_interim)

cols_to_scale <- grep("(_case_)|(roa)", colnames(llcs_panel), value = TRUE)
llcs_panel[, (cols_to_scale) := lapply(.SD, function (x) scale(x, center = TRUE, scale = TRUE)), .SDcols = cols_to_scale]

model_with_fe <- feols(
  pp_case_unsatisfied ~ roa_b2 + roa_b3 + roa_b4 + log(market_share_b2) + log(total_assets_b2) + log(revenue_b2) + leverage_b2 + n_participants_b2 + family_element_b2 + zero_interest_loan_b2 + paid_zero_interest_b2 + log(market_share_b3) + log(total_assets_b3) + log(revenue_b3) + log(age) + leverage_b3 + n_participants_b3 + family_element_b3 + zero_interest_loan_b3 + paid_zero_interest_b3 | year + inn,
  cluster = ~ inn,
  panel.id = ~inn + year,
  data = llcs_panel,
  weights = llcs_panel$w_ent,
  mem.clean = TRUE, verbose = 3, nthreads = 16)

joint_test_interim <- extract_joint_test_all_roa(model_with_fe)
joint_test_interim$method <-  "ebal"
joint_test_interim$model <- "pp_case_unsatisfied"
joint_test_interim$standardized <- "yes"

cum_effect_interim <- extract_cum_effect_all_roa(model_with_fe)
cum_effect_interim$method <- "ebal" 
cum_effect_interim$model <- "pp_case_unsatisfied"
cum_effect_interim$standardized <- "yes"

joint_test <- rbind(joint_test, joint_test_interim)
cum_effect <- rbind(cum_effect, cum_effect_interim)

## PP criminal ====

llcs_panel <- llcs_panel_full[roa_b5 != 0 & !is.na(roa_b5) & 
                                revenue_b4 != 0 & total_assets_b4 != 0 & !is.na(total_assets_b4) & !is.na(revenue_b4) &
                                !is.na(market_share_b4)  & !is.na(leverage_b4) & !is.na(age_b4) & !is.na(family_element_b4) &
                                sole_shareholder_only == 0 & year >= 2014 , ] # & 

## CEM ====

matching_output <- matchit(pp_case_any_criminal ~ roa_b5 + revenue_b4_ln + total_assets_b4_ln + age_b4_ln + n_participants_b4_ln + family_element_b4 + okved_2dig,
                           data = llcs_panel, 
                           method = "cem",
                           estimand = "ATT",  
                           cutpoints = list(
                             roa_b5 = "q5", 
                             
                             revenue_b4_ln = "q7", 
                             total_assets_b4_ln = "q7", 
                             age_b4_ln = c(0, 2.5)),
                           verbose = TRUE)

balance_table <-  bal.tab(matching_output, stats = c("m", "ks.statistics", "v"), binary = "std", continuous = "std")
matching_stats_table_interim <- extract_balance(balance_table)
matching_stats_table <- rbindlist(list(matching_stats_table, matching_stats_table_interim), fill = TRUE)

data_matched <- match_data(matching_output, drop.unmatched = TRUE)

model_with_fe <- feols(
  pp_case_any_criminal ~ roa_b2 + roa_b3 + roa_b4 + log(market_share_b2) + log(total_assets_b2) + log(revenue_b2) + leverage_b2 + n_participants_b2 + family_element_b2 + zero_interest_loan_b2 + paid_zero_interest_b2 + log(market_share_b3) + log(total_assets_b3) + log(revenue_b3) + log(age) + leverage_b3 + n_participants_b3 + family_element_b3 + zero_interest_loan_b3 + paid_zero_interest_b3 | year + inn,
  cluster = ~ inn,
  panel.id = ~inn + year,
  data = data_matched,
  weights = data_matched$weights,
  mem.clean = TRUE, verbose = 3, nthreads = 16)

joint_test_interim <- extract_joint_test_all_roa(model_with_fe)
joint_test_interim$method <-  "cem"
joint_test_interim$model <- "pp_case_any_criminal"
joint_test_interim$standardized <- "no"

cum_effect_interim <- extract_cum_effect_all_roa(model_with_fe)
cum_effect_interim$method <- "cem" 
cum_effect_interim$model <- "pp_case_any_criminal" 
cum_effect_interim$standardized <- "no"

joint_test <- rbind(joint_test, joint_test_interim)
cum_effect <- rbind(cum_effect, cum_effect_interim)

cols_to_scale <- grep("(_case_)|(roa)", colnames(data_matched), value = TRUE)
data_matched[, (cols_to_scale) := lapply(.SD, function (x) scale(x, center = TRUE, scale = TRUE)), .SDcols = cols_to_scale]

model_with_fe <- feols(
  pp_case_any_criminal ~ roa_b2 + roa_b3 + roa_b4 + log(market_share_b2) + log(total_assets_b2) + log(revenue_b2) + leverage_b2 + n_participants_b2 + family_element_b2 + zero_interest_loan_b2 + paid_zero_interest_b2 + log(market_share_b3) + log(total_assets_b3) + log(revenue_b3) + log(age) + leverage_b3 + n_participants_b3 + family_element_b3 + zero_interest_loan_b3 + paid_zero_interest_b3 | year + inn,
  cluster = ~ inn,
  panel.id = ~inn + year,
  data = data_matched,
  weights = data_matched$weights,
  mem.clean = TRUE, verbose = 3, nthreads = 16)

joint_test_interim <- extract_joint_test_all_roa(model_with_fe)
joint_test_interim$method <-  "cem"
joint_test_interim$model <- "pp_case_any_criminal"
joint_test_interim$standardized <- "yes"

cum_effect_interim <- extract_cum_effect_all_roa(model_with_fe)
cum_effect_interim$method <- "cem" 
cum_effect_interim$model <- "pp_case_any_criminal" 
cum_effect_interim$standardized <- "yes"

joint_test <- rbind(joint_test, joint_test_interim)
cum_effect <- rbind(cum_effect, cum_effect_interim)

gc()

## EBAL ====

matching_output <- weightit(pp_case_any_criminal ~ roa_b5 + revenue_b4_ln + total_assets_b4_ln + age_b4_ln + n_participants_b4_ln + family_element_b4 + okved_2dig,
                            data = llcs_panel, 
                            method = "ebal",
                            estimand = "ATT",  
                            quantile = list(
                              roa_b5 = c(0.33, 0.66),
                              revenue_b4_ln = c(0.25, 0.5, 0.75, 0.95),
                              total_assets_b4_ln = c(0.25, 0.5, 0.75, 0.95),
                              age_b4_ln = c(0.5, 0.66),
                              n_participants_b4_ln = c(0.66, 0.85)
                            ),
                            d.moments = 3,
                            maxit = 1000)

balance_table <-  bal.tab(matching_output, stats = c("m", "ks.statistics", "v"), binary = "std", continuous = "std")
matching_stats_table_interim <- extract_balance(balance_table)
matching_stats_table <- rbindlist(list(matching_stats_table, matching_stats_table_interim), fill = TRUE)

llcs_panel$w_ent <- matching_output$weights

model_with_fe <- feols(
  pp_case_any_criminal ~ roa_b2 + roa_b3 + roa_b4 + log(market_share_b2) + log(total_assets_b2) + log(revenue_b2) + leverage_b2 + n_participants_b2 + family_element_b2 + zero_interest_loan_b2 + paid_zero_interest_b2 + log(market_share_b3) + log(total_assets_b3) + log(revenue_b3) + log(age) + leverage_b3 + n_participants_b3 + family_element_b3 + zero_interest_loan_b3 + paid_zero_interest_b3 | year + inn,
  cluster = ~ inn,
  panel.id = ~inn + year,
  data = llcs_panel,
  weights = llcs_panel$w_ent,
  mem.clean = TRUE, verbose = 3, nthreads = 16)

joint_test_interim <- extract_joint_test_all_roa(model_with_fe)
joint_test_interim$method <-  "ebal"
joint_test_interim$model <- "pp_case_any_criminal"
joint_test_interim$standardized <- "no"

cum_effect_interim <- extract_cum_effect_all_roa(model_with_fe)
cum_effect_interim$method <- "ebal" 
cum_effect_interim$model <- "pp_case_any_criminal"
cum_effect_interim$standardized <- "no"

joint_test <- rbind(joint_test, joint_test_interim)
cum_effect <- rbind(cum_effect, cum_effect_interim)

cols_to_scale <- grep("(_case_)|(roa)", colnames(llcs_panel), value = TRUE)
llcs_panel[, (cols_to_scale) := lapply(.SD, function (x) scale(x, center = TRUE, scale = TRUE)), .SDcols = cols_to_scale]

model_with_fe <- feols(
  pp_case_any_criminal ~ roa_b2 + roa_b3 + roa_b4 + log(market_share_b2) + log(total_assets_b2) + log(revenue_b2) + leverage_b2 + n_participants_b2 + family_element_b2 + zero_interest_loan_b2 + paid_zero_interest_b2 + log(market_share_b3) + log(total_assets_b3) + log(revenue_b3) + log(age) + leverage_b3 + n_participants_b3 + family_element_b3 + zero_interest_loan_b3 + paid_zero_interest_b3 | year + inn,
  cluster = ~ inn,
  panel.id = ~inn + year,
  data = llcs_panel,
  weights = llcs_panel$w_ent,
  mem.clean = TRUE, verbose = 3, nthreads = 16)

joint_test_interim <- extract_joint_test_all_roa(model_with_fe)
joint_test_interim$method <-  "ebal"
joint_test_interim$model <- "pp_case_any_criminal"
joint_test_interim$standardized <- "yes"

cum_effect_interim <- extract_cum_effect_all_roa(model_with_fe)
cum_effect_interim$method <- "ebal" 
cum_effect_interim$model <- "pp_case_any_criminal"
cum_effect_interim$standardized <- "yes"

joint_test <- rbind(joint_test, joint_test_interim)
cum_effect <- rbind(cum_effect, cum_effect_interim)

## PP non-criminal ====

llcs_panel <- llcs_panel_full[roa_b5 != 0 & !is.na(roa_b5) & 
                                revenue_b4 != 0 & total_assets_b4 != 0 & !is.na(total_assets_b4) & !is.na(revenue_b4) &
                                !is.na(market_share_b4)  & !is.na(leverage_b4) & !is.na(age_b4) & !is.na(family_element_b4) &
                                sole_shareholder_only == 0 & year >= 2014 , ] # & 

## CEM ====
matching_output <- matchit(pp_case_any_noncriminal ~ roa_b5 + revenue_b4_ln + total_assets_b4_ln + age_b4_ln + n_participants_b4_ln + family_element_b4 + okved_2dig,
                           data = llcs_panel, 
                           method = "cem",
                           estimand = "ATT",  
                           cutpoints = list(
                             roa_b5 = "q5", 
                             revenue_b4_ln = "q7", 
                             total_assets_b4_ln = "q7", 
                             age_b4_ln = c(0, 2.5)),
                           verbose = TRUE)

balance_table <-  bal.tab(matching_output, stats = c("m", "ks.statistics", "v"), binary = "std", continuous = "std")
matching_stats_table_interim <- extract_balance(balance_table)
matching_stats_table <- rbindlist(list(matching_stats_table, matching_stats_table_interim), fill = TRUE)

data_matched <- match_data(matching_output, drop.unmatched = TRUE)

model_with_fe <- feols(
  pp_case_any_noncriminal ~ roa_b2 + roa_b3 + roa_b4 + log(market_share_b2) + log(total_assets_b2) + log(revenue_b2) + leverage_b2 + n_participants_b2 + family_element_b2 + zero_interest_loan_b2 + paid_zero_interest_b2 + log(market_share_b3) + log(total_assets_b3) + log(revenue_b3) + log(age) + leverage_b3 + n_participants_b3 + family_element_b3 + zero_interest_loan_b3 + paid_zero_interest_b3 | year + inn,
  cluster = ~ inn,
  panel.id = ~inn + year,
  data = data_matched,
  weights = data_matched$weights,
  mem.clean = TRUE, verbose = 3, nthreads = 16)

joint_test_interim <- extract_joint_test_all_roa(model_with_fe)
joint_test_interim$method <-  "cem"
joint_test_interim$model <- "pp_case_any_noncriminal"
joint_test_interim$standardized <- "no"

cum_effect_interim <- extract_cum_effect_all_roa(model_with_fe)
cum_effect_interim$method <- "cem" 
cum_effect_interim$model <- "pp_case_any_noncriminal"
cum_effect_interim$standardized <- "no"

joint_test <- rbind(joint_test, joint_test_interim)
cum_effect <- rbind(cum_effect, cum_effect_interim)

cols_to_scale <- grep("(_case_)|(roa)", colnames(data_matched), value = TRUE)
data_matched[, (cols_to_scale) := lapply(.SD, function (x) scale(x, center = TRUE, scale = TRUE)), .SDcols = cols_to_scale]

model_with_fe <- feols(
  pp_case_any_noncriminal ~ roa_b2 + roa_b3 + roa_b4 + log(market_share_b2) + log(total_assets_b2) + log(revenue_b2) + leverage_b2 + n_participants_b2 + family_element_b2 + zero_interest_loan_b2 + paid_zero_interest_b2 + log(market_share_b3) + log(total_assets_b3) + log(revenue_b3) + log(age) + leverage_b3 + n_participants_b3 + family_element_b3 + zero_interest_loan_b3 + paid_zero_interest_b3 | year + inn,
  cluster = ~ inn,
  panel.id = ~inn + year,
  data = data_matched,
  weights = data_matched$weights,
  mem.clean = TRUE, verbose = 3, nthreads = 16)

joint_test_interim <- extract_joint_test_all_roa(model_with_fe)
joint_test_interim$method <-  "cem"
joint_test_interim$model <- "pp_case_any_noncriminal"
joint_test_interim$standardized <- "yes"

cum_effect_interim <- extract_cum_effect_all_roa(model_with_fe)
cum_effect_interim$method <- "cem" 
cum_effect_interim$model <- "pp_case_any_noncriminal"
cum_effect_interim$standardized <- "yes"

joint_test <- rbind(joint_test, joint_test_interim)
cum_effect <- rbind(cum_effect, cum_effect_interim)

gc()

## EBAL ====

matching_output <- weightit(pp_case_any_noncriminal ~ roa_b5 + revenue_b4_ln + total_assets_b4_ln + age_b4_ln + n_participants_b4_ln + family_element_b4 + okved_2dig,
                            data = llcs_panel, 
                            method = "ebal",
                            estimand = "ATT",  
                            quantile = list(
                              roa_b5 = c(0.33, 0.66),
                              revenue_b4_ln = c(0.25, 0.5, 0.75, 0.95),
                              total_assets_b4_ln = c(0.25, 0.5, 0.75, 0.95),
                              age_b4_ln = c(0.5, 0.66),
                              n_participants_b4_ln = c(0.66, 0.85)
                            ),
                            d.moments = 3,
                            maxit = 1000)

balance_table <-  bal.tab(matching_output, stats = c("m", "ks.statistics", "v"), binary = "std", continuous = "std")
matching_stats_table_interim <- extract_balance(balance_table)
matching_stats_table <- rbindlist(list(matching_stats_table, matching_stats_table_interim), fill = TRUE)

llcs_panel$w_ent <- matching_output$weights

model_with_fe <- feols(
  pp_case_any_noncriminal ~ roa_b2 + roa_b3 + roa_b4 + log(market_share_b2) + log(total_assets_b2) + log(revenue_b2) + leverage_b2 + n_participants_b2 + family_element_b2 + zero_interest_loan_b2 + paid_zero_interest_b2 + log(market_share_b3) + log(total_assets_b3) + log(revenue_b3) + log(age) + leverage_b3 + n_participants_b3 + family_element_b3 + zero_interest_loan_b3 + paid_zero_interest_b3 | year + inn,
  cluster = ~ inn,
  panel.id = ~inn + year,
  data = llcs_panel,
  weights = llcs_panel$w_ent,
  mem.clean = TRUE, verbose = 3, nthreads = 16)

joint_test_interim <- extract_joint_test_all_roa(model_with_fe)
joint_test_interim$method <-  "ebal"
joint_test_interim$model <- "pp_case_any_noncriminal"
joint_test_interim$standardized <- "no"

cum_effect_interim <- extract_cum_effect_all_roa(model_with_fe)
cum_effect_interim$method <- "ebal" 
cum_effect_interim$model <- "pp_case_any_noncriminal"
cum_effect_interim$standardized <- "no"

joint_test <- rbind(joint_test, joint_test_interim)
cum_effect <- rbind(cum_effect, cum_effect_interim)

cols_to_scale <- grep("(_case_)|(roa)", colnames(llcs_panel), value = TRUE)
llcs_panel[, (cols_to_scale) := lapply(.SD, function (x) scale(x, center = TRUE, scale = TRUE)), .SDcols = cols_to_scale]

model_with_fe <- feols(
  pp_case_any_noncriminal ~ roa_b2 + roa_b3 + roa_b4 + log(market_share_b2) + log(total_assets_b2) + log(revenue_b2) + leverage_b2 + n_participants_b2 + family_element_b2 + zero_interest_loan_b2 + paid_zero_interest_b2 + log(market_share_b3) + log(total_assets_b3) + log(revenue_b3) + log(age) + leverage_b3 + n_participants_b3 + family_element_b3 + zero_interest_loan_b3 + paid_zero_interest_b3 | year + inn,
  cluster = ~ inn,
  panel.id = ~inn + year,
  data = llcs_panel,
  weights = llcs_panel$w_ent,
  mem.clean = TRUE, verbose = 3, nthreads = 16)

joint_test_interim <- extract_joint_test_all_roa(model_with_fe)
joint_test_interim$method <-  "ebal"
joint_test_interim$model <- "pp_case_any_noncriminal"
joint_test_interim$standardized <- "yes"

cum_effect_interim <- extract_cum_effect_all_roa(model_with_fe)
cum_effect_interim$method <- "ebal" 
cum_effect_interim$model <- "pp_case_any_noncriminal"
cum_effect_interim$standardized <- "yes"

joint_test <- rbind(joint_test, joint_test_interim)
cum_effect <- rbind(cum_effect, cum_effect_interim)

fwrite(joint_test, paste0(path_d, "plots_tables/estimation_results/wald_tests_all_abs_roa_with_weights.csv"))
fwrite(cum_effect, paste0(path_d, "plots_tables/estimation_results/cum_est_all_abs_roa_with_weights.csv"))

fwrite(matching_stats_table, paste0(path_d, "plots_tables/matching_results/matching_stats_raw_all_abs_roa.csv"))
