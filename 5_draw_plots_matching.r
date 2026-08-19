library(data.table)
library(ggplot2)
library(stringr); library(stringi)
library(ggpp)
library(egg)
options(scipen = 999)


# Declare working directory beforehand in an environment variable
# CLASSIFICATION_CORP_CASES = "path_to_your_folder"
# with the aid of usethis::edit_r_environ()
# Restart R session for the changes to take effect
path <- Sys.getenv("SHAREHOLDERS_CRIMES_PATH")
setwd(path)

# Load the data ====
matching_estimates_both_predictors <- fread(paste0("plots_tables/matching_results/matching_estimates.csv"))

matching_estimates_both_predictors[, c("indv", "n_obs") := NULL]

matching_estimates_both_predictors$dv <- forcats::fct_relevel(matching_estimates_both_predictors$dv, 
                                                "pp_case_any", "pp_case_satisfied", "pp_case_any_key", "pp_case_satisfied_key")

matching_estimates_both_predictors <- matching_estimates_both_predictors[firm_fe == 1, ]

matching_estimates_both_predictors <- rbindlist(list(
  matching_estimates_both_predictors[, .(estimate = roa_b1_slope, se = roa_b1_se, model = dv, matching_method, lag = "-1", strain = "absolute"), ],
  matching_estimates_both_predictors[, .(estimate = roa_b2_slope, se = roa_b2_se, model = dv, matching_method, lag = "-2", strain = "absolute"), ],
#  matching_estimates_both_predictors[, .(estimate = roa_hist_b1_slope, se = roa_hist_b1_se, model = dv, matching_method, lag = "-1", strain = "hist"), ],
  matching_estimates_both_predictors[, .(estimate = roa_hist_b2_slope, se = roa_hist_b2_se, model = dv, matching_method, lag = "-2", strain = "hist"), ]
))

# CEM ====
shareholders_lpmfe_both_predictors_matching_cem <- ggplot(matching_estimates_both_predictors[matching_method == "cem", ], aes(x = lag, y = estimate, group = strain, shape = strain)) + # 
  geom_point(position = position_dodge(width = 0.5), size = 4) +
  geom_errorbar(aes(ymin = estimate - (1.96 * se), ymax = estimate + (1.96 * se)), 
                linewidth = 0.5, width = 0.3, 
                position = position_dodge(width = 0.5)) +
  theme_bw() +
  theme(plot.title = element_blank(), # element_text(face = "bold", hjust = 0.5, size = 12)
        plot.subtitle = element_text(hjust = 0.5),
        axis.title.x = element_text(size = 10),
        axis.title.y = element_text(size = 10),
        panel.grid.major.x = element_blank(), 
        panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_blank(), 
        panel.grid.minor.y = element_blank(),
        text = element_text(family = "Times New Roman"),
        # legend.key.width = unit(3, "cm"),
        # legend.position = "bottom",
        # legend.text = element_text(size = 14),
        # legend.title = element_text(size = 14),
        legend.box.spacing = unit(0, "cm"),
        strip.text = element_text(face = "bold", size = 10),
        strip.background = element_blank()) +
  scale_y_continuous(breaks = c(-0.05, 0, 0.05)) +
  geom_hline(yintercept = 0) +
  facet_wrap(~ model, nrow = 2, ncol = 4, 
             labeller = labeller(model = c(pp_case_any = "Any claim", pp_case_satisfied = "Satisfied claim", pp_case_any_key = "Any serious claim", pp_case_satisfied_key = "Satisfied serious claim"))) +
  scale_shape_manual(values = c("absolute" = 1,  
                                "hist" = 15),     
                     labels = c("absolute" = "Absolute", 
                                "hist" = "Historical")) +
  labs(
    x = "", # Lag of financial variables relative to suit filing
    y = "", 
    title = "How absolute and relative firm performance metrics are associated with\n shareholders' crimes", 
    subtitle = "With weights from CEM",
    shape = "Firm performance (ROA)") +
  guides(shape = "none")

# EBAL ====
shareholders_lpmfe_both_predictors_matching_ebal <- ggplot(matching_estimates_both_predictors[matching_method == "ebal", ], aes(x = lag, y = estimate, group = strain, shape = strain)) + # 
    geom_point(position = position_dodge(width = 0.5), size = 4) +
    geom_errorbar(aes(ymin = estimate - (1.96 * se), ymax = estimate + (1.96 * se)), 
                  linewidth = 0.5, width = 0.3, 
                  position = position_dodge(width = 0.5)) +
    theme_bw() +
    theme(plot.title = element_blank(), # element_text(face = "bold", hjust = 0.5, size = 14)
          plot.subtitle = element_text(hjust = 0.5),
          axis.title.x = element_text(size = 10),
          axis.title.y = element_text(size = 10),
          panel.grid.major.x = element_blank(), 
          panel.grid.minor.x = element_blank(),
          panel.grid.major.y = element_blank(), 
          panel.grid.minor.y = element_blank(),
          text = element_text(family = "Times New Roman"),
          legend.key.width = unit(3, "cm"),
          legend.position = "bottom",
          legend.text = element_text(size = 12),
          legend.title = element_text(size = 12),
          strip.text = element_text(face = "bold", size = 10),
          strip.background = element_blank()) +
    scale_y_continuous(breaks = c(-0.01, 0, 0.01)) +
    geom_hline(yintercept = 0) +
    facet_wrap(~ model, nrow = 2, ncol = 4, 
               labeller = labeller(model = c(pp_case_any = "", pp_case_satisfied = "", pp_case_any_key = "", pp_case_satisfied_key = ""))) +
    scale_shape_manual(values = c("absolute" = 1,  
                                  "hist" = 15),     
                       labels = c("absolute" = "Absolute", 
                                  "hist" = "Historical")) +
  labs(
    x = "", 
    y = "", 
    title = "", # How absolute and relative firm performance metrics are associated with\n shareholders' crimes
    subtitle = "With weights from EBAL",
    shape = "Firm performance (ROA)")

# Both ====

matching_plots <- ggarrange(shareholders_lpmfe_both_predictors_matching_cem, shareholders_lpmfe_both_predictors_matching_ebal, labels = c("", ""), nrow = 2)

matching_plots <- ggpubr::annotate_figure(matching_plots,
                                  left = ggpubr::text_grob("Standardized estimates", 
                                                   rot = 90, 
                                                   vjust = 0.5, 
                                                   hjust = 0.5,
                                                   family = "Times New Roman",
                                                   size = 12),
                                  bottom = ggpubr::text_grob("Lag of financial variables relative to suit filing",
                                                     family = "Times New Roman",
                                                     size = 12))

ggsave(matching_plots, file = paste0("plots_tables/plots/matching_plots.png"), device = "png", height = 5, width = 9)
