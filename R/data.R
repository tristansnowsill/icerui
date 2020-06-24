#' BCEA object for cost-effectiveness of smoking cessation interventions
#'
#' An object of class \code{bcea} derived from the dataset \link[BCEA:Smoking]{Smoking}
#' @source \url{http://www.statistica.it/gianluca/software/bcea/}
"ex_smoking"

#' Simple example BCEA object
#'
#' Artificial BCEA object with two options generated from multivariate normal
#' distribution using `MASS::mvrnorm`
#'
#' The dataset is constructed such that the mean costs are 5000 and 10000
#' and mean effects are 2.5 and 2.7. Incremental costs and effects have
#' standard deviations 1000 * sqrt(3) and 0.2 and are negatively correlated
#' (\eqn{\rho = -0.2 \sqrt 3} = 0.3464...).
"ex_simple"

#' heemod PSA object for HIV therapy
#'
#' This is the result of the \code{heemod::run_psa} at the end of the vignette
#' e-probabilistic
#'
#' @source \url{https://cran.r-project.org/web/packages/heemod/vignettes/e_probabilistic.html}
"ex_hiv"

#' heemod PSA object for immunosuppression in renal replacement therapy
#'
#' This is a hypothetical model of four immunosuppressive therapies: csa, tac,
#' sir and bel. Please note that this is for illustrative purposes and is not
#' in any way a realistic economic evaluation of immunosuppressive therapies
#' in renal replacement therapy. See \url{https://doi.org/10.3310/hta20620} if
#' you want to see actual economic evaluations of immunosuppression in renal
#' replacement therapy.
"ex_renal"

# Code for ex_renal ----

# library(heemod)
#
# params <- define_parameters(
#   p_fail_base = 0.16,
#   p_rtx       = 0.05,
#   p_die_f     = 0.05,
#   p_die_d     = 0.10,
#
#   hr_fail_tac = 0.9,
#   hr_fail_sir = 0.92,
#   hr_fail_bel = 0.7,
#
#   p_fail      = dispatch_strategy(csa = p_fail_base,
#                                   tac = 1 - (1 - p_fail_base) ^ hr_fail_tac,
#                                   sir = 1 - (1 - p_fail_base) ^ hr_fail_sir,
#                                   bel = 1 - (1 - p_fail_base) ^ hr_fail_bel),
#
#   c_csa       = 500,
#   c_tac       = 800,
#   c_sir       = 1200,
#   c_bel       = 10000,
#
#   c_dial      = 5000,
#   c_rtx       = 15000,
#
#   u_fg        = 0.8,
#   u_dial      = 0.6
# )
#
# trans_mat <- define_transition(
#   C,      p_fail, p_die_f,
#   p_rtx,  C,      p_die_d,
#   0,      0,      1,
#
#   state_names = c("Functioning graft", "Dialysis", "Dead")
# )
#
# state_fg <- define_state(
#   cost = dplyr::if_else(state_time < 1.5 & markov_cycle > 1,
#                         c_rtx,
#                         dispatch_strategy(csa = c_csa,
#                                           tac = c_tac,
#                                           sir = c_sir,
#                                           bel = c_bel)),
#   qaly = u_fg
# )
#
# state_dial <- define_state(
#   cost = c_dial,
#   qaly = u_dial
# )
#
# state_dead <- define_state(
#   cost = 0,
#   qaly = 0
# )
#
# strat_any <- define_strategy(
#   `Functioning graft` = state_fg,
#   `Dialysis`          = state_dial,
#   `Dead`              = state_dead,
#   transition          = trans_mat
# )
#
# model <- run_model(
#   csa = strat_any,
#   tac = strat_any,
#   sir = strat_any,
#   bel = strat_any,
#
#   init = c(1, 0, 0),
#
#   parameters = params,
#   cycles = 40,
#   state_time_limit = 1,
#
#   cost = cost,
#   effect = qaly
# )
#
# psa <- define_psa(
#   p_fail_base ~ beta(  16,  84),
#   p_rtx       ~ beta(   5,  95),
#   p_die_f     ~ beta(  50, 950),
#   p_die_d     ~ beta( 100, 900),
#
#   hr_fail_tac ~ gamma(0.90, 0.02),
#   hr_fail_sir ~ gamma(0.92, 0.08),
#   hr_fail_bel ~ gamma(0.70, 0.15),
#
#   c_csa       ~ lognormal(500, 100),
#   c_tac       ~ lognormal(800, 100),
#   c_sir       ~ lognormal(1200, 100),
#   c_bel       ~ lognormal(10000, 100),
#
#   c_dial      ~ lognormal(5000, 100),
#   c_rtx       ~ lognormal(15000, 1000),
#
#   u_fg        ~ beta(80, 10),
#   u_dial      ~ beta(60, 40)
# )
#
# ex_renal <- run_psa(model, psa, 1000)

# ENDS ----
