
args <- commandArgs(trailingOnly = TRUE)

if (length(args) != 4) {
  stop("Four arguments required: <experiment: CI|est> <setting: n|snr|lopsided> <interference: additive|stratified|graph> <ADAPT: 0|1>")
}

experiment   <- args[1]
setting      <- args[2]
interference <- args[3]
adapt_flag   <- args[4]

if (!(experiment %in% c("CI", "est"))) {
  stop("First argument must be one of: CI, est")
}
if (!(setting %in% c("n", "snr", "lopsided"))) {
  stop("Second argument must be one of: n, snr, lopsided")
}
if (!(interference %in% c("additive", "stratified", "graph"))) {
  stop("Third argument must be one of: additive, stratified, graph")
}
if (!(adapt_flag %in% c("0", "1"))) {
  stop("Fourth argument must be one of: 0, 1")
}

# ---- Dependencies ----------------------------------------------------------
require(quadprog)
require(binaryLogic)
library(corpcor)

source('~/source.R')

# ---- Parameters common to every setting ------------------------------------
rho_range <- NULL         

p_cov          <- 4
sd_scalar      <- 1        
rho_scalar     <- 0.3
k_pair         <- c(10, 15)  

indiv                 <- FALSE
imbalance             <- 0

# Toggle the chosen interference experiment TRUE (others FALSE)
additive_experiment   <- (interference == "additive")
stratified_experiment <- (interference == "stratified")
graph_experiment      <- (interference == "graph")

# Toggle ADAPT from the 0/1 flag
ADAPT                 <- (adapt_flag == "1")
est_bal               <- TRUE

# ---- Setting-specific parameters (nested if-else) --------------------------
if (experiment == "CI") {

  ci_experiment <- TRUE

  if (setting == "n") {
    par_number        <- 4
    n_max             <- 5000
    n_range           <- c(100, 300, 500, 700)
    c_range           <- rep(0.2, par_number)
    gamma_scale_range <- rep(sqrt(0.5), par_number)

  } else if (setting == "snr") {
    par_number        <- 5
    n_range           <- rep(200, par_number)
    c_range           <- rep(0.2, par_number)
    gamma_scale_range <- c(sqrt(0.2), sqrt(0.5), sqrt(1), sqrt(2), sqrt(5))

  } else {  # lopsided
    par_number        <- 5
    n_range           <- rep(300, par_number)
    c_range           <- c(0.20, 1.65, 3.10, 4.55, 6.00)
    gamma_scale_range <- rep(sqrt(0.5), par_number)
  }

} else {  # experiment == "est"

  ci_experiment <- FALSE

  if (setting == "n") {
    par_number        <- 4
    n_range           <- c(50, 75, 100, 125)
    c_range           <- rep(0.2, par_number)
    gamma_scale_range <- rep(sqrt(0.5), par_number)

  } else if (setting == "snr") {
    par_number        <- 5
    n_range           <- rep(80, par_number)
    c_range           <- rep(0.2, par_number)
    gamma_scale_range <- c(sqrt(0.2), sqrt(0.5), sqrt(1), sqrt(2), sqrt(5))

  } else {  # lopsided
    par_number        <- 5
    n_range           <- rep(80, par_number)
    c_range           <- c(0.20, 1.65, 3.10, 4.55, 6.00)
    gamma_scale_range <- rep(sqrt(0.5), par_number)
  }
}

# ---- Ranges that depend on par_number --------------------------------------
sd_range     <- rep(sd_scalar, par_number)      
rho_range    <- rep(rho_scalar, par_number)
k_range      <- rep(list(k_pair), par_number)   
p_cov_range  <- rep(p_cov, par_number)         

# ---- Run -------------------------------------------------------------------
source('~/execute.R')
