pkgname <- "MacalesterCausalSimulation"
source(file.path(R.home("share"), "R", "examples-header.R"))
options(warn = 1)
base::assign(".ExTimings", "MacalesterCausalSimulation-Ex.timings", pos = 'CheckExEnv')
base::cat("name\tuser\tsystem\telapsed\n", file=base::get(".ExTimings", pos = 'CheckExEnv'))
base::assign(".format_ptime",
function(x) {
  if(!is.na(x[4L])) x[1L] <- x[1L] + x[4L]
  if(!is.na(x[5L])) x[2L] <- x[2L] + x[5L]
  options(OutDec = '.')
  format(x[1L:3L], digits = 7L)
},
pos = 'CheckExEnv')

### * </HEADER>
library('MacalesterCausalSimulation')

base::assign(".oldSearch", base::search(), pos = 'CheckExEnv')
base::assign(".old_wd", base::getwd(), pos = 'CheckExEnv')
cleanEx()
nameEx("ipw_viz_bar")
### * ipw_viz_bar

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: ipw_viz_bar
### Title: Creating bar charts to check if the IPW weighting actually
###   removed the relationship between treatment and the intended variable
### Aliases: ipw_viz_bar

### ** Examples

ipw_viz_bar(sim_data, Z, A)
ipw_viz_bar(sim_data, Z, A, ipw_complex)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("ipw_viz_bar", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("ipw_viz_box")
### * ipw_viz_box

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: ipw_viz_box
### Title: Creating a boxplot to compare effectiveness of propensity score
###   weighting
### Aliases: ipw_viz_box

### ** Examples

ipw_viz_box(tenure, factor(gncs), full_ratio)
ipw_viz_box(tenure, factor(gncs), full_ratio, ipw)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("ipw_viz_box", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("ipw_viz_point")
### * ipw_viz_point

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: ipw_viz_point
### Title: Creating scatterplots to determine model specification for
###   propensity scoring (linear or non-linear) - the blue smooth reflects
###   observed data trends, and the red smooth shows the predictions from a
###   logistic regression model with a specific model formula
### Aliases: ipw_viz_point

### ** Examples

ipw_viz_point(tenure, revenue, gncs)
ipw_viz_point(tenure, full_ratio, gncs)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("ipw_viz_point", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("odds_to_prob")
### * odds_to_prob

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: odds_to_prob
### Title: Changing odds to a probability
### Aliases: odds_to_prob

### ** Examples

prob = odds_to_prob(log_odds_A)
odds_to_prob(-0.6)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("odds_to_prob", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("sim_from_str")
### * sim_from_str

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: sim_from_str
### Title: Simulates data based on the structural equations provided by the
###   user
### Aliases: sim_from_str

### ** Examples

# example code
sim_data <- sim_from_str(100, "z ~ normal (quant), x ~ binomial (bin), y ~ x + z (linear)")
sim_from_str(1000, "a ~ binomial (bin), b ~ 5a + 30 (bin), c~ a+b+20 (quant), d ~ a + b + c (lin)")



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("sim_from_str", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("simple_chain_simulation")
### * simple_chain_simulation

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: simple_chain_simulation
### Title: Simulates the dependency structure of a basic chain, with 3
###   variables X, Y, Z, where X <e2><86><92> Y <e2><86><92> Z
### Aliases: simple_chain_simulation

### ** Examples

# example code
# Results in a chain simulation with 10000 simulated observations of X, Y, Z, with X and Y being numeric and Z being binary.
simple_chain_simulation(10000, x_numeric = TRUE, y_numeric = TRUE, z_numeric = FALSE)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("simple_chain_simulation", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("simple_collider_simulation")
### * simple_collider_simulation

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: simple_collider_simulation
### Title: Simulates the dependency structure of a basic collider, with 3
###   variables X, Y, Z, where X <e2><86><92> Y <e2><86><90> Z
### Aliases: simple_collider_simulation

### ** Examples

# example code
# Results in a collider simulation with 10000 simulated observations of X, Y, Z, with X and Y being numeric and Z being binary.
set.seed(451)
simple_collider_simulation(10000, x_numeric = TRUE, y_numeric = TRUE, z_numeric = FALSE)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("simple_collider_simulation", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("simple_fork_simulation")
### * simple_fork_simulation

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: simple_fork_simulation
### Title: Simulates the dependency structure of a basic fork, with 3
###   variables X, Y, Z, where X <e2><86><90> Y <e2><86><92> Z
### Aliases: simple_fork_simulation

### ** Examples

# example code
# Results in a fork simulation with 10000 simulated observations of X, Y, Z, with X and Y being numeric and Z being binary.
set.seed(451)
simple_fork_simulation(10000, x_numeric = TRUE, y_numeric = TRUE, z_numeric = FALSE)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("simple_fork_simulation", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("simulate_many")
### * simulate_many

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: simulate_many
### Title: Replicates n number of times a chosen function out of 3
###   functions: simple_chain_simulation(n, x_numeric, y_numeric,
###   z_numeric), simple_fork_simulation(n, x_numeric, y_numeric,
###   z_numeric), simple_collider_simulation(n, x_numeric, y_numeric,
###   z_numeric).
### Aliases: simulate_many

### ** Examples

# example code
# A replication of the simple_fork_simulation(10000, x_numeric = TRUE, y_numeric = FALSE, z_numeric = TRUE) for 100 times
set.seed(451)
simulate_many(simple_fork_simulation(10000, x_numeric = TRUE, y_numeric = FALSE, z_numeric = TRUE), n_rep_time = 100)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("simulate_many", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("weighting")
### * weighting

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: weighting
### Title: Creating the weights used for inverse probability weighting or
###   propensity score weighting
### Aliases: weighting

### ** Examples

weighting(sim_data, ps_mod_simple, A)
weighting(tenure, ps_mod, tenure$gncs)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("weighting", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
### * <FOOTER>
###
cleanEx()
options(digits = 7L)
base::cat("Time elapsed: ", proc.time() - base::get("ptime", pos = 'CheckExEnv'),"\n")
grDevices::dev.off()
###
### Local variables: ***
### mode: outline-minor ***
### outline-regexp: "\\(> \\)?### [*]+" ***
### End: ***
quit('no')
