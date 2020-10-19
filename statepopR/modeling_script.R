# Author:     Hamidreza Zoraghein
# Date:       2019-01-03
# Purpose:    Project populations by states for the U.S with sensitivity analysis added. Sensitivity analysis
#             currently takes into account national-level SSP projectsions of fertility, mortality and international
#             migration. The U.S. Domestic migration is bilateral. By sensitivty analysis, we mean projection based on
#             different scenarios.


###############
#*** Paths ***#
###############
path <- "C:\\Users\\deci859\\PycharmProjects\\IM3\\statepop\\statepop_R\\inputs"

# Path to results folder
resultsPath <- file.path(path, "No_Mig")

# Path to state-level inputs folder
inputsPath  <- file.path(path, "State_Inputs")

# Path to the package
mspackage   <- file.path(path, "Scripts", "multistate_0.1.0.tar.gz")

# UN standard life table e0=30; used for linear interpolation of lx values
datMortS30S  <- file.path(path, "AllRegions_mortality_UNe030.csv")

# UN standard life table e0=100; used for linear interpolation of lx values
datMortS100S <- file.path(path, "AllRegions_mortality_UNe0100.csv")


#######################
#*** Load packages ***#
#######################
# Various packages
if ("foreign" %in% installed.packages())
{
  library(foreign)
} else {
  install.packages("foreign")
  library(foreign)
}

if ("Matrix" %in% installed.packages())
{
  library(Matrix)
} else {
  install.packages("Matrix")
  library(Matrix)
}

if ("ggplot2" %in% installed.packages())
{
  library(ggplot2)
} else {
  install.packages("ggplot2")
  library(ggplot2)
}

if ("readxl" %in% installed.packages())
{
  library(readxl)
} else {
  install.packages("readxl")
  library(readxl)
}

if ("multistate" %in% installed.packages())
{
  library(multistate)
} else {
  install.packages("multistate")
  library(multistate)
}


#############################
#*** Set general options ***#
#############################
options("scipen"=100, "digits"=4) # to force R not to use scientific notations
#options(warn=2)  # treat warnings as errors


###################################
#*** Declare general variables ***#
###################################



#* Specify regions
regUAll <- list.dirs(path = "./inputs/State_Inputs", recursive = FALSE, full.names = FALSE)

#* Specify scenario
scenUAll     <- c("Constant_rate", "SSP2", "SSP3", "SSP5")
cur.scenario <- "Constant_rate"

# Specify the domestic migration factor
# If scenario is not "Constant_rate" (for fertility, mortality and international migration), this factor will become dynamic later
scen.factor <- 0 # 1 for regular, 0 for no domestic migration, 0.5 for half scenario and 2 for double scenario

# Sepecify if international migration is applied
int.mig <- 0 # 1 applied 0 not applied

#* Should details for projection model adjustment be printed?
vis <- F # TRUE (print details); FALSE (don't print details)

#* Should the Brass Relational Model be used or a simple scaling approach to compute future fertility schedules
useBrassf <- T # TRUE (use Brass); FALSE (use scaling)

#* Generate Directories
if(!file.exists(resultsPath)) {dir.create(resultsPath)} # output directory

datMortS100  <- read.csv(datMortS100S, check.names = F, stringsAsFactors = F)
datMortS30   <- read.csv(datMortS30S, check.names = F, stringsAsFactors = F)


num.ages         <- 101 #From 0 to 100
ini.all.base.pop <- as.data.frame(matrix(0, nrow = 4 * num.ages, ncol = length(regUAll)))
upd.all.base.pop <- as.data.frame(matrix(0, nrow = 4 * num.ages, ncol = length(regUAll)))
colnames(ini.all.base.pop) <- regUAll
colnames(upd.all.base.pop) <- regUAll


#* Define starting year and end year
yearStart <- 2010                # Base year
yearEnd   <- 2100                # Last year for which population is projected
steps     <- yearEnd - yearStart # each projection step can be thought of as the resulting year (e.g., projection step 1 projects values for year 1 using input for year 0)


# These dataframes will hold population projections and net/in/out state-level migrations for all states and years
tot.projection    <- NULL
tot.state.net.mig <- NULL
tot.state.in.mig  <- NULL
tot.state.out.mig <- NULL

# This dataframe holds population values before applying domestic migration. It is necessary to disaggregate
# domestic migration to/from each state across all other states
tot.pop.no.dom    <- NULL

# Initialize a dataframe for holding international migration for all years and states
tot.int.mig           <- data.frame(matrix(NA, nrow = 4*num.ages*(steps+1), ncol = length(regUAll)))
colnames(tot.int.mig) <- regUAll

#####################################################
#*** Derive national-level changes based on SSPs ***#
#####################################################

# Read the scenario table if it is not constant_rate
# cur.scenario could be SSP2, SSP3 and SSP5
if (cur.scenario != "Constant_rate"){

  scenario.csv   <- file.path(resultsPath, paste0(cur.scenario, "_scenario.csv"))
  scenario.table <- read.csv(scenario.csv, stringsAsFactors = F, check.names = F)
}

##################################
#*** Calculate functions here ***#
##################################




