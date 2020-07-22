import pkg_resources
import os
import pandas as pd
import matplotlib
import statepop.multistate as multi

# Path to results folder (ask Chris how to handle)
results_path = pkg_resources.resource_filename('statepop', 'data/inputs/No_Mig')

# Path to state-level inputs folders
state_inputs = pkg_resources.resource_filename('statepop', 'data/State_Inputs')

# Path to the package
# potentially have an absolute import statement above ? Need to recreate package.

# UN standard life table e0=30; used for linear interpolation of lx values
mortality_30 = pkg_resources.resource_filename('statepop', 'data/Inputs/AllRegions_mortality_UNe030.csv')

# UN standard life table e0=100; used for linear interpolation of lx values
mortality_100 = pkg_resources.resource_filename('statepop', 'data/Inputs/AllRegions_mortality_UNe0100.csv')

# Specify regions
regions = os.listdir(state_inputs)

# Specify scenario
scenarios = ("Constant_rate", "SSP2", "SSP3", "SSP5")

""""""""""""""""""""""""""""""""""
# Not sure how to directly incorporate this, come back to it later

# Specify the domestic migration factor
# If scenario is not "Constant_rate" (for fertility, mortality and international migration), this factor will become dynamic later
scen.factor <- 0 # 1 for regular, 0 for no domestic migration, 0.5 for half scenario and 2 for double scenario

# Sepecify if international migration is applied
int.mig <- 0 # 1 applied 0 not applied

#* Should details for projection model adjustment be printed?
vis <- F # TRUE (print details); FALSE (don't print details)

#* Should the Brass Relational Model be used or a simple scaling approach to compute future fertility schedules
useBrassf <- T # TRUE (use Brass); FALSE (use scaling)

"""""""""""""""""""""""""""""""""
#* Generate Directories
for results in results_path:
    os.makedirs(results, exist_ok=True)

mort_30 = pd.read_csv(mortality_30)
mort_100 = pd.read_csv(mortality_100)

#From 0 to 100
num_ages = [i for i in range(101)]

initial_all_base_population = pd.DataFrame(index = 4*num_ages, columns=regions)
updated_all_base_population = pd.DataFrame(index = 4*num_ages, columns=regions)

#* Define starting year and end year
year_start = 2010
year_through = 2100
steps = year_through - year_start

# These dataframes will hold population projections and net/in/out state-level migrations for all states and years
total_population_projection = pd.DataFrame()
total_state_net_migration = pd.DataFrame()
total_state_in_migration = pd.DataFrame()
total_state_out_migration = pd.DataFrame()

# This dataframe holds population values before applying domestic migration. It is necessary to disaggregate
# domestic migration to/from each state across all other states
total_population_no_domestic_migration = pd.DataFrame()

# Initialize a dataframe for holding international migration for all years and states
total_international_migration = pd.DataFrame(index = 4*num_ages*(steps+1), columns=regions)


#############################################
#*** Generate baseline population matrix ***#
#############################################

# Loop over regions to update their base year population with international and state-level migrations
for states in regions:
    # * Generate paths
    path_in = state_inputs[states]
