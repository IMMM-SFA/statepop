import pkg_resources
import os
import numpy as np
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
#for results in results_path:
#    os.makedirs(results, exist_ok=True)

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
def get_state_dirs(root_dir, delimeter='-', target_index=0):
    state_dirs = []
    for i in os.listdir(root_dir):
        split_i = i.split("-")[0]

        try:
            x = int(split_i)
            state_dirs.append(os.path.join(root_dir, i))
        except ValueError:
            pass
    return state_dirs

def get_contents_file_list(state_dirs, target_file):

    constant_rate_files = []
    for i in state_dirs:

        contents = os.listdir(i)
        if target_file in contents:
            constant_rate_files.append(os.path.join(i, target_file))

        else:
            print(f"No contents file in directory {i}")

    return constant_rate_files

def make_dataframes(file_list):
    dfs = [pd.read_csv(files) for files in file_list]
    return dfs

#* Generate paths
state_dir_list = get_state_dirs(state_inputs) # Input data directory

#* Scenario data (The Constant_rate scenario.)
constant_rates = get_contents_file_list(state_dir_list, target_file='Constant_rate.csv')
constant_rate_dfs = make_dataframes(constant_rates)

#* Base Population data
base_population = get_contents_file_list(state_dir_list, target_file='basePop.csv')
base_population_dfs = make_dataframes(base_population)

# Variables
base_population_age = "age"                                 # Age categories (numeric, starting with 0)
base_population_rural_female = "rural_female"               # Rural female population
base_population_urban_female = "urban_female"               # Urban female population
base_population_rural_male = "rural_male"                   # Rural male population
base_population_urban_male = "urban_male"                   # Urban male population

#* International migration data
# Note: The international migration rates input data needs to be in relative rates (summing up to 1)
international_migration_rates = get_contents_file_list(state_dir_list, target_file='intMig.csv')
internation_migration_rate_dfs = make_dataframes(international_migration_rates)

# Variables
net_migration_age = "age"                       # Age variable
net_migration_female = "net_female"             # Proportion of net migration for each age group for females
net_migration_male = "net_male"                 # Proportion of net migration for each age group for males

# Annual total net international migration counts for 5-year periods
female_net_internation_migration = constant_rate_dfs[0].nim_F.values() if #in range of years specified?
male_net_international_migration =

nMigFt < - as.numeric(scenario[scenario$year % in % c(yearStart: yearEnd), "nim_F"])  # female net international migrants
nMigMt < - as.numeric(scenario[scenario$year % in % c(yearStart: yearEnd), "nim_M"])  # male net international migrants