#' Function to calculate the total in migration numbers for all states
#'
#' @param input.path input folder containing the migration rates
#' @param pop.dataframe population data frame
#' @param scen.factor scale applied to migration rates based on the scenario
#' @return data frame containing in-migration values for all states
#' @export
#'

import pandas as pd
from statepop.utils import single_state_files


def in_domestic_migration_calc(state_abbr, updated_all_base_pop, domestic_migration_factor=0):
  """Requires all states"""

  # Read the csv file holding in migration rates to the current state
  in_mig_df = single_state_files(state_abbr, '_in_mig.csv')

  # Retrieve population of all states that contributed migration to the current one
  from_states_pop = in_mig_df.where(in_mig_df['from'] != state_abbr)
  from_states_pop = from_states_pop.groupby('from')['rate'].sum()
  from_states_pop_df = pd.DataFrame(data=from_states_pop)
  from_states_pop_df = from_states_pop_df.T

  uabp = updated_all_base_pop

  # Create a dataframe to hold in migration rates of all states contributing to the current one
  # Populate the in migration dataframe with the in migration rates of all contributing states

  return (uabp)


#def out_domestic_migration_calc(state_abbr, updated_all_base_pop, domestic_migration_factor):