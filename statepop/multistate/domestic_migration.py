import os
import pandas as pd
from statepop.utils import alphabetize_states, single_state_files


def in_domestic_migration(updated_all_base_pop_df, scen_factor=0):
  """Calculate the total in migration numbers for all states.

  :param updated_all_base_pop_df:         A data frame containing population numbers per state
  :type updated_all_base_pop_df:          data frame

  :param scen_factor:                     (Optional) Scale applied to migration rates based on the scenario.
                                          Default is 0, no domestic migration.
  :type scen_factor:                      int

  :return:                                A data frame with total in migration population

  """

  print("Calculating domestic migration rates into states.")
  tot_in_mig_pop = pd.DataFrame()

  for states in alphabetize_states():

    # get target state id_abbrev
    target_state = states.split('-')[1]

    # read in the contributing states in migration rates
    current_in_mig = single_state_files(target_state, '_in_mig.csv')

    # get population data for every state except the target state
    from_states = [i for i in updated_all_base_pop_df.columns if states != i]
    current_from_pop = updated_all_base_pop_df[from_states].copy()

    # set data frame to hold contributing states population
    total_in_mig_pop = pd.DataFrame()

    for state in from_states:
      state_abbrev = state.split('-')[1]

      # create a copy of the in migration data frame for the from state
      state_in_mig_urban = current_in_mig.loc[current_in_mig['from'] == state_abbrev].copy()

      # create copy for rural and zero out rate
      state_in_mig_rural = state_in_mig_urban.copy()
      state_in_mig_rural['rate'] *= 0

      # add sorting value for rural
      state_in_mig_rural['setting'] = 1

      # add sorting value for urban
      state_in_mig_urban['setting'] = 0

      # combine urban and rural data frames
      state_in_mig = pd.concat([state_in_mig_urban, state_in_mig_rural])

      # sort data frame by gender and age
      state_in_mig.sort_values(by=['gender', 'age', 'setting'], inplace=True)

      # add to all states data frame
      total_in_mig_pop[state] = state_in_mig['rate'].values

    # multiply to get the in migration numbers from each contributing state
    total_in_net_pop = (total_in_mig_pop * scen_factor) * current_from_pop

    # sum the total in migration numbers across all contributing states with one value per age group
    tot_in_mig_pop[target_state] = total_in_net_pop.sum(axis=1)

  return tot_in_mig_pop


def out_domestic_migration(updated_all_base_pop_df, scen_factor=0):
  """Calculate the total out migration numbers for all states.

  :param updated_all_base_pop_df:         A data frame containing population numbers per state
  :type updated_all_base_pop_df:          data frame

  :param scen_factor:                     (Optional) Scale applied to migration rates based on the scenario.
                                          Default scenario = 0, no domestic migration.
  :type scen_factor:                      (Optional) int

  :return:                                A data frame with total out migration population

  """

  print("Calculating domestic migration rates out of states.")
  tot_out_mig_pop = pd.DataFrame()

  for states in alphabetize_states():

    # get target state id_abbrev
    target_state = states.split('-')[1]

    # read in the contributing states in migration rates
    current_out_mig = single_state_files(target_state, '_out_mig.csv')

    # get population data for every state except the target state
    to_states = [i for i in updated_all_base_pop_df.columns if states != i]
    current_to_pop = updated_all_base_pop_df[to_states].copy()

    # set data frame to hold contributing states population
    total_out_mig_pop = pd.DataFrame()

    for state in to_states:
      state_abbrev = state.split('-')[1]

      # create a copy of the out migration data frame for the to state
      state_out_mig_urban = current_out_mig.loc[current_out_mig['to'] == state_abbrev].copy()

      # create copy for rural and zero out rate
      state_out_mig_rural = state_out_mig_urban.copy()
      state_out_mig_rural['rate'] *= 0

      # add sorting value for rural
      state_out_mig_rural['setting'] = 1

      # add sorting value for urban
      state_out_mig_urban['setting'] = 0

      # combine urban and rural data frames
      state_out_mig = pd.concat([state_out_mig_urban, state_out_mig_rural])

      # sort data frame by gender and age
      state_out_mig.sort_values(by=['gender', 'age', 'setting'], inplace=True)

      # add to all states data frame
      total_out_mig_pop[state] = state_out_mig['rate'].values

    # multiply to get the out migration numbers from each contributing state
    total_out_net_pop = (total_out_mig_pop * scen_factor) * current_to_pop

    # sum the total out migration numbers across all contributing states with one value per age group
    tot_out_mig_pop[states] = total_out_net_pop.sum(axis=1)

  return tot_out_mig_pop