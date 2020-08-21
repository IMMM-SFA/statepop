import pkg_resources
import os
import numpy as np
import pandas as pd
import matplotlib
import statepop.multistate as multi

#Path to results folder
results_path = pkg_resources.resource_filename('statepop', 'data/inputs/No_Mig')

# Path to state-level inputs folders
state_inputs = pkg_resources.resource_filename('statepop', 'data/State_Inputs')

# List of states, alphebetized
states_list = os.listdir(state_inputs)
states_list = sorted(states_list, key = lambda x: int(x.split('-')[0]))

# Specify scenario
scenarios = ("Constant_rate", "SSP2", "SSP3", "SSP5")


def get_state_dirs(root_dir, delimeter='-', target_index=0):
    """Get a list of the full file path to the state directories

    :param root_dir                 A full path with file name and extension to the directory containing the state level data

    :type root_dir:                 str

    :param ..:

    :return:                        A list of full path...

    """

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


def retrieve_csv(state_abbr, target_file):
    state_dir_list = get_state_dirs(state_inputs)  # Input data directory
    target_state = [s for s in states_list if state_abbr in s]

    if target_file == 'Constant_rate.csv':
        file_list = get_contents_file_list(state_dir_list, target_file=target_file)
        file = [i for i in file_list if target_state[0] in i][0]

        df = pd.read_csv(file, skiprows=[1, 2, 3])

    elif target_file == 'mortality.csv':
        file_list = get_contents_file_list(state_dir_list, target_file=target_file)
        file = [i for i in file_list if target_state[0] in i][0]

        df = pd.read_csv(file, usecols=['region', 'age', 'lx_rural_female',
                                        'lx_rural_male', 'lx_urban_female', 'lx_urban_male'])

    else:
        file_list = get_contents_file_list(state_dir_list, target_file=target_file)
        file = [i for i in file_list if target_state[0] in i][0]

        df = pd.read_csv(file)

    return df


def single_state_files(state_abbr, target_file, year_start=2010, year_through=2100, timestep=5):
    """Retrieve indicated state's constant rate population scenario file and assign to dataframe.

    :param state_abbr:              Two-character state abbreviations (ex: AL, AK, AZ, etc.)

    :type state_abbr:               str

    :param target_file:             CSV file being retrieved for specified state

    :type state_abbr:               str

    :param year_start:              (Optional)

    :param year_through:            (Optional)

    :param timestep:                (Optional) Years are by 5



    :return:                        Targeted CSV as a pandas DataFrame

    """

    if target_file == 'Constant_rate.csv':

        target_df = retrieve_csv(state_abbr, target_file)

        target_df = target_df.loc[target_df["year"].isin(
            list(range(year_start, year_through + timestep, timestep)))]

    elif target_file == 'basePop.csv':
        target_df = retrieve_csv(state_abbr, target_file)

    elif target_file == 'intMig.csv':
        target_df = retrieve_csv(state_abbr, target_file)

    elif target_file == 'mortality.csv':
        target_df = retrieve_csv(state_abbr, target_file)

    elif target_file == 'fertility.csv':
        target_df = retrieve_csv(state_abbr, target_file)

    elif target_file == 'domMig.csv':
        target_df = retrieve_csv(state_abbr, target_file)

    return target_df


def single_state_base_pop_df(state_abbr, int_mig_rates=0):
    # * Scenario data (The Constant_rate scenario. )
    cr_df = single_state_files(state_abbr, 'Constant_rate.csv')

    # * Base Population data
    bp_df = single_state_files(state_abbr, 'basePop.csv')

    # * International migration data
    # Note: The international migration rates input data needs to be in relative rates (summing up to 1)
    im_df = single_state_files(state_abbr, 'intMig.csv')

    # Annual total net international migration counts for 5-year periods
    female_net_international_migration = cr_df["nim_F"].round(0).astype(np.int).values
    male_net_international_migration = cr_df["nim_M"].round(0).astype(np.int).values

    # Update the base year population with the international migration
    # females -> urban0, rural0, urban1, rural1,...
    females_base_pop = pd.concat([bp_df['urban_female'], bp_df['rural_female']]).sort_index(kind='mergesort')
    females_base_pop = females_base_pop.reset_index(drop=True)

    # males
    males_base_pop = pd.concat([bp_df['urban_male'], bp_df['rural_male']]).sort_index(kind='mergesort')
    males_base_pop = males_base_pop.reset_index(drop=True)

    # combines female and male pieces
    base_pop_df = pd.DataFrame(pd.concat([females_base_pop,
                                          males_base_pop]).reset_index(drop=True))
    base_pop_df = base_pop_df.fillna(0)
    base_pop_df = base_pop_df.round(0).astype(np.int)
    base_pop_df.columns = [s for s in states_list if state_abbr in s]

    # Spread migrant numbers according to profile
    if int_mig_rates == 1:
        im_df["nmUF"] = im_df['net_female'] * female_net_international_migration[0]
        im_df["nmRF"] = im_df['nmUF']
        im_df["nmUM"] = im_df['net_male'] * male_net_international_migration[0]
        im_df['nmRM'] = im_df['nmUM']
    else:
        im_df["nmUF"] = im_df['net_female'] * 0
        im_df["nmRF"] = im_df['nmUF']
        im_df["nmUM"] = im_df['net_male'] * 0
        im_df['nmRM'] = im_df['nmUM']

    # Update the base year population with the international migration rates
    urban_rural_females = pd.concat([im_df['nmUF'], im_df['nmRF']]).sort_index(kind='mergesort')
    urban_rural_females = urban_rural_females.reset_index(drop=True)

    urban_rural_males = pd.concat([im_df['nmUM'], im_df['nmRM']]).sort_index(kind='mergesort')
    urban_rural_males = urban_rural_males.reset_index(drop=True)

    urban_rural_all = pd.DataFrame(pd.concat([urban_rural_females,
                                              urban_rural_males]).reset_index(drop=True))
    urban_rural_all.columns = [s for s in states_list if state_abbr in s]

    # Populate the data frames that store base-year population and its updated population by international migration
    initial_all_base_population = pd.DataFrame(data=base_pop_df, index=list(range(404)),
                                               columns=states_list).fillna(0)
    update_all_base_pop = base_pop_df.add(urban_rural_all, fill_value=0)

    # Keep the international migration for the current state
    current_int_migration = update_all_base_pop.subtract(base_pop_df, fill_value=0)
    total_int_migration = pd.DataFrame(data=current_int_migration, index=list(range(404)), columns=states_list)

    return total_int_migration


