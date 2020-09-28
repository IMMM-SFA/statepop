import pkg_resources
import os
import pandas as pd

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
    csv_files = []
    for i in state_dirs:

        contents = os.listdir(i)
        if target_file in contents:
            csv_files.append(os.path.join(i, target_file))

        else:
            continue
            #print(f"No contents file in directory {i}")

    return csv_files


def retrieve_csv(state_abbr, target_file):
    # Path to state-level inputs folders
    state_inputs = pkg_resources.resource_filename('statepop', 'data/State_Inputs')
    state_dir_list = get_state_dirs(state_inputs)  # Input data directory
    
    # List of states, alphebetized
    states_list = os.listdir(state_inputs)
    states_list = sorted(states_list, key = lambda x: int(x.split('-')[0]))
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

    elif target_file == '_in_mig.csv':
        file_list = get_contents_file_list(state_dir_list, target_file=f'{target_state[0]}_in_mig.csv')
        file = [i for i in file_list if target_state[0] in i][0]

        df = pd.read_csv(file)

    elif target_file == '_out_mig.csv':
        file_list = get_contents_file_list(state_dir_list, target_file=f'{target_state[0]}_out_mig.csv')
        file = [i for i in file_list if target_state[0] in i][0]

        df = pd.read_csv(file)

    else:
        file_list = get_contents_file_list(state_dir_list, target_file=target_file)
        file = [i for i in file_list if target_state[0] in i][0]

        df = pd.read_csv(file)

    return df


def single_state_files(state_abbr, target_file, year_start=2010, year_through=2100, timestep=5):
    """Retrieve indicated state's specified csv file and assign to dataframe.

    :param state_abbr:              Two-character state abbreviations

    :type state_abbr:               str

    :param year_start:              (Optional)

    :param year_through:            (Optional)

    :param timestep:                (Optional) Years are in increments of 5



    :return:                        Target CSV file as a pandas DataFrame

    """

    if target_file == 'Constant_rate.csv':

        target_df = retrieve_csv(state_abbr, target_file)

        target_df = target_df.loc[target_df["year"].isin(
            list(range(year_start, year_through + timestep, timestep)))]

    else:
        target_df = retrieve_csv(state_abbr, target_file)

    return target_df

