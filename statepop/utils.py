import pkg_resources
import os
import pandas as pd

def get_state_dirs(root_dir, delimeter='-', target_index=0):
    """Get a list of the full file path to the state directories

    :param root_dir                 A full path with file name and extension to the directory
                                    containing the state level data.

    :type root_dir:                 str

    :param delimeter:               (Optional) Character that separates state directories from
                                    other non-target directories. Default is '-'.
    :type delimeter:                str

    :param target_index:            (Optional) Index to start search, default is 0.

    :type target_index:             int

    :return:                        A list of full paths to the state directories.

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


def sort_dirs_by_number(dir_list):
    """Sort directories by their leading number.

    :param dir_list:      List if directories containing a "<number>_<abbrev>" structure
    :type dir_list:       list

    :return:              List of sorted directories by leading number

    """

    # create a dictionary of {state_id_integer: directory_name, ...}
    d_states = {int(i.split('-')[0]): i for i in dir_list}

    return [d_states[k] for k in sorted(d_states.keys())]

def alphabetize_states():
    """Sort directories by their leading number within the dataset.

    :return:              List of sorted directories by leading number

    """

    # Path to state-level inputs folders
    state_inputs = pkg_resources.resource_filename('statepop', 'data/State_Inputs')

    # List of states, alphebetized
    states_list = [i for i in os.listdir(state_inputs) if '.DS_Store' not in i]
    states_list = sorted(states_list, key=lambda x: int(x.split('-')[0]))

    return states_list


def get_target_file_list(state_dirs, target_file):
    """Retrieves full path of target file.

    :param state_dirs:      A list of full paths to state directories

    :type state_dirs:       str

    :param target_file:     CSV file to be retrieved

    :type target_file:      str

    :return:                Full path to target file.

    """

    # Path to state-level inputs folders
    csv_files = []
    for i in state_dirs:

        target_dirs = os.listdir(i)
        if target_file in target_dirs:
            csv_files.append(os.path.join(i, target_file))

        else:
            continue
            #print(f"No contents file in directory {i}")

    return csv_files


def retrieve_csv(state_abbr, target_file):
    """Retrieve indicated state's specified csv file.

    :param state_abbr:              Two-character state abbreviations

    :type state_abbr:               str

    :param target_file:             CSV file to retrieve

    :type target_file:              str

    :return:                        Target CSV file as a pandas DataFrame

    """

    # Path to state-level inputs folders
    state_inputs = pkg_resources.resource_filename('statepop', 'data/State_Inputs')
    state_dir_list = get_state_dirs(state_inputs)  # Input data directory

    # List of states, alphebetized
    states_list = alphabetize_states()
    target_state = [s for s in states_list if state_abbr in s]

    if target_file == 'Constant_rate.csv':
        file_list = get_target_file_list(state_dir_list, target_file=target_file)
        file = [i for i in file_list if target_state[0] in i][0]

        df = pd.read_csv(file, skiprows=[1, 2, 3])

    elif target_file == 'mortality.csv':
        file_list = get_target_file_list(state_dir_list, target_file=target_file)
        file = [i for i in file_list if target_state[0] in i][0]

        df = pd.read_csv(file, usecols=['region', 'age', 'lx_rural_female',
                                        'lx_rural_male', 'lx_urban_female', 'lx_urban_male'])

    elif target_file == '_in_mig.csv':
        file_list = get_target_file_list(state_dir_list, target_file=f'{target_state[0]}_in_mig.csv')
        file = [i for i in file_list if target_state[0] in i][0]

        df = pd.read_csv(file)

    elif target_file == '_out_mig.csv':
        file_list = get_target_file_list(state_dir_list, target_file=f'{target_state[0]}_out_mig.csv')
        file = [i for i in file_list if target_state[0] in i][0]

        df = pd.read_csv(file)

    else:
        file_list = get_target_file_list(state_dir_list, target_file=target_file)
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

