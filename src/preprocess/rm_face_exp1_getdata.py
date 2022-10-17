import os

import pandas as pd

from src.common.process_basic_data_sturcture import convert_str_num_to_num, cal_deviation_score, get_new_spacing_con
from src.common.process_dataframe import process_col, insert_new_col_from_two_cols, insert_new_col
from src.constants.rm_face_pilot_exp_constants import COL_exp1, COL_exp1_discri

if __name__ == '__main__':
    to_excel = False
    is_main_exp = True

    if is_main_exp:
        PATH = "../../data/raw_data/exp1/"
        col = COL_exp1
    else:
        PATH = "../../data/raw_data/exp1_disc/"
        col = COL_exp1_discri


    # list data file
    files = os.listdir(PATH)

    # read raw data
    data_csv = [file for file in files if file.endswith(".csv")]

    totalData = pd.DataFrame()
    for file_name in data_csv:
        data = pd.read_csv(PATH + file_name)
        totalData = totalData.append(data)

    # keep valid cols
    totalData = totalData[col]

    # drop practice trials
    totalData = totalData.dropna(subset = ['key_resp.keys'])

    # rename columns/convert num to float
    totalData = totalData.rename(columns = {"key_resp.keys": "response",
                                            "identity":      "stimulus_types",
                                            "key_resp.rt":   "rt"})

    process_col(totalData, "response", convert_str_num_to_num)

    # cal deviation score
    insert_new_col_from_two_cols(totalData, "response", "setsize", "deviation_score", cal_deviation_score)

    # insert new spacing condition
    if is_main_exp:
        insert_new_col(totalData, "spacing_in_deg", "spacing", get_new_spacing_con)

    if to_excel:
        if is_main_exp:
            totalData.to_excel("exp1_preprocessed.xlsx", index = False)
        else:
            totalData.to_excel("exp1_disc_preprocessed.xlsx", index = False)
