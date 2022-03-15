'''
read raw data of exp1
'''
import pandas as pd
import os

from src.common.process_basic_data_sturcture import convert_str_num_to_num, cal_deviation_score, convert_size_to_str, \
    convert_stimulus_type_exp1, get_new_spacing_con, is_resp_correct
from src.common.process_dataframe import process_col, insert_new_col_from_two_cols, insert_new_col
from src.constants.rm_face_pilot_exp_constants import COL_exp1, COL_exp1_discri

if __name__ == '__main__':
    to_excel = False
    is_main_exp = True

    # TODO
    if is_main_exp:
        PATH = "../../pilot_exp1/data/raw_data_exp1/"
        col = COL_exp1
    else:
        PATH = "../../pilot_exp1/data/raw_data_exp1_discri/"
        col = COL_exp1_discri

    # list data file
    files = os.listdir(PATH)

    # read raw data
    data_csv = [file for file in files if file.endswith('csv')]

    totalData = pd.DataFrame()
    for file_name in data_csv:
        data = pd.read_csv(PATH + file_name)
        totalData = totalData.append(data)

    # keep valid cols
    totalData = totalData[col]

    # drop practice trials
    totalData = totalData.dropna(subset = ['key_resp.keys'])

    # rename columns/convert num to float
    totalData = totalData.rename(columns = {"key_resp.keys": "response", "faces": "stimulus_types"})
    process_col(totalData, "response", convert_str_num_to_num)

    # cal deviation score
    insert_new_col_from_two_cols(totalData, "response", "setsize", "deviation_score", cal_deviation_score)

    # convert size to str - categorical
    insert_new_col(totalData, "size_w", "stimulus_size", convert_size_to_str)

    # convert stimulus type
    insert_new_col(totalData, "stimulus_types", "type", convert_stimulus_type_exp1)

    # insert new spacing condition
    if is_main_exp:
        insert_new_col(totalData, "spacing_in_deg", "spacing", get_new_spacing_con)

    # insert correct or incorrect
    if not is_main_exp:
        insert_new_col(totalData, "deviation_score", "correct", is_resp_correct)

    if to_excel:
        totalData.to_excel("try.xlsx", index = False)
