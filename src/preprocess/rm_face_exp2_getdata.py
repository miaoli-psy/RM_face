'''
read raw data of exp2
'''
import pandas as pd
import os

from src.common.process_basic_data_sturcture import convert_str_num_to_num, cal_deviation_score
from src.common.process_dataframe import process_col, insert_new_col_from_two_cols
from src.constants.rm_face_exp1_constants import COL_exp1, COL_exp1_discri

if __name__ == '__main__':
    to_excel = False
    is_main_exp = True

    # TODO
    if is_main_exp:
        PATH = "../../exp2/data/raw_data_exp2/"
        col = COL_exp1
    else:
        PATH = "../../exp2/data/raw_data_exp2_discri/"
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

    if to_excel:
        totalData.to_excel("try.xlsx", index = False)