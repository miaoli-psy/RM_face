'''
read raw data of exp2
'''
import pandas as pd
import os

from src.common.process_basic_data_sturcture import convert_str_num_to_num, get_usd_resp_order1, get_usd_resp_order2, \
    cal_deviation_score, convert_size_to_str, convert_stimulus_type_exp2
from src.common.process_dataframe import insert_new_col_from_two_cols, insert_new_col
from src.constants.rm_face_exp1_constants import COL_exp2, COL_exp2_discri

if __name__ == '__main__':
    to_excel = False
    is_main_exp = True

    # TODO
    if is_main_exp:
        PATH = "../../exp2/data/raw_data_exp2/"
        col = COL_exp2
    else:
        PATH = "../../exp2/data/raw_data_exp2_discri/"
        col = COL_exp2_discri

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
    totalData = totalData.dropna(subset = ['n'])

    # rename columns/convert num to float
    totalData = totalData.rename(columns = {"key_resp.keys":   "response1",
                                            "faces":           "stimulus_types",
                                            "key_resp_7.keys": "response2"})

    # process 2 responses
    totalData_order1 = totalData[totalData["order"] == 1]
    totalData_order2 = totalData[totalData["order"] == 2]

    insert_new_col_from_two_cols(totalData_order1, "is_face_usd", "response2", "response_usd", get_usd_resp_order1)
    insert_new_col_from_two_cols(totalData_order2, "is_face_usd", "response1", "response_usd", get_usd_resp_order2)

    insert_new_col(totalData_order1, "response1", "response_num", convert_str_num_to_num)
    insert_new_col(totalData_order2, "response2", "response_num", convert_str_num_to_num)

    totalData = pd.concat([totalData_order1, totalData_order2])

    # cal deviation score
    insert_new_col_from_two_cols(totalData, "response_num", "setsize", "deviation_score", cal_deviation_score)

    # convert size to str - categorical
    insert_new_col(totalData, "size_w", "stimulus_size", convert_size_to_str)

    # convert stimulus type
    insert_new_col(totalData, "stimulus_types", "type", convert_stimulus_type_exp2)

    if to_excel:
        totalData.to_excel("try.xlsx", index = False)
