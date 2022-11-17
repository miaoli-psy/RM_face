import os

import pandas as pd

from src.common.process_basic_data_sturcture import get_usd_resp_order1, get_usd_resp_order2, convert_str_num_to_num, \
    cal_deviation_score, get_new_spacing_con, is_rm_trial, is_hit, is_miss, is_false_alarm, is_correct_rejection
from src.common.process_dataframe import insert_new_col_from_two_cols, insert_new_col
from src.constants.rm_face_pilot_exp_constants import COL_exp2

if __name__ == '__main__':
    to_excel = False
    is_main_exp = True

    if is_main_exp:
        PATH = "../../data/raw_data/exp2/"
    else:
        PATH = "../../data/raw_data/exp2_disc/"

    col = COL_exp2

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
    totalData = totalData.dropna(subset = ['identity'])

    # rename columns/convert num to float
    totalData = totalData.rename(columns = {"key_resp.keys":   "response1",
                                            "key_resp.rt": "response1_rt",
                                            "identiry": "stimulus_types",
                                            "key_resp_7.keys": "response2",
                                            "key_resp_7.rt": "response2_rt"})

    # process 2 responses
    totalData_order1 = totalData[totalData["order"] == 1]
    totalData_order2 = totalData[totalData["order"] == 2]

    insert_new_col_from_two_cols(totalData_order1, "identity", "response2", "response_usd",
                                 get_usd_resp_order1)
    insert_new_col_from_two_cols(totalData_order2, "identity", "response1", "response_usd",
                                 get_usd_resp_order2)

    insert_new_col(totalData_order1, "response1", "response_num", convert_str_num_to_num)
    insert_new_col(totalData_order2, "response2", "response_num", convert_str_num_to_num)

    totalData = pd.concat([totalData_order1, totalData_order2])

    # cal deviation score
    insert_new_col_from_two_cols(totalData, "response_num", "setsize", "deviation_score", cal_deviation_score)

    # rm trials or not
    insert_new_col(totalData, "deviation_score", "is_rm_trial", is_rm_trial)

    if is_main_exp:
        insert_new_col(totalData, "spacing_in_deg", "spacing", get_new_spacing_con)

    # add hit, miss, FA and CR
    insert_new_col_from_two_cols(totalData, "identity", "response_usd", "hit", is_hit)
    insert_new_col_from_two_cols(totalData, "identity", "response_usd", "miss", is_miss)
    insert_new_col_from_two_cols(totalData, "identity", "response_usd", "FA", is_false_alarm)
    insert_new_col_from_two_cols(totalData, "identity", "response_usd", "CR", is_correct_rejection)

    if to_excel:
        if is_main_exp:
            totalData.to_excel("exp2_preprocessed.xlsx", index = False)
        else:
            totalData.to_excel("exp2_disc_preprocessed.xlsx", index = False)
