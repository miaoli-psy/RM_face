import pandas as pd

from src.common.process_basic_data_sturcture import cal_SEM
from src.common.process_dataframe import insert_new_col_from_two_cols, rename_df_col

if __name__ == '__main__':
    is_main_exp = True
    to_excel = False

    PATH = "../exp2/data/"

    if is_main_exp:
        DATA = "rm_face_exp2_preprocessed.xlsx"
    else:
        DATA = "rm_face_exp2_discri_preprocessed.xlsx"

    if is_main_exp:
        dv1 = "deviation_score"
        dv2 = "response_usd"
    else:
        dv1 = "correct"
        dv2 = "response_usd"

    groupby1 = "spacing"
    groupby2 = "setsize"
    groupby3 = "stimulus_size"
    groupby4 = "participant"
    groupby5 = "type"
    groupby6 = "rm_trials"

    # read data
    data = pd.read_excel(PATH + DATA)

    if is_main_exp:
        groupby_list_perpp = [groupby1, groupby2, groupby3, groupby4, groupby5, groupby6]
        groupby_list_avg = [groupby1, groupby2, groupby3, groupby5, groupby6]
    else:
        groupby_list_perpp = [groupby2, groupby3, groupby4, groupby5]
        groupby_list_avg = [groupby2, groupby3, groupby5]

    # averaged data: averaged deviation for each condition per participant (separate spacing)
    data_1 = data.groupby(groupby_list_perpp)[
        dv1] \
        .agg(['mean', 'std']) \
        .reset_index(level = groupby_list_perpp)
    data_1["samplesize"] = [4] * data_1.shape[0]  # each participant repeat each condition 4 times

    if is_main_exp:
        rename_df_col(df = data_1, old_col_name = "mean", new_col_name = "mean_deviation_score")
        insert_new_col_from_two_cols(data_1, "mean_deviation_score", "samplesize", "SEM", cal_SEM)
    else:
        rename_df_col(df = data_1, old_col_name = "mean", new_col_name = "percent_correct")
        insert_new_col_from_two_cols(data_1, "percent_correct", "samplesize", "SEM", cal_SEM)

    # averaged data: averaged deviation for each condition across participant (separate spacing)
    data_2 = data.groupby(groupby_list_avg)[
        dv1] \
        .agg(['mean', 'std']) \
        .reset_index(level = groupby_list_avg)

    data_2["samplesize"] = [20] * data_2.shape[0]  # each condition repeat 4*5 times

    if is_main_exp:
        rename_df_col(df = data_2, old_col_name = "mean", new_col_name = "mean_deviation_score")
        insert_new_col_from_two_cols(data_2, "mean_deviation_score", "samplesize", "SEM", cal_SEM)
    else:
        rename_df_col(df = data_2, old_col_name = "mean", new_col_name = "percent_correct")
        insert_new_col_from_two_cols(data_2, "percent_correct", "samplesize", "SEM", cal_SEM)

    # averaged data: face usd response
    data_3 = data.groupby(groupby_list_avg)[
        dv2] \
        .agg(['mean', 'std']) \
        .reset_index(level = groupby_list_avg)

    data_3["samplesize"] = [20] * data_3.shape[0]  # each condition repeat 4*5 times

    rename_df_col(df = data_3, old_col_name = "mean", new_col_name = "mean_resp_usd")
    insert_new_col_from_two_cols(data_3, "mean_resp_usd", "samplesize", "SEM", cal_SEM)

    # averaged data: face usd response per participant
    data_4 = data.groupby(groupby_list_perpp)[
        dv2] \
        .agg(['mean', 'std']) \
        .reset_index(level = groupby_list_perpp)

    rename_df_col(df = data_4, old_col_name = "mean", new_col_name = "mean_resp_usd")
    data_4["samplesize"] = [4] * data_4.shape[0]  # each participant repeat each condition 4 times
    insert_new_col_from_two_cols(data_4, "mean_resp_usd", "samplesize", "SEM", cal_SEM)

    if to_excel:
        data_1.to_excel("try.xlsx")
        data_2.to_excel("try2.xlsx")
        data_3.to_excel("try3.xlsx")
        data_4.to_excel("try4.xlsx")