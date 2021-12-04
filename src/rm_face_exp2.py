import pandas as pd

from src.common.process_basic_data_sturcture import cal_SEM
from src.common.process_dataframe import insert_new_col_from_two_cols, rename_df_col

if __name__ == '__main__':
    PATH = "../exp2/data/"
    DATA = "rm_face_exp2_preprocessed.xlsx"

    # read data
    data = pd.read_excel(PATH + DATA)

    # averaged data: averaged deviation for each condition per participant (separate spacing)
    data_1 = data.groupby(["spacing_in_deg", "setsize", "size_w", "participant", "stimulus_types"])[
        "deviation_score"] \
        .agg(['mean', 'std']) \
        .reset_index(level = ["spacing_in_deg", "setsize", "size_w", "participant", "stimulus_types"])

    rename_df_col(df = data_1, old_col_name = "mean", new_col_name = "mean_deviation_score")
    data_1["samplesize"] = [4] * data_1.shape[0]  # each participant repeat each condition 4 times
    insert_new_col_from_two_cols(data_1, "mean_deviation_score", "samplesize", "SEM", cal_SEM)

    # averaged data: averaged deviation for each condition across participant (separate spacing)
    data_2 = data.groupby(["spacing_in_deg", "setsize", "size_w", "stimulus_types"])[
        "deviation_score"] \
        .agg(['mean', 'std']) \
        .reset_index(level = ["spacing_in_deg", "setsize", "size_w", "stimulus_types"])

    rename_df_col(df = data_2, old_col_name = "mean", new_col_name = "mean_deviation_score")
    data_2["samplesize"] = [20] * data_2.shape[0]  # each condition repeat 4*5 times
    insert_new_col_from_two_cols(data_2, "mean_deviation_score", "samplesize", "SEM", cal_SEM)

    # averaged data: face usd response
    data_3 = data.groupby(["spacing_in_deg", "setsize", "size_w", "stimulus_types"])[
        "response_usd"] \
        .agg(['mean', 'std']) \
        .reset_index(level = ["spacing_in_deg", "setsize", "size_w", "stimulus_types"])

    rename_df_col(df = data_3, old_col_name = "mean", new_col_name = "mean_resp_usd")
    data_3["samplesize"] = [20] * data_3.shape[0]  # each condition repeat 4*5 times
    insert_new_col_from_two_cols(data_3, "mean_resp_usd", "samplesize", "SEM", cal_SEM)

    # averaged data: face usd response per participant
    data_4 = data.groupby(["spacing_in_deg", "setsize", "size_w", "participant", "stimulus_types"])[
        "response_usd"] \
        .agg(['mean', 'std']) \
        .reset_index(level = ["spacing_in_deg", "setsize", "size_w", "participant", "stimulus_types"])

    rename_df_col(df = data_4, old_col_name = "mean", new_col_name = "mean_resp_usd")
    data_4["samplesize"] = [4] * data_4.shape[0]  # each participant repeat each condition 4 times
    insert_new_col_from_two_cols(data_4, "mean_resp_usd", "samplesize", "SEM", cal_SEM)

    data_4.to_excel("try.xlsx")