import pandas as pd

from src.common.process_basic_data_sturcture import cal_SEM
from src.common.process_dataframe import insert_new_col_from_two_cols, rename_df_col

if __name__ == '__main__':
    is_main_exp = True
    PATH = "../exp1/data/"
    if is_main_exp:
        DATA = "rm_face_exp1_preprocessed.xlsx"
    else:
        DATA = "rm_face_exp1_discri_preprocessed.xlsx"
    to_excel = False

    # read data
    data = pd.read_excel(PATH + DATA)

    # averaged data: averaged deviation for each condition per participant (separate spacing)
    data_1 = data.groupby(["spacing_in_deg", "setsize", "stimulus_size", "participant", "type"])[
        "deviation_score"] \
        .agg(['mean', 'std']) \
        .reset_index(level = ["spacing_in_deg", "setsize", "stimulus_size", "participant", "type"])

    rename_df_col(df = data_1, old_col_name = "mean", new_col_name = "mean_deviation_score")
    data_1["samplesize"] = [4] * data_1.shape[0]  # each participant repeat each condition 4 times
    insert_new_col_from_two_cols(data_1, "mean_deviation_score", "samplesize", "SEM", cal_SEM)

    # averaged data: averaged deviation for each condition across participant (separate spacing)
    data_2 = data.groupby(["spacing_in_deg", "setsize", "stimulus_size", "type"])[
        "deviation_score"] \
        .agg(['mean', 'std']) \
        .reset_index(level = ["spacing_in_deg", "setsize", "stimulus_size", "type"])

    rename_df_col(df = data_2, old_col_name = "mean", new_col_name = "mean_deviation_score")
    data_2["samplesize"] = [24] * data_2.shape[0]  # each condition repeat 4*6 times
    insert_new_col_from_two_cols(data_2, "mean_deviation_score", "samplesize", "SEM", cal_SEM)

    # data where the first block is face, blocks.thisIndex = 0, 3 were face blocks
    data_first_face_block = data[data["blocks.thisN"] == 0]
    data_first_face_block = data_first_face_block[data_first_face_block["blocks.thisIndex"].isin([0, 3])]

    data_3 = data_first_face_block.groupby(["spacing_in_deg", "setsize", "stimulus_size"])[
        "deviation_score"] \
        .agg(['mean', 'std']) \
        .reset_index(level = ["spacing_in_deg", "setsize", "stimulus_size"])

    rename_df_col(df = data_3, old_col_name = "mean", new_col_name = "mean_deviation_score")
    data_3["samplesize"] = [12] * data_3.shape[0]  # each condition repeat 4*3 times
    insert_new_col_from_two_cols(data_3, "mean_deviation_score", "samplesize", "SEM", cal_SEM)

    data_4 = data_first_face_block.groupby(["spacing_in_deg", "setsize", "stimulus_size", "participant"])[
        "deviation_score"] \
        .agg(['mean', 'std']) \
        .reset_index(level = ["spacing_in_deg", "setsize", "stimulus_size", "participant"])

    rename_df_col(df = data_4, old_col_name = "mean", new_col_name = "mean_deviation_score")
    data_4["samplesize"] = [4] * data_4.shape[0]  # each condition repeat 4times
    insert_new_col_from_two_cols(data_4, "mean_deviation_score", "samplesize", "SEM", cal_SEM)

    if to_excel:
        data_1.to_excel("try.xlsx")
        data_2.to_excel("try2.xlsx")
        data_3.to_excel("try3.xlsx")
        data_4.to_excel("try4.xlsx")
