import pandas as pd

if __name__ == '__main__':
    PATH = "../../data/"
    FILE = "data_by_subject_long.csv"

    data_long = pd.read_csv(PATH + FILE, index_col=[0])

    table = pd.pivot_table(data_long,
                           values = ["deviation_score_mean"],
                           index = ["participant"],
                           columns = ["setsize", "stimulus_types", "size_scale"])

    table.to_excel("data_wide.xlsx")
