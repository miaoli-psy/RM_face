import math

def convert_str_num_to_num(inputstrnum):
    return float(inputstrnum[-1])


def cal_deviation_score(resp, setsize):
    return resp - setsize


def cal_SEM(input_std: float, sample_number) -> float:
    """This function returns the SEM given the std and the sample size"""
    return round(input_std / math.sqrt(sample_number - 1), 4)