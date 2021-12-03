import math


def convert_str_num_to_num(inputstrnum):
    return float(inputstrnum[-1])


def cal_deviation_score(resp, setsize):
    return resp - setsize


def cal_SEM(input_std: float, sample_number) -> float:
    """This function returns the SEM given the std and the sample size"""
    return round(input_std / math.sqrt(sample_number - 1), 4)


def get_usd_resp_order1(is_face_usd, resp):
    if is_face_usd == 0:
        if resp == "w":
            return 1
        else:
            return 0
    else:
        if resp == "w":
            return 0
        else:
            return 1


def get_usd_resp_order2(is_face_usd, resp):
    if is_face_usd == 0:
        if resp == "w":
            return 0
        else:
            return 1
    else:
        if resp == "w":
            return 1
        else:
            return 0


def convert_size_to_str(inputsize: float):
    if inputsize == 0.74:
        return "small"
    elif inputsize == 0.936:
        return "medium"
    elif inputsize == 1.132:
        return "large"


def convert_stimulus_type_exp1(inputtype: str):
    if inputtype == "stims/outline.png":
        return "outline"
    elif inputtype == "stims/surface.png":
        return "surface"
    elif inputtype == "stims/NF.png":
        return "face"


def convert_stimulus_type_exp2(inputtype: str):
    if inputtype == "stims/NF.png":
        return "normal"
    elif inputtype == "stims/NF_usd.png":
        return "upside_down"
