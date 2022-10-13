import math


def convert_str_num_to_num(inputstrnum):
    return float(inputstrnum[-1])


def cal_deviation_score(resp, setsize):
    return resp - setsize


def cal_cv(deviation, numerosity):
    return round(deviation/numerosity, 4)


def cal_SEM(input_std: float, sample_number) -> float:
    """This function returns the SEM given the std and the sample size"""
    return round(input_std / math.sqrt(sample_number - 1), 4)


def get_usd_resp_order1_pilot(is_face_usd, resp):
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


def get_usd_resp_order1(is_face_usd, resp):
    if is_face_usd == "NF":
        if resp == "w":
            return 1
        else:
            return 0
    else:
        if resp == "w":
            return 0
        else:
            return 1


def get_usd_resp_order2_pilot(is_face_usd, resp):
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


def get_usd_resp_order2(is_face_usd, resp):
    if is_face_usd == "NF":
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


def get_new_spacing_con(input_spacing: float):
    if input_spacing < 0.03:
        return "minimum "
    elif 0.1 < input_spacing < 0.3:
        return "small"
    elif 0.39 <= input_spacing <= 0.6:
        return "large"
    elif input_spacing > 0.6:
        return "to_match"


def is_resp_correct(input_dv):
    if input_dv == 0:
        return 1
    else:
        return 0


def is_face_usd(inputfacetype):
    if inputfacetype == "stims/NF_usd.png":
        return 1
    elif inputfacetype == "stims/NF.png":
        return 0


def get_if_rm_occor(dv):
    if dv < 0:
        return "RM trails"
    else:
        return "no RM trials"


def is_rm_trial(deviation_score):
    if deviation_score < 0:
        return "RM trials"
    else:
        return "non RM trials"


def is_hit(identity, response):
    if identity == "NF" and response == 1:
        return 1
    else:
        return 0


def is_miss(identity, response):
    if identity == "NF" and response == 0:
        return 1
    else:
        return 0


def is_false_alarm(identity, response):
    if identity == "NF_usd" and response == 0:
        return 1
    else:
        return 0


def is_correct_rejection(identity, response):
    if identity == "NF_usd" and response == 1:
        return 1
    else:
        return 0