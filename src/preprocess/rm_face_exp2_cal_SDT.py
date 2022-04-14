import pandas as pd
from scipy.stats import norm
import math

Z = norm.ppf


def SDT(hits, misses, fas, crs):
    """ returns a dict with d-prime measures given hits, misses, false alarms, and correct rejections"""
    # Floors an ceilings are replaced by half hits and half FA's
    half_hit = 0.5 / (hits + misses)
    half_fa = 0.5 / (fas + crs)

    # Calculate hit_rate and avoid d' infinity
    hit_rate = hits / (hits + misses)
    if hit_rate == 1:
        hit_rate = 1 - half_hit
    if hit_rate == 0:
        hit_rate = half_hit

    # Calculate false alarm rate and avoid d' infinity
    fa_rate = fas / (fas + crs)
    if fa_rate == 1:
        fa_rate = 1 - half_fa
    if fa_rate == 0:
        fa_rate = half_fa

        # Calculate hit_rate and avoid d' infinity
    hit_rate = hits / (hits + misses)
    if hit_rate == 1:
        hit_rate = 1 - half_hit
    if hit_rate == 0:
        hit_rate = half_hit

    # Calculate false alarm rate and avoid d' infinity
    fa_rate = fas / (fas + crs)
    if fa_rate == 1:
        fa_rate = 1 - half_fa
    if fa_rate == 0:
        fa_rate = half_fa

    d_prime = Z(hit_rate) - Z(fa_rate)
    beta = math.exp((Z(fa_rate) ** 2 - Z(hit_rate) ** 2) / 2)
    c = -(Z(hit_rate) + Z(fa_rate)) / 2
    ad = norm.cdf(d_prime / math.sqrt(2))

    return d_prime, beta, c, ad


if __name__ == '__main__':
    to_excel = False
    is_main_exp = False
    if is_main_exp:
        # removed 4 rows where hit+miss == 0 or FA+CR == 0
        # e.g. in non RM trails, participant A, large stimuli, there was no upright face trails
        totalData = pd.read_csv("../../data/rm_face_to_cal_SDT.csv")
    else:
        totalData = pd.read_csv("../../data/rm_face_disc_to_cal_SDT.csv")

    totalData["d_prime"] = totalData.apply(lambda x: SDT(x["hit"], x["miss"], x["FA"], x["CR"])[0], axis = 1)
    totalData["beta"] = totalData.apply(lambda x: SDT(x["hit"], x["miss"], x["FA"], x["CR"])[1], axis = 1)
    totalData["c"] = totalData.apply(lambda x: SDT(x["hit"], x["miss"], x["FA"], x["CR"])[2], axis = 1)
    totalData["ad"] = totalData.apply(lambda x: SDT(x["hit"], x["miss"], x["FA"], x["CR"])[3], axis = 1)

    if to_excel:
        if is_main_exp:
            totalData.to_excel("rm_face_SDT.xlsx", index = False)
        else:
            totalData.to_excel("rm_face_disc_SDT.xlsx", index = False)
