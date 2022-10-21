import os
import pandas as pd

includes = pd.read_csv("../data/rayyan/final_included_with_NCT.csv")
nct_values = includes.NCT.values

for nct in nct_values:
    os.system(f'python do_one_nct.py {nct}')
