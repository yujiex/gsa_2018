import os
import pandas as pd

df = pd.read_csv(os.getcwd() + "/seeddb/gsa_energy/upload.csv", nrows=10)
df["Custom ID"] = 1
df["Custom Meter ID"] = 1
df.to_excel(os.getcwd() + "/seeddb/gsa_energy/upload_small.xlsx", index=False)
