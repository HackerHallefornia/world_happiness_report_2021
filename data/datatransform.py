import pandas as pd

df = pd.read_csv("world-happiness-report.csv")
df_clean = df.dropna()
df_clean.to_csv("world-happiness-report_ts.csv", index=False)