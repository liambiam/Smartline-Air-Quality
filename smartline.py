import pandas as pd
import matplotlib.pyplot as plt

# df = pd.read_csv("smartline_1000_P2577.csv")
df = pd.read_csv("856596_Data\856956_Data\Indoor_VOCs_and_eCO2 (1)\Indoor_VOCs_and_eCO2\smartline_1000_V2522.csv")
# df3 = pd.read_csv(r"856596_Data\856956_Data\External_air_quality (2)\External_air_quality\smartline_1011_X2524.csv")

df['datetime'] = pd.to_datetime(df['datetime'])
# df['datetime'] = pd.to_datetime(df['datetime'])
# df3['datetime'] = pd.to_datetime(df3['datetime'])

#print('Hello')
#print(df.info())
print(df)
# print(df.info())
# print(df3.info())
# Plotting
plt.figure(figsize=(10, 6))
plt.plot(df['datetime'], df['Value'], marker='o', linestyle='-', color='b')
plt.title('Value vs Datetime')
plt.xlabel('Datetime')
plt.ylabel('Value')
plt.grid(True)
plt.show()
