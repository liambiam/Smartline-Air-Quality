import pandas as pd
import matplotlib.pyplot as plt
import os
import numpy as np

# data_path = 'C:/Users/liambiam/Documents/MSc Applied Data Science/MTHM604/Data/VolatileOrganicCompounds'
# os.chdir(data_path)

# csv_files = [f for f in os.listdir(data_path) if f.endswith('.csv')]
# print(csv_files)

# dfs = []

# for csv in csv_files:
#     df = pd.read_csv(os.path.join(data_path, csv))
#     dfs.append(df)

final_df = pd.concat(dfs, ignore_index=True)

csv_output_path = 'path/final_dataframeVOCs.csv'
final_df.to_csv(csv_output_path, index=False)

# print(final_df.head())

particulate_matter = pd.read_csv('C:/Users/liambiam/Documents/MSc Applied Data Science/MTHM604/Data/ParticulateMatter/final_dataframePMs.csv')

VOCs = pd.read_csv('C:/Users/liambiam/Documents/MSc Applied Data Science/MTHM604/Data/VolatileOrganicCompounds/final_dataframeVOCs.csv')


# data_path_2 = 'C:/Users/liambiam/Documents/MSc Applied Data Science/MTHM604/Data/SurveysAndUprns'

# # Convert 'datetime' column to datetime type
# particulate_matter['datetime'] = pd.to_datetime(particulate_matter['datetime'])

# # Count occurrences of each datetime value
# datetime_counts = particulate_matter['datetime'].value_counts()

# # Display the counts
# print(datetime_counts)