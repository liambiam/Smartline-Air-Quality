# ## LIBRARIES ##

import pandas as pd
import matplotlib.pyplot as plt
import os
import numpy as np
import glob

# ## FILE PATHS ##

# file_path = 'C:/Users/liambiam/Documents/MSc Applied Data Science/MTHM604/Data/'
# uprns_path = file_path + 'SurveyAndUprns/uprns.csv'
# pm_path = file_path + 'ParticulateMatter/'
# voc_path = file_path + 'VolatileOrganicCompounds/'
# external_path = file_path + 'External/'

# ## UPRN DATAFRAME ##

# uprns_df = pd.read_csv(uprns_path)

# ## FUNCTIONS ##

# def process_data_for_sid(sid_colname, response_file_path, uprns_df):
#     master_df = pd.DataFrame()  # Initialize an empty DataFrame for master_df

#     for index, row in uprns_df.iterrows():
#         if 'UPRN' in uprns_df.columns:
#             # Convert 'UPRN' value to integer
#             uprn_value = pd.to_numeric(row['UPRN'], errors='coerce', downcast='integer')

#             # Convert 'SID' column to integer after coercing to numeric
#             sid_value = pd.to_numeric(row[sid_colname], errors='coerce', downcast='integer')

#             # Check if 'sid_value' is a valid integer
#             if not pd.isna(sid_value):
#                 file_pattern = os.path.join(response_file_path, f'ts_*_sid{int(sid_value)}.csv')
#                 print(f"Searching for files with pattern: {file_pattern}")

#                 response_file_paths = glob.glob(file_pattern)
#                 print(f"Found {len(response_file_paths)} matching file(s).")

#                 if len(response_file_paths) == 1:
#                     file_path = response_file_paths[0]
#                     if os.path.exists(file_path):
#                         df = pd.read_csv(file_path)

#                         # Use the original 'UPRN' value
#                         df['UPRN'] = uprn_value

#                         df.to_csv(file_path, index=False, header=True)
#                         print(f"Successfully appended 'UPRN' column to '{file_path}'")
#                         print(df.head(2), "\n")
#                         master_df = pd.concat([master_df, df], ignore_index=True)
#                     else:
#                         print(f"Error: File '{file_path}' not found.")
#                 elif len(response_file_paths) == 0:
#                     print(f"Error: No file found for SID {sid_value}.")
#                 else:
#                     print(f"Error: Multiple files found for SID {sid_value}.")
#             else:
#                 print(f"Error: Invalid SID value for UPRN {uprn_value}.")
#         else:
#             print("Error: 'UPRN' column not found in the DataFrame.")

#     print("Processing complete.")
#     return master_df

# def read_files_into_dataframe(folder_path):
#     all_files = os.listdir(folder_path)
#     dfs = []

#     # Loop through each file and read it into a DataFrame
#     for file in all_files:
#         file_path = os.path.join(folder_path, file)

#         df = pd.read_csv(file_path)
#         dfs.append(df)

#     combined_df = pd.concat(dfs, ignore_index=True)

#     return combined_df

# #pm_df = process_data_for_sid('SID_ParticulateMatter', pm_path, uprns_df)
# pm_df = read_files_into_dataframe(pm_path)
# print(pm_df)

# #voc_df = process_data_for_sid('SID_VolatileOrganicCompounds', voc_path, uprns_df)
# voc_df = read_files_into_dataframe(voc_path)
# print(voc_df.head())

# external_df = process_data_for_sid('SID_External', external_path, uprns_df)

import pandas as pd

# Replace 'path_to_uprns_csv' with the actual path to your 'uprns.csv' file
master_data_path = 'C:/Users/liambiam/Documents/MSc Applied Data Science/MTHM604/Data/master_data/master_data.csv'

master_data_df = pd.read_csv(master_data_path)


print(master_data_df.head(50))  # Display the first 50 rows for illustration

print("DataFrame Info:")
print(master_data_df.info())

# Display summary statistics of the numeric columns
print("\nSummary Statistics:")
print(master_data_df.describe())