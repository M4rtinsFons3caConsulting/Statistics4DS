import pandas as pd
import glob
import os

# Directory containing the country folders
base_directory = '/home/shadybea/Downloads/New Folder'
# List to hold dataframes for final concatenation
all_dataframes = []

# Get all country folders in the base directory
country_folders = [f for f in os.listdir(base_directory) if os.path.isdir(os.path.join(base_directory, f))]

# Loop through each country folder
for country in country_folders:
    country_directory = os.path.join(base_directory, country)
    # Get all .xlsx files in the country folder
    xlsx_files = glob.glob(os.path.join(country_directory, '*.xlsx'))

    # Sort files by filename
    xlsx_files.sort()

    # List to hold dataframes for the current country
    country_dataframes = []

    # Set starting year and month
    start_year = 2022
    start_month = 1

    # Loop through sorted files and process
    for index, file in enumerate(xlsx_files):
        try:
            # Read the Excel file
            df = pd.read_excel(file)  # No need for encoding here
        except Exception as e:
            print(f"Error reading {file}: {e}")
            continue

        # Calculate year and month based on the starting point
        total_months = index + (start_month - 1) + (start_year - 2022) * 12
        year = total_months // 12
        month = total_months % 12 + 1  # Month as 1-12
        # Add new columns
        df['year'] = year + start_year
        df['month'] = month
        df['country'] = country  # Add country as a new column
        # Append to the list for the current country
        country_dataframes.append(df[['year', 'month', 'brand_id', 'Sales', 'country']])

    # Concatenate country dataframes and save to respective Excel file
    country_final_df = pd.concat(country_dataframes, ignore_index=True)
    country_final_path = os.path.join(base_directory, f'{country}_brand.xlsx')
    country_final_df.to_excel(country_final_path, index=False)

    # Append to the overall list for final concatenation
    all_dataframes.append(country_final_df)

# Concatenate all country dataframes and save to a single Excel file
final_all_sales_df = pd.concat(all_dataframes, ignore_index=True)
final_all_sales_path = os.path.join(base_directory, 'all_sales_brand.xlsx')
final_all_sales_df.to_excel(final_all_sales_path, index=False)

print("Processing complete!")