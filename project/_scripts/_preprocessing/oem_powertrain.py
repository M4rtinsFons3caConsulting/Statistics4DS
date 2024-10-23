import pandas as pd
from pathlib import Path

# Path to the folder containing the Excel files
folder_path = '\\home\\shadybea\\Downloads\\New Folder(2)'
output_folder = folder_path
all_sales_powertrain_data = []

# Define the expected sales columns
powertrain_type = ['EV', 'FCV', 'HV', 'ICE', 'Mild HV', 'PHV', 'other']

# Locate files with the "powertrain" suffix
powertrain_files = [file for file in Path(folder_path).glob('*.xlsx') if 'powertrain' in file.stem]

# Loop through each filtered file
for file in powertrain_files:
    # Extract the country and powertrain type from the filename
    filename = file.stem  # Get the file name without extension
    country, powertrain = filename.split('_')

    # Read the Excel file
    df = pd.read_excel(file)

    # Convert the date column to year and month
    df['date'] = pd.to_datetime(df['date'], errors='coerce')  # Ensure date is in datetime format
    df['year'] = df['date'].dt.year
    df['month'] = df['date'].dt.month

    # Rename the "N/A" column to "other"
    if 'N/A' in df.columns:
        df.rename(columns={'N/A': 'other'}, inplace=True)

    # Find actual sales columns that exist in the DataFrame
    sales_by_powertrain_type = [col for col in powertrain_type if col in df.columns]

    # Check if there are any sales columns to sum
    if sales_by_powertrain_type:
        # Fill missing values in sales columns with 0
        df[sales_by_powertrain_type] = df[sales_by_powertrain_type].fillna(0)

        # Sum the specified columns and create total_sales
        df['total_sales'] = df[sales_by_powertrain_type].sum(axis=1)

        # Add the country variable to the DataFrame
        df['country'] = country

        # Filter the DataFrame to only include relevant columns
        df_filtered = df[['year', 'month', 'country'] + sales_by_powertrain_type + ['total_sales']]

        # Unpivot the sales columns into a single column
        df_melted = df_filtered.melt(id_vars=['year', 'month', 'country'],
                                      value_vars=sales_by_powertrain_type,
                                      var_name='powertrain_type',
                                      value_name='sales')

        # Add total_sales to the melted DataFrame
        total_sales_df = df_filtered[['year', 'month', 'country', 'total_sales']].copy()
        total_sales_df['powertrain_type'] = 'total_sales'
        total_sales_df.rename(columns={'total_sales': 'sales'}, inplace=True)

        # Concatenate the melted DataFrame with total_sales_df
        df_final = pd.concat([df_melted, total_sales_df], ignore_index=True)

        # Save the modified DataFrame to a new Excel file
        output_filename = f"_{country}_{powertrain}"
        df_final.to_excel(Path(output_folder) / f"{output_filename}.xlsx", index=False)

        # Append to the all_sales_powertrain_data list
        all_sales_powertrain_data.append(df_final)

# Concatenate all sales data into a single DataFrame
all_sales_powertrain = pd.concat(all_sales_powertrain_data, ignore_index=True)

# Save the concatenated DataFrame to a new Excel file
all_sales_powertrain.to_excel(Path(output_folder) / '_all_sales_powertrain.xlsx', index=False)

print("Mission accomplished! All relevant files have been processed and saved.")
