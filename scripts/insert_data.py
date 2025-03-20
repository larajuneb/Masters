import sqlite3
import pandas as pd
import json
from models.plant import Plant

# Script to populate database from spreadsheets

def load_data_from_spreadsheets():

    # Connect to the database
    conn = sqlite3.connect("database/plants.db")
    cursor = conn.cursor()

    # Example for one spreadsheet (repeat for others)
    df = pd.read_excel("path/to/your/spreadsheet.xlsx")

    for _, row in df.iterrows():
        plant = Plant(
            name=row['name'],
            num_leaves=row['num_leaves'],
            shoot_length=row['shoot_length'],
            internode_length=row['internode_length'],
            leaf_dimensions=json.loads(row['leaf_dimensions']),
            porometer_readings=json.loads(row['porometer_readings']),
            timestamps=json.loads(row['timestamps'])
        )
        plant.save_to_db(conn)

    conn.close()
    print("Data loaded successfully.")

def load_leaf_numbers():
    

if __name__ == "__main__":
    load_data_from_spreadsheets()