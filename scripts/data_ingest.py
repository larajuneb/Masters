import sqlite3
import pandas as pd
import json
from plant import Plant

def load_data_from_spreadsheets():
    db_path = "data/plants.db"
    conn = sqlite3.connect(db_path)

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

if __name__ == "__main__":
    load_data_from_spreadsheets()