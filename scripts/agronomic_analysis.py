import sqlite3
import pandas as pd
from contextlib import closing

# Connect to the database (or create it if it doesn't exist)
conn = sqlite3.connect("plants.db")

# Create a cursor object to execute SQL commands
cursor = conn.cursor()

# Create the plants table (run this only once)
cursor.execute('''CREATE TABLE IF NOT EXISTS plants (
    id INTEGER PRIMARY KEY AUTOINCREMENT,
    name TEXT,
    num_leaves INTEGER,
    shoot_length REAL,
    internode_length REAL,
    leaf_dimensions TEXT,
    porometer_readings TEXT,
    timestamps TEXT
)''')

conn.commit()

# Read the spreadsheet
df = pd.read_excel("data.xlsx")

# Iterate through rows and populate the database
for index, row in df.iterrows():
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

def load_plant_from_db(conn, plant_id):
    cursor = conn.cursor()
    cursor.execute("SELECT * FROM plants WHERE id = ?", (plant_id,))
    row = cursor.fetchone()
    if row:
        return Plant(
            name=row[1],
            num_leaves=row[2],
            shoot_length=row[3],
            internode_length=row[4],
            leaf_dimensions=json.loads(row[5]),
            porometer_readings=json.loads(row[6]),
            timestamps=json.loads(row[7])
        )

# with closing(sqlite3.connect("plants.db")) as conn:
#     plant = load_plant_from_db(conn, plant_id=1)
#     print(plant.name)