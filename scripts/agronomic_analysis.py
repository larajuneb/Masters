from models.plant import Plant
import sqlite3
from contextlib import closing

def analyze_plant_data(plant_id):
    db_path = "data/plants.db"
    with closing(sqlite3.connect(db_path)) as conn:
        plant = Plant.load_plant_from_db(conn, plant_id)
        if plant:
            print(f"Plant Name: {plant.name}")
            print(f"Number of Leaves: {plant.num_leaves}")
            print(f"Internode Length: {plant.internode_length}")
        else:
            print("Plant not found.")

if __name__ == "__main__":
    plant_id = 1  # Change this to the plant ID you want to analyze
    analyze_plant_data(plant_id)