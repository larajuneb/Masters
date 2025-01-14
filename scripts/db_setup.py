import sqlite3

def create_tables():
    db_path = "data/plants.db"
    conn = sqlite3.connect(db_path)
    cursor = conn.cursor()

    # Table for plant data
    cursor.execute("""
        CREATE TABLE IF NOT EXISTS plants (
            id INTEGER PRIMARY KEY AUTOINCREMENT,
            name TEXT,
            num_leaves INTEGER,
            shoot_length REAL,
            internode_length REAL,
            leaf_dimensions TEXT,
            porometer_readings TEXT,
            timestamps TEXT
        )
    """)

    # Additional tables for complex data (if needed, e.g., internode lengths or leaf dimensions)
    cursor.execute("""
        CREATE TABLE IF NOT EXISTS internode_lengths (
            id INTEGER PRIMARY KEY AUTOINCREMENT,
            plant_id INTEGER,
            timestamp TEXT,
            length REAL,
            FOREIGN KEY (plant_id) REFERENCES plants (id)
        )
    """)

    cursor.execute("""
        CREATE TABLE IF NOT EXISTS leaf_dimensions (
            id INTEGER PRIMARY KEY AUTOINCREMENT,
            plant_id INTEGER,
            timestamp TEXT,
            leaf_number INTEGER,
            width REAL,
            length REAL,
            FOREIGN KEY (plant_id) REFERENCES plants (id)
        )
    """)

    conn.commit()
    conn.close()
    print("Tables created successfully.")

if __name__ == "__main__":
    create_tables()