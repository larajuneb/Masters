import sqlite3

def initialize_database():
    with sqlite3.connect("database/plants.db") as conn:
        with open("database/schema.sql", "r") as f:
            conn.executescript(f.read())
    print("Database initialized successfully!")

if __name__ == "__main__":
    initialize_database()