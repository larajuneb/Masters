import sqlite3
import json

# Plant model and related methods

class Plant:
    def __init__(self, barcode, treatment, greenhouse_set) -> None:
        self.barcode = barcode
        self.treatment = treatment
        self.greenhouse_set = greenhouse_set
        self.stem_diameter = {} #{date: stem diameter}
        self.num_leaves = {} #{date: # leaves}
        # self.shoot_length = {} #{date: shoot length}, add something to tag if it is a new selected shoot
        self.tracked_leaves = [] #type Leaf, to track leaf dimensions
        # self.INL_shoot = {} #{date: [list of #'s from shoot tip to base]}, add something to tag if it is a new selected shoot

    def __str__(self):
        return f"{self.barcode}{self.greenhouse_set} ({self.treatment})"   

    def save_to_db(self, conn):
        cursor = conn.cursor()
        cursor.execute("""
            INSERT INTO plants (name, num_leaves, shoot_length, internode_length, leaf_dimensions, porometer_readings, timestamps)
            VALUES (?, ?, ?, ?, ?, ?, ?)
        """, (
            self.name,
            self.num_leaves,
            self.shoot_length,
            self.internode_length,
            json.dumps(self.leaf_dimensions),
            json.dumps(self.porometer_readings),
            json.dumps(self.timestamps)
        ))
        conn.commit()

    @staticmethod
    def get_plant_by_id(plant_id, conn):
        cursor = conn.cursor()
        cursor.execute("SELECT * FROM plants WHERE plant_id = ?", (plant_id,))
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


class Leaf: #to track leaf dimensions
    def __init__(self, plant_barcode, number, selection_date, status) -> None:
        self.plant_barcode = plant_barcode
        self.number = number
        self.selection_date = selection_date
        self.status = "alive"
        self.growth = {} #{date: [width, length]}

class SL_Shoot: #to track shoot length and internode length
    def __init__(self, plant_barcode, type) -> None:
        self.plant_barcode = plant_barcode
        self.growth = {} # {date: length}

    def save_to_db(self, conn): #fix
        cursor = conn.cursor()
        cursor.execute('''
        INSERT INTO shoots (plant_id, shoot_label)
        VALUES (?, ?)
        ''', (self.plant_id, self.shoot_label))
        self.shoot_id = cursor.lastrowid
        conn.commit()

class INL_Shoot: #to track shoot length and internode length
    def __init__(self, plant_barcode, type) -> None:
        self.plant_barcode = plant_barcode
        self.growth = {} # {date: [lengths]}

    def save_to_db(self, conn): #fix
        cursor = conn.cursor()
        cursor.execute('''
        INSERT INTO internode_lengths (shoot_id, timestamp, internode_length)
        VALUES (?, ?, ?)
        ''', (self.shoot_id, self.timestamp, self.internode_length))
        self.internode_id = cursor.lastrowid
        conn.commit()


