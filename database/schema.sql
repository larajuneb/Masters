-- Plants table
CREATE TABLE IF NOT EXISTS plants (
    id INTEGER PRIMARY KEY,
    treatment TEXT NOT NULL,
    block CHAR NOT NULL,
    status TEXT NOT NULL,
    lateral_shoots INTEGER,
    dry_weight NUMERIC(5,2)
);

-- Leaf instances table (tracks selected leaves)
CREATE TABLE IF NOT EXISTS leaf_instances (
    id INTEGER PRIMARY KEY AUTOINCREMENT,
    plant_id INTEGER NOT NULL,
    leaf_label INTEGER NOT NULL,
    instance INTEGER NOT NULL,
    date_added DATE NOT NULL,
    status TEXT NOT NULL,
    FOREIGN KEY (plant_id) REFERENCES plants(id)
);

-- Leaf measurements table
CREATE TABLE IF NOT EXISTS leaf_measurements (
    id INTEGER PRIMARY KEY AUTOINCREMENT,
    leaf_id INTEGER NOT NULL,
    length NUMERIC(5,2) NOT NULL,
    width NUMERIC(5,2) NOT NULL,
    date DATE NOT NULL,
    FOREIGN KEY (leaf_id) REFERENCES leaf_instances(id)
);

-- Shoots table (tracks shoot instances over time), FOR SHOOT LENGTHS AND INL SHOOTS, type is LENGTH or INL
CREATE TABLE IF NOT EXISTS shoot_instances (
    id INTEGER PRIMARY KEY AUTOINCREMENT,
    plant_id INTEGER NOT NULL,
    type TEXT NOT NULL, 
    instance INTEGER NOT NULL,
    date_added DATE NOT NULL,
    status TEXT NOT NULL,
    FOREIGN KEY (plant_id) REFERENCES plants(id)
);

-- Internode measurements table
CREATE TABLE IF NOT EXISTS internode_measurements (
    id INTEGER PRIMARY KEY AUTOINCREMENT,
    shoot_instance_id INTEGER NOT NULL,
    internode_index INTEGER NOT NULL,
    internode_length NUMERIC(5,2) NOT NULL,
    date DATE NOT NULL,
    FOREIGN KEY (shoot_instance_id) REFERENCES shoot_instances(id)
);

-- Shoot lengths measurements table
CREATE TABLE IF NOT EXISTS shoot_lengths (
    id INTEGER PRIMARY KEY AUTOINCREMENT,
    shoot_instance_id INTEGER NOT NULL,
    length NUMERIC(5,2) NOT NULL,
    date DATE NOT NULL,
    FOREIGN KEY (shoot_instance_id) REFERENCES shoot_instances(id)
);

-- Leaf numbers table (tracks # of leaves per plant)
CREATE TABLE IF NOT EXISTS leaf_counts (
    id INTEGER PRIMARY KEY AUTOINCREMENT,
    plant_id INTEGER NOT NULL,
    num_leaves INTEGER NOT NULL,
    date DATE NOT NULL,
    FOREIGN KEY (plant_id) REFERENCES plants(id)
);

-- Stem diameters table (tracks # of leaves per plant)
CREATE TABLE IF NOT EXISTS stem_diameters (
    id INTEGER PRIMARY KEY AUTOINCREMENT,
    plant_id INTEGER NOT NULL,
    stem_diameter NUMERIC(5,2) NOT NULL,
    date DATE NOT NULL,
    FOREIGN KEY (plant_id) REFERENCES plants(id)
);