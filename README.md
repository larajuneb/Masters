This repository contains all code developed in order to analyse all data collected throughout the course of the Masters thesis.
The Plant object respresents a plant in the experiment, with all attributes populated based on the data collected. This will hopefully all be based off of a database that will be developed to organise all data in a logical fasion.
The data includes agronomic readings, porometer readings, and eventually will include gene expression data.

# Plant Data Management Project

## Overview
This project manages plant data for experiments using a SQLite database. The Python scripts handle data ingestion, querying, and analysis.

## Folder Structure
- **data/**: Contains the SQLite database file.
- **scripts/**: Includes setup and data ingestion scripts.
- **plant.py**: Defines the `Plant` class.
- **analysis.py**: Provides tools for analyzing plant data.

## How to Run
1. Set up the database:
   ```bash
   python scripts/database_setup.py
2. Load data from spreadsheets:
    python scripts/data_ingestion.py
3. Analyze data:
    python analysis.py