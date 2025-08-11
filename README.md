# StimBlue+ Grapevine Project: RNA-seq and Agronmic Data Analyses

## Table of Contents
1. [Overview](#overview)
2. [Repository Structure](#repository-structure)
3. [Agronomic Data Analysis](#agronomic-data-analysis)
4. [RNA-seq Data Analysis Pipeline](#rna-seq-data-analysis-pipeline)
5. [Installation and Setup](#installation-and-setup)
6. [Usage](#usage)
7. [Output & Results](#output--results)
8. [Troubleshooting](#troubleshooting)
9. [Contributing](#contributing)
10. [License](#license) 

---

## Overview
This repository contains all data, scripts, and results related to the MSc thesis project investigating the effects of the kelp-based biostimulant StimBlue+ on *Vitis vinifera* ‘Chenin blanc’ clone 220.

It includes two main components:
- **Agronomic Data Analysis**: Processing and statistical analysis of physiological and growth measurements collected weekly over 13 weeks.
- **RNA-seq Transcriptomic Analysis**: Bioinformatics pipeline for processing leaf tissue RNA-seq data, quantifying gene expression, performing differential expression analysis, and downstream functional enrichment.

---

## Repository Structure
/
├── agronomic-analysis/ # Agronomic data and analysis scripts
│ ├── data/ # Raw CSV files of agronomic measurements
│ ├── data-analysis/ # Agronomic analysis outputs and scripts
│ │ ├── plots/ # Plots of agronomic data (PDFs, PNGs, etc.)
│ │ ├── scripts/ # R scripts for analysis and plotting
│ │ ├── test_results/ # Statistical test outputs (tables, CSVs)
│ │ ├── Plots.pdf # Compiled PDF report of agronomic plots
│ │ └── week_rankings.csv # Week rankings by agronomic measure
├── rna-seq-analysis/ # RNA-seq bioinformatics pipeline and data
│ ├── data/ # Raw FASTQ files, reference genome & annotation
│ ├── results/ # QC reports, counts, DE results, enrichment
│ ├── refs/ # reference data for the Vitis vinifera genome and KEGG organism
│ ├── scripts/ # Bash and R scripts for RNA-seq processing
│ ├── slurm/ # SLURM job scripts for HPC
│ ├── envs/ # Conda environment YAML files
│ └── README.md # (Optional RNA-seq-specific README)
├── LICENSE
└── README.md # This main overview README


---

## Agronomic Data Analysis

- **Purpose:** Analyze physiological and growth measurements from grapevine treated with StimBlue+, compared to control vines, over a 13-week period.
- **Raw data:** Stored as CSV files in `agronomic-analysis/data/`. Each file corresponds to an agronomic measurement.
- **Scripts:** R scripts located in `agronomic-analysis/data-analysis/scripts/` perform statistical analyses and generate plots.
- **Results:** Statistical test outputs are in `test_results/`, visualizations in `plots/`, with a compiled PDF report `Plots.pdf`. Week-wise rankings for measurements are in `week_rankings.csv`.
- **How to reproduce:** Run R scripts sequentially in the `scripts/` folder using R or RStudio with required packages installed.

---

## RNA-seq Data Analysis Pipeline

- **Goal:** Process 18 samples of paired-end 200 bp RNA-seq data from *Vitis vinifera* leaf tissue to identify differential gene expression due to StimBlue+ treatment across three timepoints, comparing StimBlue+ treated vines to control vines across timepoints, and treatments to themselves across timepoints.
- **Pipeline steps:**
  1. Quality control and adapter trimming with `fastp` and `MultiQC`.
  2. Transcript quantification using `Salmon` (selective alignment and bias correction).
  3. Import counts and differential expression analysis with `tximport` and `DESeq2` in R.
  4. Functional enrichment analysis (GO and KEGG) using `clusterProfiler`.
- **Raw data and references:** Stored in `rna-seq-analysis/data/` (FASTQ, genome FASTA, GTF).
- **Scripts and jobs:** Pipeline scripts in `rna-seq-analysis/scripts/` and HPC job submission scripts in `rna-seq-analysis/slurm/`.
- **Environments:** Conda environment YAML files under `rna-seq-analysis/envs/` for reproducibility.
- **How to run:** See Usage section below.

---


## Installation and Setup

1. **Clone repository:**
   ```bash
   git clone https://github.com/larajuneb/Masters.git
   cd Masters
   ```

2. **Create Conda environments:**
   ```bash
   conda env create -f rna-seq-analysis/envs/rna_env.yml
   conda env create -f rna-seq-analysis/envs/r_env.yml
   ```

3. **Activate environment before running RNA-seq pipeline:**
   ```bash
   conda activate rna_env
   ```

4. **R environment for analyses and plotting:**
   ```bash
   conda activate r_env
   ```

---

## Usage

### Agronomic Data  
- Open R or RStudio.
- Navigate to `agronomic-analysis/data-analysis/scripts/`.
- Run the R scripts in order to perform data cleaning, analysis, and plotting.

### RNA-seq Pipeline  
- Prepare reference indices (if needed):
  ```bash
  bash rna-seq-analysis/scripts/00_make_indices.sh
  ```
- Submit quality control and trimming jobs:
  ```bash
  sbatch rna-seq-analysis/slurm/qc_trim.sbatch
  ```
- Run Salmon quantification:
  ```bash
  bash rna-seq-analysis/scripts/02_quant_salmon.sh
  ```
- Run differential expression and enrichment analysis in R:
  ```bash
  conda activate r_env
  Rscript rna-seq-analysis/scripts/04_deseq2_analysis.R
  ```

---

## Output & Results

- **Agronomic analysis:**
  - Statistical test results in `agronomic-analysis/data-analysis/test_results/`
  - Plots in `agronomic-analysis/data-analysis/plots/` and `Plots.pdf`
  - Week rankings in `week_rankings.csv`

- **RNA-seq analysis:**
  - Trimmed reads and QC reports in `rna-seq-analysis/results/qc/`
  - Salmon quantification results in `rna-seq-analysis/results/salmon/`
  - DESeq2 results and enrichment analysis in `rna-seq-analysis/results/deseq2/`

---

## Troubleshooting

| Issue                          | Possible Cause                         | Suggested Solution                     |
|-------------------------------|--------------------------------------|--------------------------------------|
| Conda environment errors       | Environment not created or activated | Re-run environment creation commands and activate env |
| Low Salmon mapping rates       | Mismatched reference genome or index | Verify reference files and index versions match samples |
| SLURM jobs fail or time out    | Insufficient resources or config     | Increase time and memory in `.sbatch` scripts |
| R package missing or errors    | Packages not installed in environment | Install missing packages with `conda` or `install.packages()` |

---

## Contributing

Contributions are welcome! To contribute:
1. Fork the repository
2. Create a new branch (`git checkout -b feature-name`)
3. Commit your changes (`git commit -m "Add feature"`) 
4. Push to your branch (`git push origin feature-name`)
5. Open a Pull Request here on GitHub

Please report any bugs or issues by opening GitHub issues.

---

## License

This project is licensed under the US License. See the [LICENSE](LICENSE) file for details.

---

*Developed by Lara Berry as part of MSc thesis project on Kelp Blue Biotech's kelp biostimulant, StimBlue+, on grapevine transcriptomics.*
