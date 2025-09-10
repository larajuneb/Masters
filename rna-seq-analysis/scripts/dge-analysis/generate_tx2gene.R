#!/usr/bin/env Rscript

# Load libraries
library(dplyr)
library(readr)

# Path to your Salmon quant files (use any one file for IDs)
quant_file <- "~/Masters/rna-seq-analysis/results/nextflow-out-final/star_salmon/CONTROL_28H_8-1_C069846/quant.sf"

# Read the quant file
quant <- read_tsv(quant_file, col_types = cols())

# Extract transcript IDs from Salmon
tx_ids <- quant$Name

# Derive gene IDs from transcript IDs by stripping the _t001 suffix
# This assumes transcripts are named like Vitvi05_01chr01g00010_t001
tx2gene <- tibble(
  TXNAME = tx_ids,
  GENEID = sub("_t\\d+$", "", tx_ids)
)

# Save tx2gene for tximport
write_csv(tx2gene, "~/Masters/rna-seq-analysis/refs/tx2gene_salmon.csv")
