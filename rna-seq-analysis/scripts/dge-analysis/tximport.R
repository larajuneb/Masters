#!/usr/bin/env Rscript

# Load libraries
library(tximport)
library(readr)
library(tibble)

# -----------------------------
# Step 1: Specify paths to quantification files
# -----------------------------

results_dir <- "~/Masters/rna-seq-analysis/results/nextflow-out-final/star_salmon"
files <- list.files(path = results_dir, 
                    pattern = "quant.sf$",   # only quant.sf files
                    recursive = TRUE,        # look into subfolders
                    full.names = TRUE)    

# Name the samples (these names will appear in your DESeq2 analysis)
names(files) <- basename(dirname(files))

# -----------------------------
# Step 2: Read in tx2gene mapping
# -----------------------------
# This CSV must be in two columns: TXNAME, GENEID
tx2gene <- read_csv("~/Masters/rna-seq-analysis/refs/tx2gene_salmon.csv")  # use your cleaned tx2gene CSV

# -----------------------------
# Step 3: Import transcript-level counts and summarize to gene level
# -----------------------------
txi <- tximport(files, 
                type = "salmon",  
                tx2gene = tx2gene,
                ignoreTxVersion = TRUE, # ignores transcript version suffixes
                countsFromAbundance = "lengthScaledTPM") 

# -----------------------------
# Step 4: Save the gene-level counts for DESeq2
# -----------------------------
saveRDS(txi, file = "~/Masters/rna-seq-analysis/refs/txi_gene_level.rds")

