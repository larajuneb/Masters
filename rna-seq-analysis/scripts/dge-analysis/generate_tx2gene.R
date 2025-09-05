#!/usr/bin/env Rscript

# Load libraries
library(GenomicFeatures)
library(dplyr)
library(readr)

# Path to your GFF file
gff_file <- "/home/23594292/Masters/rna-seq-analysis/refs/annotation/PN40024_5.1_on_T2T_ref_with_names.gff3"

# Create a TxDb object from GFF
txdb <- makeTxDbFromGFF(gff_file, format="gff3")

# Extract transcript-to-gene mapping
k <- keys(txdb, keytype = "TXNAME")
tx2gene <- select(txdb, keys=k, columns="GENEID", keytype="TXNAME")

# Save as CSV
write_csv(tx2gene, "/home/23594292/Masters/rna-seq-analysis/refs/tx2gene.csv")

cat("tx2gene mapping saved to /home/23594292/Masters/rna-seq-analysis/refs/tx2gene.csv\n")

