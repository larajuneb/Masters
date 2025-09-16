# Required packages
library(dplyr)
library(readr)
library(stringr)
library(purrr)
library(tidyr)

# Main annotation function
annotate_genes <- function(filtered_genes_csv,
                           go_file,
                           functional_summary_file,
                           blast2go_annot_file,
                           blast2go_tsv_file) {
  
  # 1. read filtered DEGs (expects 'geneID' column)
  genes_df <- read_csv(filtered_genes_csv, col_types = cols())
  
  # 7. combine everything (joins on geneID)
  annotated <- genes_df %>%
    left_join(func_df,   by = "geneID") %>%
    left_join(go_summary, by = "geneID") %>%
    left_join(b2g_summary, by = "geneID") %>%
    left_join(b2g_tsv_summary, by = "geneID")
  
  # 8. tidy: convert empty strings to NA
  annotated <- annotated %>%
    mutate(across(where(is.character), ~ na_if(.x, "")))
  
  annotated
}

# Example usage:
dir <- "~/Masters/rna-seq-analysis/results/deseq2/timepoints/TP1/"
filtered_genes_csv <- paste0(dir, "DESeq2_results_TP1_filtered_annotated.csv")
out_file <- paste0(dir, "annot/log2FC_1_")
ref_dir <- "~/Masters/rna-seq-analysis/refs/annotation/formatted/" #path for formatted annotations

go_file <- paste0(ref_dir, "go_summary.csv")
functional_summary_file <- paste0(ref_dir, "functional_summary.csv")
blast2go_annot_file <- paste0(ref_dir, "b2g_summary.csv")
blast2go_tsv_file <- paste0(ref_dir, "b2g_tsv_summary.csv")

annotated <- annotate_genes(filtered_genes_csv,
                            go_file,
                            functional_summary_file,
                            blast2go_annot_file,
                            blast2go_tsv_file)
readr::write_csv(annotated, paste0(out_file, "_annotated_genes.csv"))

