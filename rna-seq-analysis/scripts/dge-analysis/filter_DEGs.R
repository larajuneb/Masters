# -----------------------------
# Load libraries
# -----------------------------
library(readr)
library(dplyr)
library(stringr)

# -----------------------------
# Function: filter & annotate DESeq2 CSV results
# -----------------------------
filter_and_annotate_csv <- function(csv_file, anno_file, out_file, padj_threshold, log2FC_threshold) {
  
  # Read CSV
  res_df <- read_csv(csv_file, col_types = cols())
  
  # Ensure first column is geneID
  if (colnames(res_df)[1] == "...1") {
    colnames(res_df)[1] <- "geneID"
  }
  
  # Ensure geneID column exists
  if (!"geneID" %in% colnames(res_df)) {
    # Assume first column is geneID
    res_df <- res_df %>%
      rename(geneID = 1)
  }
  
  # Filter significant genes
  res_filt <- res_df %>%
    filter(!is.na(padj),
           padj < padj_threshold,
           abs(log2FoldChange) > log2FC_threshold)
  
  if (nrow(res_filt) == 0) {
    message("No significant genes found.")
    write_csv(res_filt, out_file)
    return(invisible(res_filt))
  }
  
  # Read in GFF3, keep only "gene" entries
  gff <- read_tsv(anno_file, comment = "#", col_names = FALSE, col_types = cols(.default = "c"))
  
  gff_genes <- gff %>%
    filter(X3 == "gene") %>%
    transmute(
      geneID   = str_extract(X9, "ID=[^;]+") %>% str_replace("ID=", ""),
      geneName = str_extract(X9, "Name=[^;]+") %>% str_replace("Name=", "")
    )
  
  # Join DESeq2 results with annotation
  annotated <- res_filt %>%
    left_join(gff_genes, by = "geneID") %>%
    mutate(final_gene = ifelse(is.na(geneName), geneID, geneName)) %>%
    select(final_gene, geneID, everything(), -geneName)
  
  # Write to file
  write_csv(annotated, out_file)
  message("Filtered & annotated results written to: ", out_file)
  
  return(annotated)
}

# -----------------------------
# Example usage
# -----------------------------
padj_threshold <- 0.05
log2FC_threshold <- 0.5

csv_file   <- "~/Masters/rna-seq-analysis/results/deseq2/timepoints/TP1/DESeq2_results_TP1.csv"
anno_file  <- "~/Masters/rna-seq-analysis/refs/annotation/PN40024_5.1_on_T2T_ref_with_names.gff3"
out_file   <- "~/Masters/rna-seq-analysis/results/deseq2/timepoints/TP1/DESeq2_results_TP1_filtered_annotated_0.5.csv"

filtered <- filter_and_annotate_csv(csv_file, anno_file, out_file, padj_threshold, log2FC_threshold)
