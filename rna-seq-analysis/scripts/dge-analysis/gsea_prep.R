# gsea_prep.R
# ----------------------------
# Set your input and output file paths here:
input_csv  <- "~/Masters/rna-seq-analysis/results/deseq2/timepoints/TP3/DESeq2_results_TP3.csv"
output_rnk <- "~/Masters/rna-seq-analysis/results/gsea/timepoints/DESeq2_results_TP3_log2FC_rnk.rnk"

# ----------------------------
# 1. Read in DE results
# ----------------------------
res <- read.csv(input_csv, header = TRUE, check.names = FALSE)

# If the first column is unnamed ("...1"), rename it to "gene"
if ("...1" %in% colnames(res)) {
  colnames(res)[colnames(res) == "...1"] <- "geneID"
} else if (!"geneID" %in% colnames(res)) {
  # assume first column is gene if not explicitly named
  names(res)[1] <- "geneID"
}

# ----------------------------
# 2. Prepare ranked list
# ----------------------------
# Remove rows with missing values in 'log2FoldChange'
ranked <- res[!is.na(res$log2FoldChange), c("geneID", "log2FoldChange")]

# Ensure unique gene IDs (GSEA requires unique genes)
ranked <- ranked[!duplicated(ranked$gene), ]

# Sort by ranking metric (descending)
ranked <- ranked[order(ranked$log2FoldChange, decreasing = TRUE), ]

# ----------------------------
# 3. Save as .rnk file
# ----------------------------
write.table(
  ranked,
  file      = output_rnk,
  sep       = "\t",
  quote     = FALSE,
  row.names = FALSE,
  col.names = FALSE  # GSEA .rnk file has no header
)

cat("GSEA .rnk file written to:", output_rnk, "\n")
