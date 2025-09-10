#!/usr/bin/env Rscript

# -----------------------------
# Load libraries
# -----------------------------
library(DESeq2)
library(tximport)
library(readr)
library(dplyr)
library(pheatmap)

# -----------------------------
# Step 0: Load tximport object
# -----------------------------
txi <- readRDS("~/Masters/rna-seq-analysis/refs/txi_gene_level.rds")  # previously saved txi object

# -----------------------------
# Step 1: Load sample metadata
# -----------------------------
samplesheet <- read_csv("~/Masters/rna-seq-analysis/refs/samplesheet.csv")

# Make sure the sample names match column names in txi
all(samplesheet$sample %in% colnames(txi$counts))  # should return TRUE

# -----------------------------
# Step 2: Loop over timepoints for per-timepoint DESeq2
# -----------------------------
timepoints <- unique(samplesheet$timepoint)

for (tp in timepoints) {
  cat("Running DESeq2 for timepoint:", tp, "\n")
  
  # Subset to one timepoint & convert to plain data.frame
  meta_tp <- as.data.frame(samplesheet[samplesheet$timepoint == tp, ])
  
  # Convert relevant columns to factors
  meta_tp$condition <- factor(meta_tp$condition)
  meta_tp$timepoint <- factor(meta_tp$timepoint)
  meta_tp$replicate <- factor(meta_tp$replicate)
  
  # Set rownames to sample IDs
  rownames(meta_tp) <- meta_tp$sample
  
  # Annotation table for pheatmap
  annotation_col <- meta_tp[, c("condition", "timepoint", "replicate")]
  
  # Subset txi to only include samples from this timepoint
  txi_tp <- list(
    counts   = txi$counts[, meta_tp$sample, drop = FALSE],
    abundance= txi$abundance[, meta_tp$sample, drop = FALSE],
    length   = txi$length[, meta_tp$sample, drop = FALSE],
    countsFromAbundance = txi$countsFromAbundance
  )
  
  # Make condition a factor (avoid character warning)
  meta_tp$condition <- factor(meta_tp$condition)
  
  # Create DESeq2 dataset
  dds <- DESeqDataSetFromTximport(
    txi     = txi_tp,
    colData = meta_tp,
    design  = ~ condition
  )
  
  # Optional: filter low-count genes
  # keep <- rowSums(counts(dds)) >= 10
  # dds <- dds[keep,]
  
  # -------------------------------
  # Step 1: Run DESeq2
  # -------------------------------
  dds <- DESeq(dds)
  # Run DESeq2
  dds <- DESeq(dds)
  
  rownames(meta_tp) <- meta_tp$sample
  
  # Save the DESeq2 object (so you can reload without re-running)
  saveRDS(dds, paste0("~/Masters/rna-seq-analysis/results/deseq2/DESeq2_dds_", tp, ".rds"))
  
  # -------------------------------
  # Step 2: QC & Visualisation
  # -------------------------------
  vsd_bt <- vst(dds, blind = TRUE)  
  vsd_bf <- vst(dds, blind = FALSE)  # variance stabilisation
  
  meta_tp_factor <- meta_tp %>%
    mutate(
      timepoint = as.factor(timepoint),
      replicate = as.factor(replicate)
    )
  
  annotation_col <- meta_tp_factor[, c("condition", "timepoint", "replicate")]
  rownames(annotation_col) <- rownames(meta_tp_factor)
  
  # PCA plot
  pdf(paste0("~/Masters/rna-seq-analysis/results/deseq2/blind_true/PCA_", tp, ".pdf"))
  print(plotPCA(vsd_bt, intgroup = "condition"))
  dev.off()
  
  pdf(paste0("~/Masters/rna-seq-analysis/results/deseq2/blind_false/PCA_", tp, ".pdf"))
  print(plotPCA(vsd_bf, intgroup = "condition"))
  dev.off()
  
  # Sample distance heatmap
  # Calculate sample distances from VST data
  sampleDists <- dist(t(assay(vsd_bt)))
  sampleDistMatrix <- as.matrix(sampleDists)   # full numeric matrix
  rownames(sampleDistMatrix) <- colnames(vsd_bt)
  colnames(sampleDistMatrix) <- colnames(vsd_bt)
  
  # Make heatmap
  pdf(paste0("~/Masters/rna-seq-analysis/results/deseq2/blind_true/Heatmap_", tp, ".pdf"))
  pheatmap(sampleDistMatrix,
           clustering_distance_rows = sampleDists,
           clustering_distance_cols = sampleDists,
           annotation_col = annotation_col)
  dev.off()
  
  # Calculate sample distances from VST data
  sampleDists <- dist(t(assay(vsd_bf)))
  sampleDistMatrix <- as.matrix(sampleDists)   # full numeric matrix
  rownames(sampleDistMatrix) <- colnames(vsd_bf)
  colnames(sampleDistMatrix) <- colnames(vsd_bf)
  
  # Make heatmap
  pdf(paste0("~/Masters/rna-seq-analysis/results/deseq2/blind_false/Heatmap_", tp, ".pdf"))
  pheatmap(sampleDistMatrix,
           clustering_distance_rows = sampleDists,
           clustering_distance_cols = sampleDists,
           annotation_col = annotation_col)
  dev.off()
  
  # MA plot
  pdf(paste0("~/Masters/rna-seq-analysis/results/deseq2/MAplot_", tp, ".pdf"))
  plotMA(dds, ylim=c(-5,5))
  dev.off()
  
  # -------------------------------
  # Step 3: Extract Results
  # -------------------------------
  
  # Extract results for condition comparison
  res <- results(dds, contrast = c("condition", "stimblue", "control"))
  
  # Order by adjusted p-value
  res <- res[order(res$padj), ]
  
  # Save results for this timepoint
  out_file <- paste0("~/Masters/rna-seq-analysis/results/deseq2/DESeq2_results_", tp, ".csv")
  write.csv(as.data.frame(res), file = out_file)
  
  cat("Saved DESeq2 results for timepoint:", tp, "to", out_file, "\n\n")
}
