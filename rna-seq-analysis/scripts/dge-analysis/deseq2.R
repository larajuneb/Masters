#!/usr/bin/env Rscript

# -----------------------------
# Load libraries
# -----------------------------
library(DESeq2)
library(tximport)
library(readr)
library(dplyr)
library(pheatmap)
library(stringr)
library(ggplot2)

# -----------------------------
# NEW FUNCTION: filter & annotate DESeq2 results
# -----------------------------
filter_and_annotate <- function(res, gff_file, out_file) {
  # Turn DESeq2 results into dataframe
  res_df <- as.data.frame(res)
  res_df$geneID <- rownames(res_df)
  
  # Filter significant genes
  res_filt <- res_df %>%
    filter(!is.na(padj), padj < 0.05, abs(log2FoldChange) > 1)
  
  if (nrow(res_filt) == 0) {
    message("No significant genes found.")
    write.csv(res_filt, out_file, row.names = FALSE)
    return(invisible(res_filt))
  }
  
  # Read in GFF3, keep only "gene" entries
  gff <- read_tsv(
    gff_file,
    comment = "#",
    col_names = FALSE,
    col_types = cols(.default = "c")
  )
  
  gff_genes <- gff %>%
    filter(X3 == "gene") %>%
    transmute(
      geneID = str_extract(X9, "ID=[^;]+") %>% str_replace("ID=", ""),
      geneName = str_extract(X9, "Name=[^;]+") %>% str_replace("Name=", "")
    )
  
  # Join DESeq2 results with annotation
  annotated <- res_filt %>%
    left_join(gff_genes, by = "geneID") %>%
    mutate(final_gene = ifelse(is.na(geneName), geneID, geneName)) %>%
    select(final_gene, geneID, everything(), -geneName)
  
  # Write to file
  write.csv(annotated, out_file, row.names = FALSE)
  message("Filtered & annotated results written to: ", out_file)
  
  return(annotated)
}

# -----------------------------
# Step 0: Load tximport object
# -----------------------------
txi <- readRDS("~/Masters/rna-seq-analysis/refs/txi_gene_level.rds")  # previously saved txi object

# -----------------------------
# Step 1: Load sample metadata
# -----------------------------
samplesheet <- read_csv("~/Masters/rna-seq-analysis/refs/samplesheet.csv")

# Make sure the sample names match column names in txi
stopifnot(all(samplesheet$sample %in% colnames(txi$counts)))  # should return TRUE

# -----------------------------
# Global blind DESeq2 run
# -----------------------------
cat("Running global DESeq2 (all samples)\n")

meta_global <- as.data.frame(samplesheet)
meta_global$condition <- factor(meta_global$condition)
meta_global$timepoint <- factor(meta_global$timepoint)
meta_global$replicate <- factor(meta_global$replicate)

# Reorder metadata to match txi$counts
meta_global <- meta_global[match(colnames(txi$counts), meta_global$sample), ]
rownames(meta_global) <- meta_global$sample

# Subset txi (use all samples)
txi_global <- list(
  counts   = txi$counts[, meta_global$sample, drop = FALSE],
  abundance= txi$abundance[, meta_global$sample, drop = FALSE],
  length   = txi$length[, meta_global$sample, drop = FALSE],
  countsFromAbundance = txi$countsFromAbundance
)

dds_global <- DESeqDataSetFromTximport(
  txi     = txi_global,
  colData = meta_global,
  design  = ~ condition
)

dds_global <- DESeq(dds_global)
# Variance stabilising transformation (blind = TRUE)
vsd_global <- vst(dds_global, blind = TRUE)

# Calculate PCA
pca_data <- plotPCA(vsd_global, intgroup = c("condition", "timepoint"), returnData = TRUE)
percentVar <- round(100 * attr(pca_data, "percentVar"))

# Manual ggplot with fixed colors for condition, shapes for timepoint
p <- ggplot(pca_data, aes(x = PC1, y = PC2, color = condition, shape = timepoint)) +
  geom_point(size = 3) +
  xlab(paste0("PC1: ", percentVar[1], "% variance")) +
  ylab(paste0("PC2: ", percentVar[2], "% variance")) +
  theme_bw() +
  scale_color_manual(values = c("control" = "red", "stimblue" = "blue")) +
  scale_shape_manual(values = c(16, 17, 15))  # circle, triangle, square

# Save PCA plot
pdf("~/Masters/rna-seq-analysis/results/deseq2/global_blind/PCA_global.pdf")
print(p)
dev.off()

# Sample distance heatmap
sampleDists <- dist(t(assay(vsd_global)))
sampleDistMatrix <- as.matrix(sampleDists)
rownames(sampleDistMatrix) <- colnames(vsd_global)
colnames(sampleDistMatrix) <- colnames(vsd_global)

pdf("~/Masters/rna-seq-analysis/results/deseq2/global_blind/Heatmap_global.pdf")
pheatmap(sampleDistMatrix,
         clustering_distance_rows = sampleDists,
         clustering_distance_cols = sampleDists,
         annotation_col = meta_global[, c("condition", "timepoint")])
dev.off()

# MA plot for global blind DESeq2
pdf("~/Masters/rna-seq-analysis/results/deseq2/global_blind/MAplot_global.pdf")
plotMA(dds_global, ylim = c(-5, 5))
dev.off()

# Extract results for condition comparison
res_global <- results(dds_global, contrast = c("condition", "stimblue", "control"))
res_global <- res_global[order(res_global$padj), ]

# Save raw results
write.csv(as.data.frame(res_global), 
          file = "~/Masters/rna-seq-analysis/results/deseq2/global_blind/DESeq2_results_global.csv")

# Filtered & annotated results
anno_file <- "~/Masters/rna-seq-analysis/refs/annotation/PN40024_5.1_on_T2T_ref_with_names.gff3"
out_file_filtered <- "~/Masters/rna-seq-analysis/results/deseq2/global_blind/DESeq2_results_global_filtered_annotated.csv"
filter_and_annotate(res_global, anno_file, out_file_filtered)

cat("Global blind DESeq2 run complete. Results saved in 'global_blind' folder.\n")
# -----------------------------
# Global DESeq2: ~ timepoint + condition
# -----------------------------
cat("Running global DESeq2: ~ timepoint + condition\n")

dds_timeadj <- DESeqDataSetFromTximport(
  txi     = txi_global,
  colData = meta_global,
  design  = ~ timepoint + condition
)

dds_timeadj <- DESeq(dds_timeadj)
vsd_timeadj <- vst(dds_timeadj, blind = TRUE)

# PCA plot
pca_data <- plotPCA(vsd_timeadj, intgroup = c("condition", "timepoint"), returnData = TRUE)
percentVar <- round(100 * attr(pca_data, "percentVar"))
p <- ggplot(pca_data, aes(x = PC1, y = PC2, color = condition, shape = timepoint)) +
  geom_point(size = 3) +
  xlab(paste0("PC1: ", percentVar[1], "% variance")) +
  ylab(paste0("PC2: ", percentVar[2], "% variance")) +
  theme_bw() +
  scale_color_manual(values = c("control" = "red", "stimblue" = "blue")) +
  scale_shape_manual(values = c(16, 17, 15))

pdf("~/Masters/rna-seq-analysis/results/deseq2/global_time_adjusted/PCA_global.pdf")
print(p)
dev.off()

# Sample distance heatmap
sampleDists <- dist(t(assay(vsd_timeadj)))
sampleDistMatrix <- as.matrix(sampleDists)
rownames(sampleDistMatrix) <- colnames(vsd_timeadj)
colnames(sampleDistMatrix) <- colnames(vsd_timeadj)
pdf("~/Masters/rna-seq-analysis/results/deseq2/global_time_adjusted/Heatmap_global.pdf")
pheatmap(sampleDistMatrix,
         clustering_distance_rows = sampleDists,
         clustering_distance_cols = sampleDists,
         annotation_col = meta_global[, c("condition", "timepoint")])
dev.off()

# MA plot
pdf("~/Masters/rna-seq-analysis/results/deseq2/global_time_adjusted/MAplot_global.pdf")
plotMA(dds_timeadj, ylim = c(-5,5))
dev.off()

# ---- Extract all pairwise timepoint contrasts ----
time_levels <- levels(meta_global$timepoint)
for(i in 1:(length(time_levels)-1)){
  for(j in (i+1):length(time_levels)){
    contrast_name <- paste0("timepoint_", time_levels[j], "_vs_", time_levels[i])
    cat("Extracting contrast:", contrast_name, "\n")
    res_tp <- results(dds_timeadj, contrast = c("timepoint", time_levels[j], time_levels[i]))
    res_tp <- res_tp[order(res_tp$padj), ]
    
    out_csv <- paste0("~/Masters/rna-seq-analysis/results/deseq2/global_time_adjusted/DESeq2_results_timepoint_", 
                      time_levels[j], "_vs_", time_levels[i], ".csv")
    write.csv(as.data.frame(res_tp), out_csv)
    
    out_filt <- paste0("~/Masters/rna-seq-analysis/results/deseq2/global_time_adjusted/DESeq2_results_timepoint_", 
                       time_levels[j], "_vs_", time_levels[i], "_filtered_annotated.csv")
    filter_and_annotate(res_tp, anno_file, out_filt)
  }
}

# ---- Extract condition effect ----
res_cond <- results(dds_timeadj, contrast = c("condition", "stimblue", "control"))
res_cond <- res_cond[order(res_cond$padj), ]
write.csv(as.data.frame(res_cond),
          file = "~/Masters/rna-seq-analysis/results/deseq2/global_time_adjusted/DESeq2_results_condition.csv")
filter_and_annotate(res_cond, anno_file,
                    "~/Masters/rna-seq-analysis/results/deseq2/global_time_adjusted/DESeq2_results_condition_filtered_annotated.csv")

cat("Global DESeq2 ~ timepoint + condition complete.\n")

# -----------------------------
# Global DESeq2: ~ timepoint * condition (interaction)
# -----------------------------
cat("Running global DESeq2: ~ timepoint * condition (interaction)\n")

dds_interaction <- DESeqDataSetFromTximport(
  txi     = txi_global,
  colData = meta_global,
  design  = ~ timepoint * condition
)

dds_interaction <- DESeq(dds_interaction)
vsd_interaction <- vst(dds_interaction, blind = TRUE)

# PCA plot
pca_data <- plotPCA(vsd_interaction, intgroup = c("condition", "timepoint"), returnData = TRUE)
percentVar <- round(100 * attr(pca_data, "percentVar"))
p <- ggplot(pca_data, aes(x = PC1, y = PC2, color = condition, shape = timepoint)) +
  geom_point(size = 3) +
  xlab(paste0("PC1: ", percentVar[1], "% variance")) +
  ylab(paste0("PC2: ", percentVar[2], "% variance")) +
  theme_bw() +
  scale_color_manual(values = c("control" = "red", "stimblue" = "blue")) +
  scale_shape_manual(values = c(16, 17, 15))

pdf("~/Masters/rna-seq-analysis/results/deseq2/global_interaction/PCA_global.pdf")
print(p)
dev.off()

# Sample distance heatmap
sampleDists <- dist(t(assay(vsd_interaction)))
sampleDistMatrix <- as.matrix(sampleDists)
rownames(sampleDistMatrix) <- colnames(vsd_interaction)
colnames(sampleDistMatrix) <- colnames(vsd_interaction)
pdf("~/Masters/rna-seq-analysis/results/deseq2/global_interaction/Heatmap_global.pdf")
pheatmap(sampleDistMatrix,
         clustering_distance_rows = sampleDists,
         clustering_distance_cols = sampleDists,
         annotation_col = meta_global[, c("condition", "timepoint")])
dev.off()

# MA plot
pdf("~/Masters/rna-seq-analysis/results/deseq2/global_interaction/MAplot_global.pdf")
plotMA(dds_interaction, ylim = c(-5,5))
dev.off()

# ---- Loop over all interaction contrasts: stimblue vs control at each timepoint ----
for(tp in time_levels){
  # Construct the interaction name dynamically
  interaction_name <- paste0("condition_stimblue_vs_control.timepoint", tp)
  
  if(interaction_name %in% resultsNames(dds_interaction)){
    cat("Extracting interaction: stimblue vs control at timepoint", tp, "\n")
    res_int <- results(dds_interaction, name = interaction_name)
    res_int <- res_int[order(res_int$padj), ]
    
    out_csv <- paste0("~/Masters/rna-seq-analysis/results/deseq2/global_interaction/DESeq2_results_interaction_timepoint_", tp, ".csv")
    write.csv(as.data.frame(res_int), out_csv)
    
    out_filt <- paste0("~/Masters/rna-seq-analysis/results/deseq2/global_interaction/DESeq2_results_interaction_timepoint_", tp, "_filtered_annotated.csv")
    filter_and_annotate(res_int, anno_file, out_filt)
    
  } else {
    warning("Interaction term not found for timepoint ", tp)
  }
}

cat("Global DESeq2 ~ timepoint * condition complete.\n")



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
  
  # Subset txi to only include samples from this timepoint
  txi_tp <- list(
    counts   = txi$counts[, meta_tp$sample, drop = FALSE],
    abundance= txi$abundance[, meta_tp$sample, drop = FALSE],
    length   = txi$length[, meta_tp$sample, drop = FALSE],
    countsFromAbundance = txi$countsFromAbundance
  )
  
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
  
  # Create directories
  outdir_true  <- paste0("~/Masters/rna-seq-analysis/results/deseq2/timepoints/TP", tp, "/blind_true/")
  outdir_false <- paste0("~/Masters/rna-seq-analysis/results/deseq2/timepoints/TP", tp, "/blind_false/")
  
  saveRDS(dds, file.path(outdir_true,  paste0("DESeq2_dds_TP", tp, ".rds")))
  saveRDS(dds, file.path(outdir_false, paste0("DESeq2_dds_TP", tp, ".rds")))
  
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
  
  #PCA plots
  pdf(file.path(outdir_true, "PCA.pdf"))
  print(plotPCA(vsd_bt, intgroup = "condition"))
  dev.off()
  
  pdf(file.path(outdir_false, "PCA.pdf"))
  print(plotPCA(vsd_bf, intgroup = "condition"))
  dev.off()
  
  # Sample distance heatmap
  # Calculate sample distances from VST data
  sampleDists <- dist(t(assay(vsd_bt)))
  sampleDistMatrix <- as.matrix(sampleDists)   # full numeric matrix
  rownames(sampleDistMatrix) <- colnames(vsd_bt)
  colnames(sampleDistMatrix) <- colnames(vsd_bt)
  
  pdf(file.path(outdir_true, "Heatmap.pdf"))
  pheatmap(sampleDistMatrix,
           clustering_distance_rows = sampleDists,
           clustering_distance_cols = sampleDists,
           annotation_col = meta_tp[, c("condition", "timepoint", "replicate")])
  dev.off()
  
  # Calculate sample distances from VST data
  sampleDists <- dist(t(assay(vsd_bf)))
  sampleDistMatrix <- as.matrix(sampleDists)   # full numeric matrix
  rownames(sampleDistMatrix) <- colnames(vsd_bf)
  colnames(sampleDistMatrix) <- colnames(vsd_bf)
  
  pdf(file.path(outdir_false, "Heatmap.pdf"))
  pheatmap(sampleDistMatrix,
           clustering_distance_rows = sampleDists,
           clustering_distance_cols = sampleDists,
           annotation_col = meta_tp[, c("condition", "timepoint", "replicate")])
  dev.off()
  
  # MA plot
  pdf(file.path(outdir_true, "MAplot.pdf"))
  plotMA(dds, ylim = c(-5, 5))
  dev.off()
  pdf(file.path(outdir_false, "MAplot.pdf"))
  plotMA(dds, ylim = c(-5, 5))
  dev.off()
  
  # -------------------------------
  # Step 3: Extract Results
  # -------------------------------
  
  # Extract results for condition comparison
  res <- results(dds, contrast = c("condition", "stimblue", "control"))
  
  # Order by adjusted p-value
  res <- res[order(res$padj), ]
  write.csv(as.data.frame(res), file.path(outdir_true,  paste0("DESeq2_results_TP", tp, ".csv")))
  write.csv(as.data.frame(res), file.path(outdir_false, paste0("DESeq2_results_TP", tp, ".csv")))
  
  filter_and_annotate(res, anno_file,
                      file.path(outdir_true,  paste0("DESeq2_results_TP", tp, "_filtered_annotated.csv")))
  filter_and_annotate(res, anno_file,
                      file.path(outdir_false, paste0("DESeq2_results_TP", tp, "_filtered_annotated.csv")))
  
  cat("Saved DESeq2 results for timepoint:", tp, "\n\n")
}
