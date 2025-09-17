# Load libraries
library(fgsea)
library(data.table)
library(dplyr)
library(ggplot2)


# ---------------------------
# Function to run GSEA for a single rnk file
# ---------------------------
run_gsea_file <- function(rnk_file, metric_name, out_dir){
  # rnk_file <- rnk_files$stat
  # metric_name <- "stat"
  # out_dir <- "~/Masters/rna-seq-analysis/results/gsea/global_time_adjusted/stat/"
  
  
  
  if(!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)
  # Read ranked file
  de <- fread(rnk_file, header = FALSE)
  colnames(de) <- c("geneID", metric_name)
  
  # Check genes missing in GMT
  genes_missing_go <- setdiff(de$geneID, unique(unlist(gmt)))
  writeLines(genes_missing_go, file.path(out_dir, paste0("genes_missing_go_", metric_name, ".txt")))
  
  # Create ranked vector
  ranks <- setNames(de[[metric_name]], de$geneID)
  
  # Run fgsea
  set.seed(123)
  fgsea_res <- fgsea(
    pathways = gmt,
    stats = ranks,
    minSize = 15,
    maxSize = 500,
    nperm = 10000
  )
  
  fgsea_res <- fgsea_res[order(fgsea_res$padj), ]
  
  # Save results
  fwrite(fgsea_res, file.path(out_dir, paste0("GSEA_results_", metric_name, ".csv")))
  
  go_descriptions <- go_map$GO_MF[ match(fgsea_res$pathway, go_map$GO) ]
  
  fgsea_res_annot <- cbind(GO_MF = go_descriptions, fgsea_res)
  fwrite(fgsea_res_annot, file.path(out_dir, paste0("GSEA_results_", metric_name, "_annot.csv")))
  
  # -------------------------
  # Top 10 pathways plot (existing)
  # -------------------------
  top10 <- fgsea_res_annot %>% arrange(padj) %>% head(10)
  
  pdf(file.path(out_dir, paste0("GSEA_top10_", metric_name, ".pdf")), width = 8, height = 6)
  print(
    ggplot(top10,
           aes(x = reorder(pathway, NES), y = NES, fill = padj < 0.05)) +
      geom_col() +
      coord_flip() +
      geom_text(aes(x = pathway, y = 0, label = GO_MF),
                hjust = 1,
                nudge_y = -0.05,
                size = 3) +
      labs(
        title = paste("Top 10 enriched GO terms (", metric_name, ")", sep = ""),
        x = "GO ID",
        y = "Normalized Enrichment Score (NES)"
      ) +
      scale_fill_manual(values = c("grey", "steelblue"), guide = FALSE)
  )
  dev.off()
  

  # -------------------------
  # Leading edge genes
  # -------------------------
  leading_edges <- lapply(top10$pathway, function(pw){
    data.frame(pathway = pw, gene = fgsea_res$leadingEdge[[which(fgsea_res$pathway == pw)]])
  })
  leading_edges_df <- do.call(rbind, leading_edges)
  fwrite(leading_edges_df, file.path(out_dir, paste0("GSEA_leadingEdge_", metric_name, ".csv")))
  
  # -------------------------
  # NES histogram
  # -------------------------
  pdf(file.path(out_dir, paste0("GSEA_NES_distribution_", metric_name, ".pdf")), width = 8, height = 6)
  print(
    ggplot(fgsea_res, aes(x = NES)) + 
      geom_histogram(binwidth = 0.2, fill = "grey", color = "black") + 
      theme_minimal() + 
      labs(title = paste("Distribution of NES values (", metric_name, ")", sep=""), 
           x = "NES", y = "Count")
  )
  dev.off()
  
  # -------------------------
  # Positive vs Negative top pathways
  # -------------------------
  top_pos <- fgsea_res_annot %>% filter(NES > 0) %>% arrange(padj) %>% head(10)
  top_neg <- fgsea_res_annot %>% filter(NES < 0) %>% arrange(padj) %>% head(10)
  
  top_combined <- bind_rows(top_pos, top_neg) %>%
    mutate(direction = ifelse(NES > 0, "Upregulated", "Downregulated"))
  
  pdf(file.path(out_dir, paste0("GSEA_top10_PosNeg_", metric_name, ".pdf")),
      width = 16, height = 10)
  print(
    ggplot(top_combined,
           aes(x = reorder(pathway, NES), y = NES, fill = direction)) +
      geom_col() +
      coord_flip() +
      # Text labels: to the left of 0 for negative NES, to the right for positive
      geom_text(
        aes(
          label = GO_MF,
          y = ifelse(NES > 0,
                     -2,   # a bit beyond the positive bar tip
                     2)   # a bit beyond the negative bar tip
        ),
        hjust = ifelse(top_combined$NES > 0, 0, 1),  # right-justify negatives
        size = 3
      ) +
      scale_fill_manual(values = c("Upregulated" = "steelblue",
                                   "Downregulated" = "firebrick")) +
      labs(
        title = paste("Top Positive and Negative Enriched GO Pathways (", metric_name, ")", sep = ""),
        x = "GO ID",
        y = "Normalized Enrichment Score (NES)"
      ) +
      theme_minimal()
  )
  dev.off()
  
  return(fgsea_res)
}

# ---------------------------
# User inputs
# ---------------------------

gmt_file <- "~/Masters/rna-seq-analysis/refs/annotation/formatted/GO_gsea.gmt"
go_map <- fread("~/Masters/rna-seq-analysis/refs/annotation/formatted/GO_gsea.csv", header = TRUE)
gmt <- gmtPathways(gmt_file)  # read GO GMT as named list

rnk_files <- list(
  stat = "~/Masters/rna-seq-analysis/results/gsea/timepoints/TP3/stat/DESeq2_results_TP3_stat_rnk.rnk",
  log2FC = "~/Masters/rna-seq-analysis/results/gsea/timepoints/TP3/log2FC/DESeq2_results_TP3_log2FC_rnk.rnk"
)

# ---------------------------
# Run GSEA for both files
# ---------------------------
res_stat <- run_gsea_file(rnk_files$stat, "stat", "~/Masters/rna-seq-analysis/results/gsea/timepoints/TP3/stat/")
res_log2fc <- run_gsea_file(rnk_files$log2FC, "log2FC", "~/Masters/rna-seq-analysis/results/gsea/timepoints/TP3/log2FC/")
