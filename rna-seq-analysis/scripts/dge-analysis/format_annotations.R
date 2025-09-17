# Required packages
library(dplyr)
library(readr)
library(stringr)
library(purrr)
library(tidyr)

# Helper: normalize transcript/protein IDs back to gene-level IDs
normalize_to_gene <- function(x) {
  x %>%
    str_replace("\\.prot$", "") %>%              # drop .prot if present
    str_replace("_t\\d+.*$", "") %>%            # drop transcript/_t001 and anything after
    str_trim()
}

# Robust GMT parser: returns tibble with columns GO, GO_MF, geneID (one row per geneID)
parse_gmt <- function(gmt_file, gene_pattern = "Vitvi") {
  lines <- readr::read_lines(gmt_file)
  parsed <- map_dfr(lines, function(line) {
    line <- str_trim(line)
    if (line == "" || str_starts(line, "#")) return(tibble(GO = NA_character_, GO_MF = NA_character_, geneID = character(0)))
    
    # try tab-splitting (standard GMT)
    parts_tab <- str_split(line, "\t")[[1]]
    if (length(parts_tab) >= 3) {
      GO <- parts_tab[1]
      GO_MF <- parts_tab[2]
      geneIDs <- parts_tab[-c(1,2)] %>% str_trim() %>% discard(~ .x == "")
      return(tibble(GO = GO, GO_MF = GO_MF, geneID = geneIDs))
    }
    
    # fallback — find GO id and first gene token matching pattern (e.g. "Vitvi")
    GO <- str_extract(line, "GO:\\d+")
    pos_gene <- str_locate(line, paste0("\\b", gene_pattern))[1]
    if (!is.na(pos_gene)) {
      # GO_MF is text between GO and first gene start
      start_GO <- ifelse(is.na(GO), 1, str_locate(line, GO)[2] + 1)
      GO_MF <- str_trim(str_sub(line, start_GO, pos_gene - 1))
      genes_str <- str_sub(line, pos_gene)
      geneIDs <- str_split(str_trim(genes_str), "\\s+")[[1]] %>% str_trim() %>% discard(~ .x == "")
      return(tibble(GO = GO, GO_MF = GO_MF, geneID = geneIDs))
    }
    
    # last resort: whitespace split (may mis-handle GO_MF with spaces)
    toks <- str_split(line, "\\s+")[[1]] %>% discard(~ .x == "")
    if (length(toks) >= 3) {
      GO <- toks[1]
      GO_MF <- toks[2]
      geneIDs <- toks[-c(1,2)]
      return(tibble(GO = GO, GO_MF = GO_MF, geneID = geneIDs))
    }
    
    warning("Could not parse GMT line: ", line)
    tibble(GO = NA_character_, GO_MF = NA_character_, geneID = character(0))
  })
  
  # drop empty rows (if any)
  parsed %>% filter(!is.na(geneID) & geneID != "")
}

format_annotations <- function(go_file,
                               functional_summary_file,
                               blast2go_annot_file,
                               blast2go_tsv_file,
                               gene_pattern = "Vitvi",
                               out_dir) {
  go_file <- paste0(ref_dir, "5.1_on_T2T_ref_GO.gmt")
  # 2. parse GMT into long table (geneID per row)
  go_long <- parse_gmt(go_file, gene_pattern = gene_pattern)
  
  # 3. summarize GO per gene (IDs + descriptions)
  go_summary <- go_long %>%
    group_by(geneID) %>%
    summarise(
      GO_IDs = paste(unique(na.omit(GO)), collapse = ";"),
      GO_MF_desc = paste(unique(na.omit(GO_MF)), collapse = ";"),
      .groups = "drop"
    )
  write_csv(go_summary, file.path(out_dir, "go_summary.csv"))
  
  # remove prefixes
  go_long <- go_long %>%
    mutate(
      GO = GO,
      GO_MF = GO_MF,
      GO_MF = gsub(" ", "_", GO_MF)
    )
  
  # collapse genes by GO term
  
  go_gmt <- go_long %>%
    group_by(GO, GO_MF) %>%
    summarise(genes = paste(geneID, collapse = "\t"), .groups = "drop") %>%
    ungroup()
  
  write_csv(go_gmt, file.path(out_dir, "GO_gsea.csv"))
  
  # Create the final lines in GSEA .gmt format
  
  gmt_lines <- apply(go_gmt, 1, function(x) {
    paste(c(x["GO"], x["GO_MF"], x["genes"]), collapse = "\t")
  })
  
  # Save to file
  writeLines(gmt_lines, paste0(out_dir, "GO_gsea.gmt"))
  
  
  # 4. functional summary (clean file, pad missing columns, rename gene -> geneID)
  num_cols_expected <- 12  # set to expected number of columns in the summary file
  
  # Read raw lines
  lines <- readLines(functional_summary_file)
  
  # Split by tab and pad short rows
  split_lines <- strsplit(lines, "\t")
  clean_lines <- lapply(split_lines, function(x) {
    length(x) <- num_cols_expected
    x
  })
  
  # Recombine into a temporary tab-delimited string and read as tibble
  temp_file <- tempfile(fileext = ".tsv")
  writeLines(sapply(clean_lines, function(x) paste(x, collapse = "\t")), temp_file)
  
  func_df <- read_tsv(temp_file, col_types = cols(.default = col_character())) %>%
    rename_with(~ ifelse(. == "gene", "geneID", .), everything())
  write_csv(func_df, file.path(out_dir, "functional_summary.csv"))
  
  # 5. parse Blast2GO .annot (extract GO IDs per protein, then normalize to gene)
  b2g_lines <- readr::read_lines(blast2go_annot_file)
  b2g_parsed <- map_dfr(b2g_lines, function(line) {
    line <- str_trim(line)
    if (line == "" ) return(tibble(gene_raw = NA_character_, GO = NA_character_)[0,])
    # first token is sequence/protein name
    toks <- str_split(line, "\\s+")[[1]] %>% discard(~ .x == "")
    prot <- toks[1]
    gos <- str_extract_all(line, "GO:\\d{3,7}")[[1]]  # capture any GO:######
    if (length(gos) == 0) return(tibble(gene_raw = prot, GO = NA_character_))
    tibble(gene_raw = prot, GO = gos)
  })
  
  # normalize to gene-level IDs (drop _t001 etc and .prot)
  if (nrow(b2g_parsed) > 0) {
    b2g_parsed <- b2g_parsed %>%
      mutate(geneID = normalize_to_gene(gene_raw)) %>%
      filter(!is.na(geneID))
    b2g_summary <- b2g_parsed %>%
      filter(!is.na(GO)) %>%
      group_by(geneID) %>%
      summarise(B2GO_IDs = paste(unique(GO), collapse = ";"), .groups = "drop")
  } else {
    b2g_summary <- tibble(geneID = character(0), B2GO_IDs = character(0))
  }
  write_csv(b2g_summary, file.path(out_dir, "b2g_summary.csv"))
  
  # 6. parse Blast2GO TSV and keep the columns we want
  b2g_tsv <- read_delim(
    blast2go_tsv_file,
    delim = "\t",
    col_types = cols(.default = col_character())
  )
  
  # columns to keep
  keep_cols <- c(
    "Sequence Name",
    "Sequence Description",
    "Enzyme Code",
    "Annotation GO ID",
    "Annotation GO Count",
    "InterPro Accession",
    "InterPro GO ID"
  )
  
  # check that all exist, warn if some are missing
  missing_cols <- setdiff(keep_cols, names(b2g_tsv))
  if (length(missing_cols) > 0) {
    warning("These expected columns were not found in the Blast2GO TSV: ",
            paste(missing_cols, collapse = ", "))
  }
  
  # select the ones that exist and normalise gene ID
  b2g_tsv_sel <- b2g_tsv %>%
    select(any_of(keep_cols)) %>%
    rename_with(~ gsub(" ", "_", .x, fixed = TRUE)) %>%   # optional: remove spaces from names
    mutate(geneID = normalize_to_gene(`Sequence_Name`))
  
  # collapse multiple rows per geneID if needed
  b2g_tsv_summary <- b2g_tsv_sel %>%
    group_by(geneID) %>%
    summarise(across(everything(), ~ paste(unique(na.omit(.x)), collapse = ";")),
              .groups = "drop")
  write_csv(b2g_tsv_summary, file.path(out_dir, "b2g_tsv_summary.csv"))
  
  message("Annotation files saved to: ", normalizePath(out_dir))
}

ref_dir <- "~/Masters/rna-seq-analysis/refs/annotation/"
out_dir <- "~/Masters/rna-seq-analysis/refs/annotation/formatted/"

go_file <- paste0(ref_dir, "5.1_on_T2T_ref_GO.gmt")
functional_summary_file <- paste0(ref_dir, "5.1_on_T2T_ref_functional_annotation_summary_with_coordinates.tsv")
blast2go_annot_file <- paste0(ref_dir, "T2T_5.1_blast2go/PN40024_T2T_5.1_ref_blast2go.annot")
blast2go_tsv_file <- paste0(ref_dir, "T2T_5.1_blast2go/PN40024_T2T_5.1_ref_blast2go.tsv")

format_annotations(go_file,
                   functional_summary_file,
                   blast2go_annot_file,
                   blast2go_tsv_file,
                   "Vitvi",
                   out_dir)
