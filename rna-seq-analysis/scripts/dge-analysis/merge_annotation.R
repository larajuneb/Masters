# Load required libraries
library(dplyr)
library(tidyr)
library(readr)
library(stringr)

# Function to annotate filtered genes
annotate_genes <- function(filtered_genes_csv,
                           gff3_file,
                           go_file,
                           functional_summary_file,
                           blast2go_annot_file,
                           blast2go_tsv_file) {
  
  # 1. Read filtered genes
  genes_df <- read_csv(filtered_genes_csv, col_types = cols())
  
  # 2. Read GFF3 annotation
  gff3_df <- read_tsv(gff3_file, comment = "###", col_names = c("chr", "source", "type", 
                                                                "start", "end", "score", "strand", 
                                                                "phase", "attributes"), col_types = cols())
  # Extract gene ID and Name from attributes
  gff3_genes <- gff3_df %>% 
    filter(type == "gene") %>% 
    mutate(geneID = str_extract(attributes, "ID=[^;]+") %>% str_remove("ID="),
           geneName = ifelse(str_detect(attributes, "Name="),
                             str_extract(attributes, "Name=[^;]+") %>% str_remove("Name="),
                             NA)) %>% 
    select(geneID, geneName, chr, start, end, strand)
  
  # 3. Read GO file (GMT)
  go_lines <- read_lines(go_file)
  go_list <- lapply(go_lines, function(x) {
    parts <- str_split(x, "\\s+")[[1]]
    go_term <- parts[1]
    geneIDs <- parts[-c(1,2)] # remove GO term ID and category
    tibble(geneID = geneIDs, GO_term = go_term)
  })
  go_df <- bind_rows(go_list)
  
  # 4. Read functional summary
  func_df <- read_tsv(functional_summary_file, col_types = cols())
  
  # 5. Read Blast2GO annotation (both annot and tsv)
  blast2go_annot_df <- read_tsv(blast2go_annot_file, col_names = c("protID", "GO_ID", "Description"), col_types = cols())
  
  blast2go_tsv_df <- read_tsv(blast2go_tsv_file, col_types = cols())
  # extract GO annotations from the tsv column "Annotation GO ID"
  blast2go_tsv_df <- blast2go_tsv_df %>% 
    select(Sequence.Name, Annotation.GO.ID) %>% 
    rename(protID = Sequence.Name, GO_ID_tsv = Annotation.GO.ID)
  
  # Merge all annotations with filtered genes
  annotated_genes <- genes_df %>% 
    left_join(gff3_genes, by = c("geneID" = "geneID")) %>% 
    left_join(func_df, by = c("geneID" = "gene")) %>% 
    left_join(go_df %>% group_by(geneID) %>% summarise(GO_terms = paste(GO_term, collapse=";")), by = "geneID") %>% 
    left_join(blast2go_annot_df %>% group_by(protID) %>% summarise(B2GO_terms = paste(GO_ID, collapse=";")), 
              by = c("geneID" = "protID")) %>% 
    left_join(blast2go_tsv_df, by = c("geneID" = "protID"))
  
  return(annotated_genes)
}

# Example usage:
# annotated <- annotate_genes("filtered_genes.csv",
#                             "PN40024_5.1_on_T2T_ref_with_names.gff3",
#                             "5.1_on_T2T_ref_GO.gmt",
#                             "5.1_on_T2T_ref_functional_annotation_summary_with_coordinates.tsv",
#                             "PN40024_T2T_5.1_ref_blast2go.annot",
#                             "PN40024_T2T_5.1_ref_blast2go.tsv")
# write_csv(annotated, "annotated_genes.csv")
