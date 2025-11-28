otu_table <- read.table(file = "clipboard", sep = "\t", header=TRUE)
head(otu_table)

library(ComplexHeatmap)
library(grid)

# Identify columns
param_col   <- 1
subject_col <- ncol(otu_table)
taxa_cols   <- (param_col + 1):(subject_col - 1)

# Clean Subject: remove tabs, carriage returns, extra spaces, leading quotes
subject_raw <- as.character(otu_table[[subject_col]])

subject_clean <- subject_raw
subject_clean <- gsub("\t", " ", subject_clean, fixed = TRUE)  # remove tabs
subject_clean <- gsub("\r", " ", subject_clean, fixed = TRUE)  # remove weird CRs
subject_clean <- gsub("^\"\\s*|\\s*\"$", "", subject_clean)    # strip leading/trailing quotes
subject_clean <- trimws(subject_clean)                         # trim spaces

right_anno <- rowAnnotation(
  Subject = anno_text(
    subject_clean,
    just = "left",
    gp   = gpar(fontsize = 8)
  ),
  annotation_width = unit(4, "cm")
)

# Build matrix of taxa (presence/absence)
mat <- as.matrix(otu_table[, taxa_cols])

# Treat empty strings and NA as 0 (absent), 1 as present
mat[mat == ""] <- NA
mat[is.na(mat)] <- 0

# Make sure it's numeric
mat <- apply(mat, 2, function(x) as.numeric(as.character(x)))
mat <- as.matrix(mat)

# Row names: Param
rownames(mat) <- otu_table[[param_col]]

col_sums <- colSums(mat)                      # how many times each taxon is present
ord_cols <- order(col_sums, decreasing = TRUE)
mat_ord  <- mat[, ord_cols, drop = FALSE]

df_for_order <- as.data.frame(mat_ord)
# Order rows: for each column (in priority order), put rows with 1 above rows with 0
ord_rows <- do.call(order, lapply(df_for_order, function(x) -x))

mat_ord        <- mat_ord[ord_rows, , drop = FALSE]
subject_clean2 <- subject_clean[ord_rows]

right_anno <- rowAnnotation(
  Subject = anno_text(
    subject_clean2,
    just = "left",
    gp   = gpar(fontsize = 8)
  ),
  annotation_width = unit(4, "cm")
)

Heatmap(
  mat_ord,
  col = c("white", "black"),  # 0 → white, 1 → black (numeric scale)
  show_row_names    = TRUE,   # Y axis = Param
  show_column_names = TRUE,   # X axis = taxa
  name              = "Presence",
  cluster_rows      = FALSE,
  cluster_columns   = FALSE,
  right_annotation  = right_anno
)

