elena <- read.csv("C://Users/rodri/Downloads/Elena_taxon_matrix.csv", row.names = 1)
elena <- as.matrix(elena)

otu_table <- t(elena)

library(ComplexHeatmap)
Heatmap(otu_table,
col = c("white", "black"),
show_row_names = TRUE,
show_column_names = TRUE,
name = "Presence")
