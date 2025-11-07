library(ggtern)
library(ggplot2)
data_matrix  <- read.csv("C://Users/rodri/Downloads/Elena_taxon_matrix_simplified.csv", row.names = 1)
data_matrix <- t(data_matrix)
ternary_data <- data_matrix[, colnames(data_matrix)]

# Normalize the data so that each row sums to 1
ternary_data <- as.data.frame(apply(ternary_data, 1, function(x) x / sum(x)))
ternary_data <- t(ternary_data)
colnames(ternary_data) <- c("FeS", "conductivepilins_archaellins", "MHCs")

ternary_data <- as.data.frame(ternary_data)
ternary_data$Labels <- rownames(ternary_data)  # Use row names as labels

ggtern(data = ternary_data, aes(x = FeS, y = conductivepilins_archaellins, z = MHCs, label = Labels)) +
  geom_point(size = 5, color = "blue") +
  geom_text(size = 3, vjust = -0.5) +  # Adjust size and position of text labels
  labs(title = "Ternary Plot of Selected Variables",
       T = "FeS", L = "conductive pilins/archaellins", R = "c-type multihemes (MHCs)") +
  theme_bw() +
  theme_showarrows()
