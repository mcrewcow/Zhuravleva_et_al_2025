library(igraph)
install.packages('corrr')
library(corrr)

# Load your data
file_path <- "C://Users/rodri/Downloads//Elena_taxon_matrix.csv"
data <- read.csv(file_path, row.names = 1, check.names = FALSE)

data <- as.data.frame(lapply(data, as.numeric))

# Compute the correlation matrix
cor_matrix <- cor(data, method = "pearson", use = "pairwise.complete.obs")

# Convert the correlation matrix to a long format (edge list)
cor_long <- as.data.frame(as.table(cor_matrix)) %>%
  filter(Var1 != Var2) %>%  # Remove self-correlations
  rename(node1 = Var1, node2 = Var2, weight = Freq)

# Filter edges by a correlation threshold
threshold <- 0.25
edges <- cor_long %>%
  filter(abs(weight) > threshold)

# Create a graph from the edge list
graph <- graph_from_data_frame(edges, directed = FALSE)

# Plot the graph with adjusted weights for the layout
plot(graph,
     vertex.label.cex = 0.7,
     edge.width = abs(E(graph)$weight) * 5,  # Edge width proportional to weight
     edge.color = ifelse(E(graph)$weight > 0, "blue", "red"),  # Color by sign
     vertex.size = 10,
     layout = layout_with_fr(graph, weights = abs(E(graph)$weight)))  # Absolute weights

# Extract edge information
edge_list <- as.data.frame(get.data.frame(graph, what = "edges"))

# Modify edge labels: Here, I'll use the weights as labels
# Replace this with gene identifiers if needed
E(graph)$label <- round(E(graph)$weight, 2)  # Show correlation values rounded to 2 decimals
# Compute layout with absolute weights
layout <- layout_with_kk(graph, weights = abs(E(graph)$weight))

# Plot the graph with edge labels
plot(graph,
     vertex.label.cex = 0.7,               # Size of the node labels
     edge.width = abs(E(graph)$weight) * 5,  # Width of the edges proportional to weight
     edge.color = ifelse(E(graph)$weight > 0, "blue", "red"),  # Edge color based on sign
     vertex.size = 10,                     # Size of the nodes
     layout = layout,                      # Use the fixed layout
     edge.label = round(E(graph)$weight, 2),  # Add edge labels with rounded weights
     edge.label.cex = 0.6,                 # Size of the edge labels
     edge.label.color = "black")           # Color of the edge labels
