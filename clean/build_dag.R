# Load the DiagrammeR package
library(DiagrammeR)

# Create a simple graph
graph <- create_graph() %>%
    add_node(label = "This") %>% 
    add_node(label = "That") %>% 
    add_edge(from = 1, to = 2)


# Set font size for nodes
graph <- set_node_attrs(graph,
                        node_attr = "fontsize",
                        values = 4)

# Set font size for edges
graph <- set_edge_attrs(graph,
                        edge_attr = "fontsize",
                        values = c(10, 8))

# Render the graph
render_graph(graph)

