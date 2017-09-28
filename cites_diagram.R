library(dplyr)
library(DiagrammeR)
library(RColorBrewer)

citesData <- read.csv("cites_compdata.csv", stringsAsFactors = FALSE)

citesDataOrigin <- filter(citesData, Origin != "")

glimpse(citesData)

importers <- citesData %>% select(Importer) %>% unique() %>% unlist(use.names = FALSE)
exporters <- citesData %>% select(Exporter) %>% unique() %>% unlist(use.names = FALSE)
originators <- citesDataOrigin %>% select(Origin) %>% unique() %>% unlist(use.names = FALSE)

node_names <- unique(c(importers,exporters,originators))

importer_nodes <-
    create_node_df(
        n = length(unique(citesData$Importer)),
        type = unique(citesData$Importer),
        label = unique(citesData$Importer),
        role = "Importer",
        style = "filled",
        color = "aqua")

exporter_nodes <-
    create_node_df(
        n = length(unique(citesData$Exporter)),
        type = unique(citesData$Exporter),
        label = unique(citesData$Exporter),
        role = "Exporter",
        style = "filled",
        color = "darkturquoise")

origin_nodes <-
    create_node_df(
        n = length(unique(citesDataOrigin$Origin)),
        type = unique(citesDataOrigin$Origin),
        label = unique(citesDataOrigin$Origin),
        role = "Origin",
        style = "filled",
        color = "thistle")

# nodes <- combine_ndfs(exporter_nodes,
#                       importer_nodes,
#                       origin_nodes)
nodes <- create_node_df(
    n = length(node_names),
    type = node_names,
    label = node_names
)

pal <- brewer.pal(n = 8, "Dark2")

edges_e <-
    create_edge_df(
        from = nodes$id[match(citesData$Exporter, nodes$type)],
        to = nodes$id[match(citesData$Importer, nodes$type)],
        rel = "export",
        data = citesData$Exporter.reported.quantity,
        penwidth = ifelse(is.na(citesData$Exporter.reported.quantity), 1,
                          log(citesData$Exporter.reported.quantity+1)),
        color = pal[match(citesData$Term, unique(citesData$Term))]
    )

edges_i <-
    create_edge_df(
        from = nodes$id[match(citesData$Exporter, nodes$type)],
        to = nodes$id[match(citesData$Importer, nodes$type)],
        rel = "import",
        data = citesData$Importer.reported.quantity,
        penwidth = ifelse(is.na(citesData$Importer.reported.quantity), 1,
                          log(citesData$Importer.reported.quantity+1)),
        color = pal[match(citesData$Term, unique(citesData$Term))]
    )

edges_o <-
    create_edge_df(
        from = nodes$id[match(citesDataOrigin$Origin, nodes$type)],
        to = nodes$id[match(citesDataOrigin$Exporter, nodes$type)],
        rel = "origin",
        color = "darkorchid",
        penwidth = 1,
        style = "dashed"
    )

edges <- combine_edfs(edges_e,
                      edges_i,
                      edges_o)

cites_graph <- create_graph(nodes_df = nodes,
                            edges_df = edges
                            )

render_graph(cites_graph,
             title = "Trade in Psittaciformes 1996-2016",layout = "nicely"
             )


cites_graph %>%
    select_edges(conditions = rel == "import") %>%
    render_graph()

## Next: can we combine edges for trade between same country?
