library(dplyr)
library(DiagrammeR)
library(RColorBrewer)

## Colour palette
pal <- brewer.pal(n = 8, "Dark2")

## CITES trade statistics derived from the CITES Trade Database, UNEP World Conservation Monitoring Centre, Cambridge, UK.
citesData <- read.csv("data/comptab_Amazona_barbadensis_1975-2017.csv", stringsAsFactors = FALSE)
desc <- "Trade in Amazona barbadensis 1975-2017"

glimpse(citesData)

## Origin is blank if the country of export is the country of origin, or if the country of origin is not reported
citesDataOrigin <- filter(citesData, Origin != "")

## Get nodes for each trade role
importers <- citesData %>% select(Importer) %>% unique() %>% unlist(use.names = FALSE)
exporters <- citesData %>% select(Exporter) %>% unique() %>% unlist(use.names = FALSE)
originators <- citesDataOrigin %>% select(Origin) %>% unique() %>% unlist(use.names = FALSE)

## Create importer nodes
importer_nodes <-
    create_node_df(
        n = length(unique(citesData$Importer)),
        type = "Importer",
        label = unique(citesData$Importer),
        style = "filled",
        color = pal[1])

## Create exporter nodes
exporter_nodes <-
    create_node_df(
        n = length(unique(citesData$Exporter)),
        type = "Exporter",
        label = unique(citesData$Exporter),
        style = "filled",
        color = pal[2])

## Create origin nodes
origin_nodes <-
    create_node_df(
        n = length(unique(citesDataOrigin$Origin)),
        type = "Origin",
        label = unique(citesDataOrigin$Origin),
        style = "filled",
        color = pal[3])

nodes <- combine_ndfs(exporter_nodes,
                     importer_nodes,
                     origin_nodes)

## Simpler approach: create node data frame for all unique nodes
node_names <- unique(c(importers,exporters,originators))

nodes <- create_node_df(
    n = length(node_names),
    type = node_names,
    label = node_names,
    ## Additional attributes: trade roles
    exporter = node_names %in% citesData$Exporter,
    importer = node_names %in% citesData$Importer,
    origin = node_names %in% citesData$Origin
)

edges_e <-
    create_edge_df(
        from = nodes$id[match(citesData$Exporter, nodes$type)],
        to = nodes$id[match(citesData$Importer, nodes$type)],
        rel = "export",
        data = citesData$Exporter.reported.quantity,
        penwidth = ifelse(is.na(citesData$Exporter.reported.quantity), 1,
                          log(citesData$Exporter.reported.quantity+1)),
        color = pal[1]
    )

edges_i <-
    create_edge_df(
        from = nodes$id[match(citesData$Exporter, nodes$type)],
        to = nodes$id[match(citesData$Importer, nodes$type)],
        rel = "import",
        data = citesData$Importer.reported.quantity,
        penwidth = ifelse(is.na(citesData$Importer.reported.quantity), 1,
                          log(citesData$Importer.reported.quantity+1)),
        color = pal[2]
    )

edges_o <-
    create_edge_df(
        from = nodes$id[match(citesDataOrigin$Origin, nodes$type)],
        to = nodes$id[match(citesDataOrigin$Exporter, nodes$type)],
        rel = "origin",
        color = pal[3],
        penwidth = 1,
        style = "dashed"
    )

edges <- combine_edfs(edges_e,
                      edges_i)

cites_graph <- create_graph(nodes_df = nodes,
                            edges_df = edges
                            )

## We get a crazy graph because there are many edges between some node pairs.
render_graph(cites_graph,
             title = desc,
             layout = "nicely"
             )

## Filter to only reported imports FIXME
cites_graph %>%
    select_edges(conditions = rel == "import") %>%
    render_graph()

## To make the diagram more manageable, combine edges for trade between same countries (taking the maximum reported quantity)
exportSummary <- citesData %>%
  group_by(Exporter, Importer) %>%
  summarise(quantity = sum(max(Importer.reported.quantity, Exporter.reported.quantity, na.rm = TRUE)))

## Create edges for trade summary
## (N.B. It would be fun to calculate the distance between countries and set the edge lengths accordingly but for the moment I'll leave it to select default values.)
edges_e2 <-
  create_edge_df(
    from = nodes$id[match(exportSummary$Exporter, nodes$type)],
    to = nodes$id[match(exportSummary$Importer, nodes$type)],
    rel = "export",
    data = exportSummary$quantity,
    penwidth = log(exportSummary$quantity+1)
    )

cites_graph2 <- create_graph(nodes_df = nodes,
                             edges_df = edges_e2
)

render_graph(cites_graph2,
             title = desc,
             layout = "nicely"  # options are: nicely, circle, tree, kk, and fr
)

## TODO
## Node size == connectedness (proportional to log(num direct trade links))
## Node colour - geopolitical or other?
## Arrow colour - source (e.g. captive-bred vs wild)? Would require alternative summary with grouping by source.
## Year by year animation or split into pre- and post-CITES listing (1981)?
## Remove XX node to avoid confusion
