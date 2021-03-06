library(DiagrammeR)
grViz("
graph phylo {
  node [fontname='Arial', shape = point, width = 0];
  nullAnc;
  null1;
  null2;
  null3;

  node [fontname='Arial']
  A [shape = diamond];
  B [shape = hexagon];
  C [shape = square];
  D [shape = square, style = 'rounded'];
  E [shape = circle];

  ## Edges
  nullAnc -- null1 -- A;
  null1 -- B;
  nullAnc -- null2 -- E;
  null2 -- null3 -- C;
  null3 -- D;

  subgraph {
    rank = same; A; B; C; D; E
  }
}
")
