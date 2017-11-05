library(DiagrammeR)

## Demo - DiagrammeR and Graphviz
grViz("

      digraph lifecycle {

      # Node statements
      node [shape = circle
      fontname = Helvetica
      fontsize = 11
      style = filled
      fixedsize = true
      fillcolor = '#3070BA']
      J [label = 'Juv']
      I [label = 'Im']
      A [label = 'Ad']
      D [label = 'Div']
      P [label = 'Pair']

      # Edge statements
      edge [arrowsize = 0.7]
      J->I;
      I->A;
      A->P [label = 'Pairing'
      fontsize = 9
      fontname = Helvetica];
      P->A [label = 'Mortality'
      fontsize = 9
      fontname = Helvetica];
      P->J [label = 'Recruitment'
      fontsize = 9
      fontname = Helvetica];
      fontsize = 9
      P->D [label = 'Divorce'
      fontsize = 9
      fontname = Helvetica];
      D->A [label = 'p<1'
      fontsize = 9
      fontname = Helvetica
      fontcolor = 'red'];
      J:n->J:n;
      I:n->I:n;
      D:s->D:s;
      P:s->P:s;

      # Graph statement
      graph [layout = dot]

      # Subgraph statements to group life stages
      subgraph {
      rank = same; J; I; A
      }
      subgraph {
      rank = same; P; D
      }

      } # end of digraph

      ")

## Adjustments to edges and label positions
grViz("

      digraph lifecycle {

      # Node statements
      node [shape = circle
      fontname = Helvetica
      fontsize = 11
      style = filled
      fixedsize = true
      fillcolor = '#3070BA']
      J [label = 'Juv']
      I [label = 'Im']
      A [label = 'Ad']
      D [label = 'Div']
      P [label = 'Pair']

      # Edge statements
      edge [arrowsize = 0.7]
      J->I;
      I->A;
      A:sw->P:nw [xlabel = 'Pairing '
      fontsize = 9
      fontname = Helvetica];
      P:ne->A:se [xlabel = ' Mortality'
      fontsize = 9
      fontname = Helvetica];
      P:w->J [xlabel = 'Recruitment     '
      fontsize = 9
      fontname = Helvetica];
      fontsize = 9
      P->D [label = 'Divorce'
      fontsize = 9
      fontname = Helvetica];
      D:n->A:e [label = '  p<1'
      fontsize = 9
      fontname = Helvetica
      fontcolor = 'red'];
      J:n->J:n;
      I:n->I:n;
      D:s->D:s;
      P:s->P:s;

      # Graph statement
      graph [layout = dot]

      # Subgraph statements to group life stages
      subgraph {
      rank = same; J; I; A
      }
      subgraph {
      rank = same; P; D
      }

      } # end of digraph

      ")

## Using external specification
##grViz("dot-lifecycle.gv")
