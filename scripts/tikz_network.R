tikz <- function (graph, layout) {
  ## Here we get the matrix layout
  if (class(layout) == "function")
    layout <- layout(graph)
  
  layout <- layout / max(abs(layout))
  
  size=(V(graph)$size-min(V(graph)$size))/(max(V(graph)$size)-min(V(graph)$size))*1+.125 #vertex size
  width=round(((E(graph)$width-min(E(graph)$width))/(max(E(graph)$width)-min(E(graph)$width))*3+.25),2)
  col=t(col2rgb(V(graph)$color))

  ##plot scaling
  cat("\\begin{tikzpicture}[scale=15]\n")
  cat("\\SetVertexStyle[LineOpacity=.7, LineWidth=.1]\n")
  cat("\\SetEdgeStyle[Opacity=.7]\n")
  
  for (vertex in V(graph)) {
    label <- V(graph)[vertex]$name
    if (is.null(label))
      label <- ""
    label=gsub(pattern = "\\_",replacement = " ",x = label)
    vsz= size[vertex] #size of vertex
    vcol=col[vertex,]
    ##drawing vertices
    cat (sprintf ("\t\\Vertex[x=%f, y=%f, label=%s, fontscale=1.2, size=%f, RGB, color={%s}, opacity=.6, position=below]{v%d}\n", layout[vertex,1], layout[vertex,2],label,  vsz, paste(vcol, collapse = ","),  vertex))
  }
  cat("\n")
  
  edge.list=as_edgelist(graph)
  col=t(col2rgb(E(graph)$color))
  
  for(j in 1:length(E(graph))){
    vcol=col[j,]
    if (!is.directed(graph)){ ##undirected graph
      if(V(g)[edge.list[j,1]] == V(g)[edge.list[j,2]]){
        cat (sprintf ("\t\\Edge[loopsize=1.5, opacity=.5](v%s)(v%s)\n", V(graph)[edge.list[j,1]], V(graph)[edge.list[j,2]]))
      } else {
      cat (sprintf ("\t\\Edge[bend=10, RGB, color={%s}, lw=%fpt](v%s)(v%s)\n",  paste(vcol, collapse = ","), width[j], V(graph)[edge.list[j,1]], V(graph)[edge.list[j,2]]))
      }
    } else{ #directed graph
      if(V(graph)[edge.list[j,1]] == V(graph)[edge.list[j,2]]){
        cat (sprintf ("\t\\Edge[loopsize=1.5, opacity=.5](v%s)(v%s)\n", V(graph)[edge.list[j,1]], V(graph)[edge.list[j,2]]))
      } else {
    cat(sprintf ("\t\\Edge[Direct, RGB, color={%s}, lw=%fpt](v%d)(v%d)\n", paste(vcol, collapse = ","), width[j], V(graph)[edge.list[j,1]], V(graph)[edge.list[j,2]]))
      }
    }  
  }
  cat("\\end{tikzpicture}\n")
}

