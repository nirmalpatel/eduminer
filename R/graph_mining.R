#' @import dplyr
#' @importFrom magrittr %>%
#' @export
trace_adjmatrix <- function(x) {
  
  stopifnot(x %hascols% c("caseid", "activity", "completeTime"))
  
  activity_alphabet <- sort(unique(x[["activity"]]))  
  n_activities <- n_distinct(x[["activity"]])
  
  # Trace adjacency matrix
  adjmat <- matrix(rep(0, n_activities ^ 2), nrow = n_activities, ncol = n_activities)
  
  rownames(adjmat) <- activity_alphabet
  colnames(adjmat) <- activity_alphabet
  
  caseids <- unique(x[["caseid"]])
  
  for (curcaseid in caseids) {
    
    case_data <- x %>%
      filter_(~ caseid == curcaseid) %>%
      arrange_(~ completeTime)
    
    case_path <- case_data[["activity"]]
    
    path_length <- length(case_path)
    
    if (path_length > 1) {
      
      for (i in 2:path_length) {
        adjmat[case_path[i - 1], case_path[i]] <- adjmat[case_path[i - 1], case_path[i]] + 1
      }  
    }  
  }
  
  adjmat
}

#' @import dplyr
#' @importFrom magrittr %>%
#' @importFrom lubridate days
#' @export
add_start_end <- function(x, start_col = "START", end_col = "END") {
  stopifnot(x %hascols% c("caseid", "activity", "completeTime"))
  
  bind_rows(lapply(split(x, x[["caseid"]]), function(case_data) {
    caseid <- unique(case_data[["caseid"]])[[1]]
    
    start_df <- tibble(caseid = caseid,
                       activity = start_col,
                       completeTime = min(case_data[["completeTime"]]) - days())
    
    end_df <- tibble(caseid = caseid,
                     activity = end_col,
                     completeTime = min(case_data[["completeTime"]]) + days())
    
    bind_rows(start_df, case_data, end_df)
  }))
}

#' @importFrom igraph graph.adjacency vcount V get.edges ecount E
#' @importFrom rgexf write.gexf
#' @importFrom stringr str_replace_all
#' @export
export_trace_adjmatrix <- function(x, file) {
  
  pathway_graph <- graph.adjacency(x, mode = "directed", weighted = TRUE)
  
  nodes_df <- data.frame(ID = c(1:vcount(pathway_graph)), NAME = V(pathway_graph)$name)
  edges_df <- as.data.frame(get.edges(pathway_graph, c(1:ecount(pathway_graph))))
  edges_att <- data.frame(Weight = E(pathway_graph)$weight)
  
  # Replacing all & characters with 'and' so that XML can be exported
  nodes_df$NAME <- str_replace_all(nodes_df$NAME, "&", "and")
  
  write.gexf(nodes = nodes_df,
             edges = edges_df,
             edgesWeight = edges_att$Weight,
             defaultedgetype = "directed",
             output = file)
}

#' @export
filter_trace_adjmatrix <- function(x, min_edge_weight = 0) {
  x[x < min_edge_weight] <- 0
  x
}

filter_pathway_graph_vertices <- function(x, min_degree = 0) {
  
}