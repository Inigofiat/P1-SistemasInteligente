# Clear environment and console
rm(list=ls())
cat("\014")
graphics.off()
# Set working directory
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

#Import the libraries needed to display the results
library(kableExtra)
library(magrittr)

# Include algorithm functions
source("../algorithms/blind/expand-node.R")
source("../algorithms/blind/breadth-first-search.R")
source("../algorithms/blind/depth-first-search.R")
source("../algorithms/blind/depth-limited-search.R")
source("../algorithms/blind/iterative-deepening-search.R")
source("../algorithms/informed/uniform-cost-search.R")
source("../algorithms/informed/greedy-best-first-search.R")
source("../algorithms/informed/a-star-search.R")

# Include functions for data analysis and result plot
source("../algorithms/results-analysis/analyze-results.R")

# 8-Puzzle Problem

source("../problem/MisioneroCanibalCod.R")

# Function to solve the problem using different algorithms
solve.problem <- function(problem) {
  bfs_gs   <- breadth.first.search(problem, max_iterations = 2000, count_print = 1000, graph_search = TRUE)
  dfs_gs   <- depth.first.search(problem, max_iterations = 2000, count_print = 1000, graph_search = TRUE)
  dls20_gs <- depth.limited.search(problem, max_iterations = 2000, depth_limit = 20, count_print = 1000, graph_search = TRUE)
  ids_gs   <- iterative.deepening.search(problem, max_iterations = 2000, max_depth = 20, count_print = 1000, graph_search = TRUE)
  
  results <- analyze.results(list(bfs_gs,
                                  dfs_gs,
                                  dls20_gs,
                                  ids_gs
                                  ), problem)
  
  # Print results in an HTML Table
  kable_material(kbl(results, caption = "MisionerosCanibal"),  c("striped", "hover", "condensed", "responsive"))
}

# 6 steps needed to be solved
problem <- initialize.problem(3,3,2)
# Solve the problem
solve.problem(problem)

# 12 steps needed to be solved
problem <- initialize.problem(5,5,3)
# Solve the problem

solve.problem(problem)

problem <- initialize.problem(10,10,4)
# Solve the problem
solve.problem(problem)