library(tidyverse)
library(ape)
library(tidytree)
args = commandArgs(trailingOnly=TRUE)

if (length(args) != 3) stop("Three arguments: name, path_to_trees, outfile")

name          <- args[1]
path_to_trees <- args[2]
outfile       <- args[3]

######### Read in all tree files into a single tibble
tree_files <- dir(path = path_to_trees, pattern = "*.tre")
tibble(filename = tree_files) %>%
  mutate(file_contents = map(filename,
           ~ as_tibble(read.tree(file.path(path_to_trees, .))))) -> nested_tree_files

# Parse into one row per branch length
nested_tree_files %>%
  unnest(cols = c(file_contents)) %>%
  mutate(filename = str_replace(filename, "\\.tre", ""), 
         filename = str_replace(filename, "\\+F", "")) %>%
  separate(filename, c("id", "model"), sep="-") %>%
  mutate(ASRV = if_else(str_detect(model,"\\+G"), TRUE, FALSE),
         model = str_replace(model, "\\+G", ""),
         dataset = name) %>%
  rename(branch_length = branch.length) %>%
  select(-parent, -label) %>%
  write_csv(outfile)

