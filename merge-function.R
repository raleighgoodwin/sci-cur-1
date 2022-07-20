library(rio)
library(here)
library(tidyverse)

merge_df <- function(dfs, labels) {
  rbind(dfs) %>% 
    mutate(batch = labels)
}