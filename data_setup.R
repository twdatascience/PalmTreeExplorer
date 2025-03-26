library(dplyr)
library(stringr)

palmTrees <- readRDS("./data/palmtrees.rds")

columnNamesTable <- tibble(data_name = names(palmTrees),
                           select_name = str_to_title(str_replace_all(names(palmTrees), "_", " ")))

numNamesTable <- palmTrees |> 
  select(where(is.numeric)) |> 
  (\(df) tibble(data_name = names(df),
         select_name = paste(str_to_title(str_replace_all(str_extract(names(df), "^.*(?=_[^_]+$)"), "_", " ")), 
   str_extract(names(df), "[^_]+$"))))()

charNamesTable <- palmTrees |> 
  select(where(is.character)) |> 
  (\(df) tibble(data_name = names(df),
                select_name = str_to_title(str_replace_all(names(df), "_", " "))))()

saveRDS(columnNamesTable, file = "./data/columnNameTables.rds")

saveRDS(numNamesTable, file = "./data/numNamesTable.rds")

saveRDS(charNamesTable, file = "./data/charNamesTable.rds")
