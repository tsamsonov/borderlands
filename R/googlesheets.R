library(DBI)
library(dplyr)
library(tidyr)
library(googlesheets)

tidy_tbls <- function(tbls){
  
  lapply(tbls, function(df){
    
    yrcols = colnames(df) %>%
      as.numeric() %>%
      sapply(function(X) !is.na(X)) %>%
      which()
    
    srcols = grep('Source', colnames(df))
    
    values = df %>% 
      gather(YEAR, VALUE, yrcols) %>% 
      mutate(VALUE = as.numeric(gsub(',', '.', VALUE)),
             YEAR = as.integer(YEAR))
    
    if (length(srcols) == length(yrcols)) {
      
      sources = df %>% 
        gather(YEAR, SRCID, srcols)
      
      values %>% 
        bind_cols(sources) %>% 
        mutate(IDXID = as.integer(INDEX),
               SRCID = as.integer(SRCID)) %>% 
        dplyr::select(IDXID,
               ISOID = ISO,
               YEAR,
               VALUE,
               SRCID)
    } else {
      values %>%
        mutate(IDXID = as.integer(INDEX)) %>% 
        dplyr::select(IDXID,
               ISOID = ISO,
               YEAR,
               VALUE)
    }
  })
}

# Connect to DB -----------------------------------------------------

con <- dbConnect(RSQLite::SQLite(),
                 dbname = "data/borderlands.db"
)

# Load INDEXES ------------------------------------------------------------

idx = gs_title('Показатели') %>% gs_read()
dbWriteTable(con, c("INDEXES"), value=idx, overwrite=TRUE, row.names=FALSE)

# my_sheets <- gs_ls()

# Load VALUES -------------------------------------------------------------


endrows = c(
  '1а. Демографические показатели - ГОСУДАРСТВА' = 19,
  '1б. Демографические показатели - РЕГИОНЫ' = 95
)

out_table = NULL

for (i in 1:length(endrows)) {
  
  endrow = endrows[i]
  tbl_name = attr(endrow, 'name') %>% gs_title()
  
  vars =  gs_ws_ls(tbl_name)
  
  tbls = lapply(vars, function(X) gs_read(tbl_name, 
                                          ws = X,
                                          col_types = readr::cols(.default = "c"),
                                          range = cell_rows(2:endrow)))
  
  res = tbls %>% 
    tidy_tbls() %>% 
    bind_rows() %>% 
    dplyr::filter(!is.na(VALUE))
  
  if (i == 1)
    out_table = res
  else
    out_table = out_table %>% bind_rows(res)

}

# Load to DB --------------------------------------------------------
dbWriteTable(con, c("VALUES"), value=out_table, overwrite=TRUE, row.names=FALSE)