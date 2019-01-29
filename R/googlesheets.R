library(DBI)
library(dplyr)
library(tidyr)
library(googlesheets4)

token_tables = '156ysLkR-T4qIs8_M5XEL9t-ZF6bONtfw6eweq-Cojuc'
token_idx = '1AVYkP3GTQY3LHUYF8M4LYhelsKYCFyVk7Y5yJHI-yhc'

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

# idx = read_sheet(token_idx)
# dbWriteTable(con, c("INDEXES"), value=idx, overwrite=TRUE, row.names=FALSE)

# Load TABLES ------------------------------------------------------------
tbls = read_sheet(token_tables)

# Load VALUES -------------------------------------------------------------

# endrows = c(
#   '1а. Демографические показатели - ГОСУДАРСТВА' = 19,
#   '1б. Демографические показатели - РЕГИОНЫ' = 95
# )

ids = c(21, 22)
sheets = list(1:5, 1:5)

load_tbls = tbls %>% filter(id %in% ids)

out_table = NULL

for (i in 1:length(ids)) {
  
  endrow = load_tbls %>% slice(i) %>% pull(endrow)
  tbl_name = load_tbls %>% slice(i) %>% pull(token)
  
  vars = sheets_get(tbl_name)$sheets$name[sheets[[i]]]
  
  tbls = lapply(vars, function(X) read_sheet(tbl_name, 
                                             sheet = X,
                                             col_types = 'c',
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

copy_to(con, out_table, name = 'ADD')
dbSendQuery(con, 'REPLACE INTO "VALUES" SELECT * FROM "ADD"')
dbDisconnect(con)
  

# Load to DB --------------------------------------------------------
# dbWriteTable(con, c("VALUES"), value = out_table, append = TRUE, row.names = FALSE)