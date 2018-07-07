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
    # yrcols = srcols - length(srcols)
    
    values = df %>% 
      gather(YEAR, VALUE, yrcols) %>% 
      mutate(VALUE = as.numeric(VALUE),
             YEAR = as.integer(YEAR))
    
    if (length(srcols) == length(yrcols)) {
      
      sources = df %>% 
        gather(YEAR, SRCID, srcols)
      
      values %>% 
        bind_cols(sources) %>% 
        select(IDXID = INDEX,
               ISOID = ISO,
               YEAR,
               VALUE,
               SRCID)
    } else {
      values %>% 
        select(IDXID = INDEX,
               ISOID = ISO,
               YEAR,
               VALUE)
    }
  })
}

# name = '1а. Демографические показатели - ГОСУДАРСТВА'
# endrow = 19

name = '1б. Демографические показатели - РЕГИОНЫ'
endrow = 95

my_sheets <- gs_ls()
states = gs_title(name)
vars = gs_ws_ls(states)

tbls = lapply(vars, function(X) gs_read(states, ws = X, range = cell_rows(2:endrow)))

res = tbls %>% 
  tidy_tbls() %>% 
  bind_rows()
