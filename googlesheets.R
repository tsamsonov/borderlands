library(dplyr)
library(tidyr)
library(googlesheets)

tbls_tidy <- function(tbls){
  
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
        select(ISOID = ISO,
               IDXID = INDEX,
               YEAR,
               VALUE,
               SRCID)
    } else {
      values %>% 
        select(ISOID = ISO,
               IDXID = INDEX,
               YEAR,
               VALUE)
    }
  })
}

my_sheets <- gs_ls()
states = gs_title('1а. Демографические показатели - ГОСУДАРСТВА')
vars = gs_ws_ls(states)
tbls = lapply(vars, function(X) gs_read(states, ws = X, range = "A2:L19"))

res = bind_rows(tbls_tidy)
