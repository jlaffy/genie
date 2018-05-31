retrieve <- function(value, df) {

  out <- df %>%
    dplyr::filter(program %in% value) %>%
    dplyr::select(gene) %>%
    unlist(use.names = F)

  sort(out[!duplicated(out)])

}


genie <- function(key = gbm_default,
                  df = gbm_data,
                  hashtable = gbm_hashtable) {

  retrieve(value = hashtable[key], df = df)

}
