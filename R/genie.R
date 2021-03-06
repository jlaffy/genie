retrieve <- function(value, df) {

  out <- df %>%
    dplyr::filter(program %in% value) %>%
    dplyr::select(gene) %>%
    unlist(use.names = F)

  sort(out[!duplicated(out)])

}


genie <- function(key = genie::gbm_default,
                  df = genie::gbm_data,
                  hashtable = genie::gbm_hashtable) {

  if (any(key == 'all')) {
	return(df$gene[!duplicated(df$gene)])
  }

  retrieve(value = hashtable[key], df = df)

}
