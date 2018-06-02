# addData <- function(df, overwrite = FALSE) {
# 
#   if (isTRUE(overwrite)) {
# 	gbm_data.old <- genie::gbm_data
# 	usethis::use_data(gbm_data.old)
#   }
# 
#   usethis::use_data(substitute(df), overwrite = overwrite)
# }


structureRows <- function(df, nrow = 1) {

  dim(df) <- c(nrow, 2)
  colnames(df) <- c('gene', 'program')
  df
}


addRows <- function(.gene,
					.program,
					addTo = genie::gbm_data,
					hashtable = genie::gbm_hashtable,
					permanent = FALSE,
					overwrite = FALSE) {

  .program <- hashtable[.program]

  adding <- mapply(cbind,
				   .gene,
				   .program,
				   USE.NAMES = F,
				   SIMPLIFY = 'matrix')

  adding <- structureRows(df=adding, nrow=length(.gene))
  print(adding)

  rbind(addTo, adding)

}


addHash <- function(value,
					key = NULL,
					hashtable = genie::gbm_hashtable) {

  if (is.null(key)) {
	key <- value
  }

  else if (!is.null(key) & !value %in% key) {
	key <- c(value, key)
  }

  key <- c(tolower(key), toupper(key))

  value <- rep(value, length(key))

  names(value) <- key

  c(hashtable, value)
}

