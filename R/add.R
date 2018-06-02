programPrep <- function(gene, program, hashtable = genie::gbm_hashtable) {

  program <- hashtable[program]

  if (any(is.na(program))) {
	stop('1 or more programs not recognised.')
  }

  if (length(program) == 1 & length(gene) > 1) {
	program <- rep(program, length(gene))
  }

  program

}


structureRows <- function(df, nrow = 1) {

  dim(df) <- c(nrow, 2)
  colnames(df) <- c('gene', 'program')
  df

}


addRows <- function(gene,
					program,
					addTo = genie::gbm_data,
					hashtable = genie::gbm_hashtable,
					permanent = FALSE,
					overwrite = FALSE) {

  program <- programPrep(gene = gene,
						 program = program,
						 hashtable = hashtable)

  gene.bound <- cbind(gene)
  program.bound <- cbind(program)
  adding <- rbind(gene.bound, program.bound)

  adding <- structureRows(df = adding,
						  nrow = length(gene))

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

