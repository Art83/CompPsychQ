#' get_sds function
#' @param dataset original dataset "SDS" from the bundle
#' @param completers boolean parameter, if True filters out participants that are not labeled as completers
#' @return dataframe with 3 columns:
#'         PIN, sds_sum, sds_cat
#' @export
get_sds <- function(dataset, subscales=F, completers=T){
  if(nrow(dataset) == 0 | ncol(dataset) == 0){
    stop("Empty dataset")
  }
  dataset$PIN <- gsub("'", "", dataset$PIN)
  essential_cols <- c("pin", "complete",  "item", "response")
  colnames(dataset) <- tolower(colnames(dataset))
  if(!all(essential_cols %in% colnames(dataset))){
    stop(essential_cols[!essential_cols %in% colnames(dataset)]," column(s) not found in the dataset")
  }
  if(any(is.na(dataset["pin"])) | any(dataset["pin"] == "")){
    stop("Missed data in pin column")
  }
  if(any(is.na(dataset["item"])) | any(dataset["pin"] == "")){
    stop("Missed data in item column")
  }
  if(completers){
    num_participants <- unique(dataset[dataset$complete == 'y', "pin"])
    dataset <- dataset[dataset$complete == "y", ]
    if(nrow(dataset) == 0){
      stop("There are no completers in your dataset")
    }
  } else {
    num_participants <- unique(dataset$pin)
  }
  
  if(any(is.na(dataset$response))){
    warning("You have NAs in response columns!")
  }
  items_of_inter <- c('SOCIAL*', 'FAMILY*', 'WORK*')
  dataset <- dataset[dataset$item %in% items_of_inter, ]
  dataset$response <- as.numeric(dataset$response)
  if(any(is.na(dataset["response"]))){
    na_work <- which(is.na(dataset[,"response"]))
    dataset[na_work, "response"] <- sapply(na_work, function(x) mean(dataset[dataset$pin == dataset$pin[x], "response"],na.rm = T) )
  }
  dataset$sds_cat <- ifelse(dataset$response >= thr_sds, 1, 0)
  ds <- aggregate(response ~ pin, dataset, sum)
  df_sum <- aggregate(sds_cat ~ pin, data = dataset, sum)
  df_sum$sds_cat <- ifelse(df_sum$sds >= 1, 1, 0)
  df_sum <- merge(ds, df_sum, by="pin")
  colnames(df_sum) <- c("PIN", "asrs_sum", "asrs_cat")
  return(df_sum)
}
