#' get_raads function
#' @param dataset original dataset "RAADS-14" from the bundle
#' @param completers boolean parameter, if True filters out participants that are not labeled as completers
#' @param subscales boolean parameter, if True includes to the returned dataframe moves subscales 
#' @return either dataframe with 3 columns:
#'         PIN, raads_sum, raads_cat or dataframe with 6 columns: "PIN", "lsas_sum", "lsas_cat", "raads_sym_md","raads_sym_sa","raads_sym_sr"
#' @export
get_raads <- function(dataset, subscales=F, completers=T){
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
  if(!all(dataset$response %in% c("True now and when I was young", "True only when I was younger than 16", "True only now", "Never true"))){
    stop("Range constraints are broken!")
  }
  
  if(!all(as.numeric(dataset$item) %in% c(1:14) )){
    stop("Item constraints are broken!")
  }
  
  
  dataset$item <- as.numeric(dataset$item)
  dataset$response <- ifelse(dataset$item == 6, raads_scale$score_2[match(dataset$response, raads_scale$response)],
                             lsas_scale$score_1[match(dataset$response, raads_scale$response)])
  
  dataset$response <- as.numeric(dataset$response)
  
  df_sum <- aggregate(response ~ pin, data=dataset, sum, na.action = NULL)
  df_sum$raads_cat <- ifelse(df_sum$response >= thr_raads, 1, 0)

  
  
  
  if(subscales == F){
    colnames(df_sum) <- c("PIN", "raads_sum", "raads_cat")
    return(df_sum)
  } else {
    subsc <- data.frame(matrix(ncol = length(names(contingency_raads))+1, nrow = length(num_participants)))
    colnames(subsc) <- c("pin", names(contingency_raads))
    subsc$pin <- as.character(subsc$pin)
    subsc[,1] <- as.character(num_participants)
    for(i in names(contingency_raads)){
      agreg_t <- aggregate(response ~ pin, data=dataset[dataset$item %in% contingency_raads[[i]],], sum, na.action=NULL)
      subsc[,i] <- unname(sapply(subsc$pin, function(x) agreg_t[agreg_t$pin == x, "response"]))
    }
    answer <- merge(df_sum, subsc, by="pin")
    colnames(answer) <- c("PIN", "raads_sum", "raads_cat", "raads_sym_md","raads_sym_sa","raads_sym_sr")
    return(answer)
  }
  
}