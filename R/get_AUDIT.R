#' get_audit function
#' @param dataset original dataset "AUDIT" from the bundle
#' @param completers boolean parameter, if True filters out participants that are not labeled as completers
#' @param subscales boolean parameter, if True includes to the returned dataframe moves subscales 
#' @return either dataframe with 4 columns:
#'         "PIN", "audit_sum", "audit_cat", "audit_sev" or dataframe with 7 columns: "PIN", "audit_sum", "audit_cat", "audit_sev", "audit_sym_ac", "audit_sym_bd", "audit_sym_probs"
#' @export
get_audit <- function(dataset, subscales=F, completers=T){
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
  if(!all(dataset$response %in% c(as.character(audit_scale_1$response), as.character(audit_scale_2$response), 
                                  as.character(audit_scale_3_8$response), as.character(audit_scale_9_10$response)) )){
    stop("Range constraints are broken!")
  }
  
  if(!all(as.numeric(dataset$item) %in% c(1:10) )){
    stop("Item constraints are broken!")
  }
  
  
  
  
  
  dataset$item <- as.numeric(dataset$item)
  dataset$response <- ifelse(dataset$item == 1, audit_scale_1$score[match(dataset$response, audit_scale_1$response)],
                             ifelse(dataset$item == 2,audit_scale_2$score[match(dataset$response, audit_scale_2$response)], 
                                    ifelse(dataset$item %in% c(3:8), audit_scale_3_8$score[match(dataset$response, audit_scale_3_8$response)],
                                           audit_scale_9_10$score[match(dataset$response, audit_scale_9_10$response)])))
  
  dataset$response <- as.numeric(dataset$response)
  
  df_sum <- aggregate(response ~ pin, data=dataset, sum, na.action = NULL)
  df_sum$audit_cat <- ifelse(df_sum$response >= thr_audit, 1, 0)
  df_sum$audit_sev <- ifelse(df_sum$response >= 15, 3,
                             ifelse(df_sum$response >= 8 & df_sum$response <= 14, 2,
                                    ifelse(df_sum$response >= 1 & df_sum$response <= 7, 1,0)))
  
  
  
  if(subscales == F){
    colnames(df_sum) <- c("PIN", "audit_sum", "audit_cat", "audit_sev")
    return(df_sum)
  } else {
    subsc <- data.frame(matrix(ncol = length(names(contingency_audit))+1, nrow = length(num_participants)))
    colnames(subsc) <- c("pin", names(contingency_audit))
    subsc$pin <- as.character(subsc$pin)
    subsc[,1] <- as.character(num_participants)
    for(i in names(contingency_audit)){
      agreg_t <- aggregate(response ~ pin, data=dataset[dataset$item %in% contingency_audit[[i]],], sum, na.action=NULL)
      subsc[,i] <- unname(sapply(subsc$pin, function(x) agreg_t[agreg_t$pin == x, "response"]))
    }
    answer <- merge(df_sum, subsc, by="pin")
    colnames(answer) <- c("PIN", "audit_sum", "audit_cat", "audit_sev", "audit_sym_ac", "audit_sym_bd", "audit_sym_probs")
    return(answer)
  }
  
}