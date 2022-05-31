#' get_gad function
#' @param dataset original dataset "GAD-7" from the bundle
#' @param completers boolean parameter, if True filters out participants that are not labeled as completers
#' @param subscales boolean parameter, if True includes to the returned dataframe moves subscales 
#' @return either dataframe with 5 columns:
#'         PIN, response, gad_sum, gad_cat, gad_sev or dataframe with 7 columns:PIN, response, gad_sum, gad_cat, gad_sev, gad_sym_nsom, gad_symp_som
#' @export

get_gad <- function(dataset, subscales=F, completers=T){
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
  
  # if(any(is.na(dataset$response))){
  #   warning("You have NAs in response columns!")
  # }
  # if(!all(dataset$response %in% c("0", "1", "2", "3", "4", ))){
  #   stop("Range constraints are broken!")
  # }
  if(!all(dataset$item %in% c("1", "2", "3", "4", "5","6","7", "last"))){
    stop("Range constraints are broken!")
  }
  
  
  
  dataset <- dataset[dataset$item != "last", ]
  dataset$response <- as.numeric(dataset$response)
  df_sum <- aggregate(response ~ pin, data=dataset, sum, na.action = NULL)
  df_sum$gad_cat <- ifelse(df_sum$response >= thr_gad, 1, 0)
  df_sum$gad_sev = ifelse(df_sum$response < 4, 0, 
                    ifelse( (df_sum$response >= 5 & df_sum$response <= 9), 1,
                             ifelse( (df_sum$response >= 10 & df_sum$response <= 14), 2, 3)))
  
  
  if(subscales == F){
    colnames(df_sum) <- c("PIN", "gad_sum", "gad_cat", "gad_sev")
    return(df_sum)
  } else {
    subsc <- data.frame(matrix(ncol = length(names(contingency_gad))+1, nrow = length(num_participants)))
    colnames(subsc) <- c("pin", names(contingency_gad))
    subsc$pin <- as.character(subsc$pin)
    subsc[,1] <- as.character(num_participants)
    for(i in names(contingency_gad)){
      agreg_t <- aggregate(response ~ pin, data=dataset[dataset$item %in% contingency_gad[[i]],], sum, na.action=NULL)
      subsc[,i] <- unname(sapply(subsc$pin, function(x) agreg_t[agreg_t$pin == x, "response"]))
    }
    answer <- merge(df_sum, subsc, by="pin")
    colnames(answer) <- c("PIN", "gad_sum", "gad_cat", "gad_sev", "nonsom_fcr", "som_fcr")
    return(answer)
  }
  
}