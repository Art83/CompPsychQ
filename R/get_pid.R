#' get_pid function
#' @param dataset original dataset "PID-5-BF" from the bundle
#' @param completers boolean parameter, if True filters out participants that are not labeled as completers
#' @param subscales boolean parameter, if True includes to the returned dataframe moves subscales 
#' @return either dataframe with 3 columns:
#'         PIN, pid_sum, pid_cat or dataframe with 8 columns: "PIN", "pid_sum","pid_cat", "pid_sym_na", "pid_sym_det", "pid_sym_antag", "pid_sym_disin", "pid_sym_psych"
#' @export

get_pid <- function(dataset, subscales=F, completers=T){
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
  if(!all(dataset$response %in% c("0", "1", "2", "3"))){
    stop("Range constraints are broken!")
  }
  
  if(!all(as.character(dataset$item) %in% as.character(c(1:25)) )){
    stop("Item constraints are broken!")
  }
  
  
  
  
  dataset$response <- as.numeric(dataset$response)
  df_sum <- aggregate(response ~ pin, data=dataset, sum, na.action = NULL)
  

  if(subscales == F){
    colnames(df_sum) <- c("PIN", "pid_sum")
    return(df_sum)
  } else {
    subsc <- data.frame(matrix(ncol = length(names(contingency_pid))+1, nrow = length(num_participants)))
    colnames(subsc) <- c("pin", names(contingency_pid))
    subsc$pin <- as.character(subsc$pin)
    subsc[,1] <- as.character(num_participants)
    for(i in names(contingency_pid)){
      agreg_t <- aggregate(response ~ pin, data=dataset[dataset$item %in% contingency_pid[[i]],], sum, na.action=NULL)
      subsc[,i] <- unname(sapply(subsc$pin, function(x) agreg_t[agreg_t$pin == x, "response"]))
    }
    answer <- merge(df_sum, subsc, by="pin")

    
    
    
    colnames(answer) <- c("PIN", "pid_sum", "pid_sym_na", "pid_sym_det", "pid_sym_antag", "pid_sym_disin", "pid_sym_psych")
    return(answer)
  }
  
}