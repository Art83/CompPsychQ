#' get_eating function
#' @param dataset original dataset "EAT-26" from the bundle
#' @param completers boolean parameter, if True filters out participants that are not labeled as completers
#' @param subscales boolean parameter, if True includes to the returned dataframe eating subscales 
#' @return either dataframe with 3 columns:
#'         PIN, response, oci_cat or dataframe with 6 columns: PIN, response, eat_cat, eat_sym_diet, eat_sym_bul, eat_sym_oral
#' @export

get_eating <- function(dataset, subscales=F, completers=T){
  if(nrow(dataset) == 0 | ncol(dataset) == 0){
    stop("Empty dataset")
  }
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
  if(any(is.na(dataset["response"])) | any(dataset["pin"] == "")){
    stop("Missed data in response column")
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
  
  # Range constraints & checking type
  if(is.factor(dataset$response) | is.factor(dataset$item)){
    stop("One of the columns is factor")
  }

  dataset$item <- as.numeric(dataset$item)
  d_1_25 <- dataset[dataset$item %in% c(1:25), ]
  if(all(d_1_25$response %in% c("Always", "Usually", "Often", "Rarely", "Never"))){
    stop("Not expected value in items")
  }
  d_26 <- dataset[dataset$item == 26, ]
  if(all(d_26$response %in% c("Always", "Usually", "Often", "Rarely", "Never"))){
    stop("Not expected value in items")
  }

  d_1_25$response <- ifelse(as.character(d_1_25$response) == 'Always', 3, 
                                ifelse(as.character(d_1_25$response) == 'Usually', 2,
                                       ifelse(as.character(d_1_25$response) == 'Often', 1,0)))
  
  
  d_26$response <- ifelse(as.character(d_26$response) == 'Never', 3, 
                             ifelse(as.character(d_26$response) == 'Rarely', 2,
                                    ifelse(as.character(d_26$response) == 'Sometimes', 1,0)))
  
  eat_total <-  rbind(d_1_25, d_26)
  eat_total <- eat_total[order(eat_total$pin, as.numeric(eat_total$item)),]
  
  
  df_sum <- aggregate(response ~ pin, data=eat_total, sum)
  df_sum$eat_cat <- ifelse(df_sum$response >= CompPsychQ::thr_eat, 1, 0)
  
  
  if(subscales == F){
    return(df_sum)
  } else {
    subsc <- data.frame(matrix(ncol = length(names(CompPsychQ::contingency_eat))+1, nrow = length(num_participants)))
    colnames(subsc) <- c("pin", names(CompPsychQ::contingency_eat))
    subsc[,1] <- as.character(num_participants)
    for(i in names(CompPsychQ::contingency_eat)){
      subsc[,i] <- aggregate(response ~ pin, data=eat_total[as.numeric(eat_total$item) %in% CompPsychQ::contingency_eat[[i]],], sum)[,2]
    }
    answer <- merge(df_sum, subsc, by="pin")
    return(answer)
  }
  
}