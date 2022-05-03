#' get_demographics function
#' @param dataset original dataset "demographics" from the bundle
#' @param completers boolean parameter, if True filters out participants that are not labeled as completers
#' @param convert boolean parameter, if True converts all categorical values into integers
#' @return dataframe with eleven columns:
#'         PIN, gender, age,height,weight,BMI, marital status, education,employment, income, location 
#' @export

get_demographics <- function(dataset, completers=T, convert=F, checks=T){
  
  if(nrow(dataset) == 0 | ncol(dataset) == 0){
    stop("Empty dataset")
  }
  dataset$PIN <- gsub("'", "", dataset$PIN)
  essential_cols <- c("pin", "item", "response", "location")
  nonessential_cols <- c("complete", "date", "calendar_time", "timestamp", "timezone", "commit", "version")
  
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
  # Age
  tryCatch(age <- dataset[dataset$item == '2. Age (years)', c("pin", "response")], error = function(e){
    message("Check age column")
    message(e)
  })
  colnames(age)[2] <- "age"
  age$age <- as.numeric(age$age)
  
  # gender
  tryCatch(gender <- dataset[dataset$item == '1. Gender', c("pin", "response")], error = function(e){
    message("Check gender column")
    message(e)
  })
  colnames(gender)[2] <- "gender"
  
  # height
  tryCatch({height <- dataset[dataset$item == '3a. Height (cm)' | 
                                dataset$item == '3b. Height (feet)' | 
                                dataset$item == '3c. Height (inches)', c("pin", "item", "response")]
  height <- height[!is.na(height$response),] }, error = function(e){
    message("Check height column")
    message(e)
  })
  height <- tidyr::pivot_wider(height, names_from = item, values_from = response)
  colnames(height)[grepl("feet",colnames(height))] <- "feet"
  colnames(height)[grepl("inches",colnames(height))] <- "inch"
  colnames(height)[grepl("cm",colnames(height))] <- "cm"
  ## Converting from feet + inch to cm and putting it into cm column 
  height$feet <- as.numeric(height$feet)
  height$inch <- as.numeric(height$inch)
  height$cm <- as.numeric(height$cm)
  height$cm[which(is.na(height$cm))] <- (as.numeric(height$feet[which(is.na(height$cm))])*12 + as.numeric(height$inch[which(is.na(height$cm))]))*2.54
  height <- height[,c("pin","cm")]
  colnames(height)[grepl("cm", colnames(height))] <- "height"
  
  # weight
  tryCatch({weight <- dataset[dataset$item == '4a Weight(kg)' | 
                                dataset$item == '4b Weight (lbs)', c("pin", "item", "response")]
  weight <- weight[!is.na(weight$response),] }, error = function(e){
    message("Check height column")
    message(e)
  })
  ## Converting from lbs to kg
  weight$response <- as.numeric(weight$response)
  weight$response[which(grepl('lbs', weight$item))] <- as.numeric(weight$response[which(grepl('lbs', weight$item))]) * 0.454
  weight <- weight[,c("pin", "response")]
  colnames(weight)[grepl("response", colnames(weight))] <- "weight"
  
  # BMI and combining height, weight and BMI into one df
  weight_height <- merge(height, weight, by="pin")
  weight_height$BMI <- (as.numeric(weight_height$weight) / as.numeric(weight_height$height)^2)*10000
  
  # Education
  tryCatch(edu <- dataset[dataset$item == '5. Education level', c("pin", "response")], error = function(e){
    message("Check education column")
    message(e)
  })
  colnames(edu)[grepl("response", colnames(edu))] <- "education"
  
  #Marital status
  tryCatch(ms <- dataset[dataset$item == '6. Marital status', c("pin", "response")], error = function(e){
    message("Check marital status column")
    message(e)
  })
  colnames(ms)[grepl("response", colnames(ms))] <- "marital_status"
  
  # Employment
  tryCatch(employ <- dataset[dataset$item == '7. Employment status', c("pin", "response")], error = function(e){
    message("Check employment column")
    message(e)
  })
  colnames(employ)[grepl("response", colnames(employ))] <- "employment"
  
  # Income
  tryCatch(income <- dataset[dataset$item == '8. Gross annual household income (US dollars)', c("pin", "response")], error = function(e){
    message("Check income column")
    message(e)
  })
  colnames(income)[grepl("response", colnames(income))] <- "income"
  
  # location
  location <- dataset[match(unique(dataset$pin), dataset$pin), c("pin", "location")]
  
  # Combining into one dataframe to return
  participants_table <- tryCatch(Reduce(function(x,y) merge(x,y,by="pin"), list(gender,
                                                                                age,
                                                                                weight_height,
                                                                                ms,
                                                                                edu,
                                                                                employ,
                                                                                income,
                                                                                location)),
                                 error = function(e){
                                   message("Wrong dimensions of some demographics vectors")
                                   message(e)
                                 })
  
  # Basic Checks
  if(checks){
    participants_table$age[which(participants_table$age > 80 | participants_table$age < 16)] <- NA
    participants_table$height[which(participants_table$height > 200 | participants_table$height < 140)] <- NA
    participants_table$weight[which(participants_table$weight > 200 | participants_table$weight < 40)] <- NA
    participants_table$BMI[which(participants_table$BMI > 50 | participants_table$BMI < 10)] <- NA
  }
  
  if(convert){
    participants_table$marital_status <- sapply(participants_table$marital_status, function(x) CompPsychQ::marital_table[[x]])
    participants_table$education <- sapply(participants_table$education, function(x) CompPsychQ::education_table[[x]])
    participants_table$employment <- sapply(participants_table$employment, function(x) CompPsychQ::employment_table[[x]])
    participants_table$income <- sapply(participants_table$income, function(x) CompPsychQ::income_table[[x]])
    participants_table$gender <- sapply(participants_table$gender, function(x) CompPsychQ::gender_table[[x]])
    return(participants_table)
  } else {
    return(participants_table)
  }
    
}





