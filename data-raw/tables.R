## code to prepare `DATASET` dataset goes here
gender_table <- list("Male" = 1,
                     "Female" = 0)

education_table <- list("Primary" = 1,
                        "Secondary" = 2,
                        "Associate or vocational education" = 3,
                        "Bachelor" = 4,
                        "Masters or above" = 5)

income_table <- list('< $15,000' = 1,
                     '$15,000 - $24,999' = 2,
                     '$25,000 - $74,999' = 3,
                     '> $75,000' = 4)

employment_table <- list("Unemployed" = 1,
                         "Student" = 2,
                         "Self-employed" = 3,
                         "Employed (part-time)" = 4,
                         "Employed (full-time)" = 5,
                         "Retired" = 6)

marital_table <- list("Divorced" = 1,
                      "Separated" = 2,
                      "Never married" = 3,
                      "Married" = 4)
usethis::use_data(gender_table ,
                  education_table,
                  income_table,
                  employment_table,
                  marital_table, overwrite = TRUE)
