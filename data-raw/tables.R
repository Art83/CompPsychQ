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

thr_OCI <- 21

contingency_oci <- list("sym_wash" = c(5,11,17),
                        "sym_obsess" = c(6, 12, 18),
                        "sym_hoard" = c(1, 7, 13),
                        "sym_order" = c(3, 9, 15),
                        "sym_check" = c(2, 8, 14),
                        "sym_ment" = c(4,10,16))


usethis::use_data(gender_table,
                  education_table,
                  income_table,
                  employment_table,
                  marital_table,
                  thr_OCI,
                  contingency_oci, overwrite = TRUE)

