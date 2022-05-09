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

thr_oci <- 21
thr_moves <- 10

contingency_oci <- list("sym_wash" = c(5,11,17),
                        "sym_obsess" = c(6, 12, 18),
                        "sym_hoard" = c(1, 7, 13),
                        "sym_order" = c(3, 9, 15),
                        "sym_check" = c(2, 8, 14),
                        "sym_ment" = c(4,10,16))

contingency_moves <- list("sym_mtsimp" = c(2, 6),
                          "sym_mtcomp" = c(10, 15, 16),
                          "sym_mtsub" = c(2, 6, 10, 15, 16),
                          "sym_vtsimp" = c(1, 13),
                          "sym_vtcomp" = c(5, 12),
                          "sym_vtsub" = c(1, 13, 5, 12),
                          "sym_ticsub" = c(2, 6, 10, 15, 16, 1, 13, 5, 12),
                          "sym_obsess" = c(3, 7, 9, 14),
                          "sym_comp" = c(4, 8, 11),
                          "sym_ocsub" = c(3, 7, 9, 14, 4, 8, 11),
                          "sym_assoc" = c(17, 18, 19, 20))



usethis::use_data(gender_table,
                  education_table,
                  income_table,
                  employment_table,
                  marital_table,
                  thr_oci,
                  thr_moves,
                  contingency_oci,
                  contingency_moves,
                  overwrite = TRUE)

