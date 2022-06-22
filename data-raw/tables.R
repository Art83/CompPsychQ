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
thr_eat <- 20
thr_sds <- 5
thr_asrs <- 14
thr_ftnd <- 4
thr_asrm <- 6
thr_isi <- 15
thr_ptsd <- 3
thr_gad <- 10
thr_phq <- 10



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

contingency_eat <- list("eat_sym_diet" = c(1,6,7,10,11,12,14,16,17,22,23,24,26),
                        "eat_sym_bul" = c(3,4,9,18,21,25),
                        "eat_sym_oral" = c(2,5,8,13,15,19,20))

contingency_gad <- list("gad_sym_nsom" = c(1, 2, 3, 7),
                        "gad_symp_som" = c(4, 5, 6 ))


contingency_phq <- list("phq_sym_nsom" = c(1, 2, 6, 9),
                        "phq_symp_som" = c(3, 4, 5, 7,8))



contingency_dass <- list("dass_sym_depr" = c(3, 5, 10, 13, 16, 17, 21),
                        "dass_sym_anx" = c(2, 4, 7, 9, 15, 19, 20),
                        "dass_sym_stress" = c(1, 6, 8, 11, 12, 14, 18))




ftnd_scale <- list(`1` = list("Within 5 minutes" = 3,
                              "6 - 30 minutes" = 2,
                              "31 - 60 minutes" = 1,
                              "6 - 30 minutes" = 0),
                   `2` = list("Yes" = 1,
                              "No" = 0),
                   `3` = list("The first one in the morning" = 1,
                              "All others" = 0),
                   `4` = list("31 or more" = 3,
                              "21-31" = 2,
                              "11 - 20" = 1,
                              "10 or less" = 0),
                   `5` = list("Yes" = 1,
                              "No" = 0),
                   `6` = list("Yes" = 1,
                              "No" = 0))

prime_scale <- data.frame(response = c("Definitely disagree", "Somewhat disagree","Slightly disagree", "Not sure", "Slightly agree", "Somewhat agree", "Definitely agree"),
                          score = c(0:6))


usethis::use_data(gender_table,
                  education_table,
                  income_table,
                  employment_table,
                  marital_table,
                  thr_oci,
                  thr_moves,
                  thr_eat,
                  thr_asrs,
                  thr_sds,
                  thr_asrm,
                  thr_isi,
                  thr_ptsd,
                  thr_gad,
                  thr_phq,
                  contingency_oci,
                  contingency_moves,
                  contingency_eat,
                  contingency_gad,
                  contingency_phq,
                  contingency_dass,
                  ftnd_scale,
                  prime_scale,
                  overwrite = TRUE, internal = T)

