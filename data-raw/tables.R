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
thr_isi <- 10
thr_ptsd <- 3
thr_gad <- 10
thr_phq <- 10
thr_pgsi <- 8
thr_yiat <- 38
thr_lsas <- 30
thr_raads <- 14
thr_audit <- 8



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

contingency_pid <- list("pid_sym_na" = c(8,9,10,11,15),
                        "pid_sym_det" = c(4,13,14,16,18),
                        "pid_sym_antag" = c(17,19,20,22,25),
                        "pid_sym_disin" = c(1,2,3,5,6),
                        "pid_sym_psych" = c(7,12,21,23,24))


contingency_yiat <- list("yiat_sym_loctm" = c(1, 2, 3, 6, 8, 9),
                         "yiat_sym_csp" = c(4, 5, 7, 10, 11, 12))



contingency_lsas <- list("lsas_sym_sif" = paste0("F",c(5, 7, 10, 11, 12, 15, 18, 19, 22, 23, 24)),
                         "lsas_sym_sia" = paste0("A",c(5, 7, 10, 11, 12, 15, 18, 19, 22, 23, 24)),
                         "lsas_sym_pf" = paste0("F", c(1, 2, 3, 4, 6, 8, 9,13, 14, 16, 17, 20, 21)),
                         "lsas_sym_pa" = paste0("A", c(1, 2, 3, 4, 6, 8, 9,13, 14, 16, 17, 20, 21)) )

contingency_raads <- list("raads_sym_md" = c(1,4,9,11,12,13,14),
                          "raads_sym_sa" = c(3,5,6,8),
                          "raads_sym_sr" = c(2,7,10))

contingency_audit <- list("audit_sym_ac" = c(1,2,3),
                          "audit_sym_bd" = c(4,5,6),
                          "audit_sym_probs" = c(7,8,9,10))



ftnd_scale <- list(`1` = list("Within 5 minutes" = 3,
                              "6 - 30 minutes" = 2,
                              "31 - 60 minutes" = 1,
                              "After 60 minutes" = 0),
                   `2` = list("Yes" = 1,
                              "No" = 0),
                   `3` = list("The first one in the morning" = 1,
                              "All others" = 0),
                   `4` = list("31 or more" = 3,
                              "21 - 30" = 2,
                              "11 - 20" = 1,
                              "10 or less" = 0),
                   `5` = list("Yes" = 1,
                              "No" = 0),
                   `6` = list("Yes" = 1,
                              "No" = 0))

prime_scale <- data.frame(response = c("Definitely disagree", "Somewhat disagree","Slightly disagree", "Not sure", "Slightly agree", "Somewhat agree", "Definitely agree"),
                          score = c(0:6),stringsAsFactors = F)

pgsi_scale <- data.frame(response = c("Never", "Sometimes","Most of the time", "Almost always"),
                          score = c(0:3),stringsAsFactors = F)

yiat_scale <- data.frame(response = c("Never", "Rarely", "Sometimes", "Often", "Very often"),
                         score = c(1:5), stringsAsFactors = F)

raads_scale <- data.frame(response = c("Never true","True only when I was younger than 16","True only now", "True now and when I was young"),
                                      score_1 = c(0:3), score_2 = c(3:0),stringsAsFactors = F)


audit_scale_1 <- data.frame(response = c("Never", "Monthly or less", "2-4 times a month", "2-3 times a week", "4 or more times a week"),
                            score = c(0:4), stringsAsFactors = F)


audit_scale_2 <- data.frame(response = c("1 or 2", "3 or 4", "5 or 6", "7 to 9", "10 or more"),
                            score = c(0:4),stringsAsFactors = F)


audit_scale_3_8 <- data.frame(response = c("Never", "Less than monthly", "Monthly", "Weekly", "Daily or almost daily"),
                            score = c(0:4), stringsAsFactors = F)


audit_scale_9_10 <- data.frame(response = c("No", "Yes but not in the last year", "Yes, during the last year"),
                             score = c(0,2,4), stringsAsFactors = F)

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
                  thr_pgsi,
                  thr_yiat,
                  thr_lsas,
                  thr_raads,
                  thr_audit,
                  thr_ftnd,
                  contingency_oci,
                  contingency_moves,
                  contingency_eat,
                  contingency_gad,
                  contingency_phq,
                  contingency_dass,
                  contingency_pid,
                  contingency_yiat,
                  contingency_lsas,
                  contingency_raads,
                  contingency_audit,
                  ftnd_scale,
                  prime_scale,
                  pgsi_scale,
                  yiat_scale,
                  raads_scale,
                  audit_scale_1,
                  audit_scale_2,
                  audit_scale_3_8,
                  audit_scale_9_10,
                  overwrite = TRUE, internal = T)

