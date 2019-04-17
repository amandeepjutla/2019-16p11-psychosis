# 3_derive_psychosis_index.r

# This script derives a psychotic symptom index and also
# builds a summary dataframe for use in analyses.

# Positive screen based on SOPS
svip$sops <- svip$sops %>% mutate(
  sops_psychosis_index = 
    sops_delusional + 
    sops_persecutory + 
    sops_grandiosity + 
    sops_hallucinations) %>% mutate(
  sops_positive = if_else(sops_psychosis_index > 0, 1, 0, 0)) 

# Positive screen based on BCL (ie, CBCL or ABCL)
svip$abcl_18_59 <- svip$abcl_18_59 %>% mutate(
  abcl_positive = if_else(abcl_18_59.thought_t_score >= 
                            BCL_T_SCORE_CUTOFF, 1, 0, 0)) 

svip$cbcl_6_18 <- svip$cbcl_6_18 %>% mutate(
  cbcl_positive = if_else(cbcl_6_18.thought_problems_t_score >= 
                            BCL_T_SCORE_CUTOFF, 1, 0, 0)) 

# `missingness` variables incorporate presence/absence information.
svip$abcl_18_59 <- svip$abcl_18_59 %>% mutate(
  abcl_missingness = if_else(abcl_18_59.thought_t_score >= 
                               BCL_T_SCORE_CUTOFF, 1, 0, missing = NULL))

svip$cbcl_6_18 <- svip$cbcl_6_18 %>% mutate(
  cbcl_missingness = if_else(cbcl_6_18.thought_problems_t_score >= 
                               BCL_T_SCORE_CUTOFF, 1, 0, missing = NULL)) 

# Psychotic symptoms, OCD symptoms, and OCD diagnosis based on DISC
svip$disc_youth <- svip$disc_youth %>% mutate(
  disc_positive =
    if_else(disc_youth.misc_schiz_symp_year > 0 |
            disc_youth.misc_schiz_symp_month > 0 |
            disc_youth.symp_delusions == "yes" |
            disc_youth.symp_halluc == "yes", 1, 0, 0)) %>% mutate(
  ocd =
    if_else(disc_youth.anx_ocd_symp_year > 0, 1, 0, 0)) %>% mutate(
  ocd_dx =
    if_else(!is.na(disc_youth.anx_ocd_year) & 
            disc_youth.anx_ocd_year != "negative", 1, 0, 0))

svip$disc_youth <- svip$disc_youth %>% 
  mutate_if(is.character, str_replace_all, pattern = "no",
            replacement = "0") %>%
  mutate_if(is.character, str_replace_all, pattern = "not-assessed",
            replacement = "0") %>%
  mutate_if(is.character, str_replace_all, pattern = "yes",
            replacement = "1") 

disc_pattern <- str_c("disc_youth",".symp_delusions")
disc_questions <- svip$disc_youth %>% 
  select(starts_with(disc_pattern)) %>% 
  colnames
svip$disc_youth <- svip$disc_youth %>% 
  modify_at(disc_questions, as.numeric)

disc_pattern <- str_c("disc_youth",".symp_halluc")
disc_questions <- svip$disc_youth %>% 
  select(starts_with(disc_pattern)) %>% 
  colnames
svip$disc_youth <- svip$disc_youth %>% 
  modify_at(disc_questions, as.numeric)

disc_subset <- filter(
  svip$disc_youth, !is.na(disc_youth.measure.eval_age_months))

# `disc_subset` doesn't incorporate missingness information
# (but `disc_missingness` will).
disc_subset[is.na(disc_subset)] <- 0

disc_subset <- disc_subset %>% mutate(
  disc_missingness_index = 
    disc_youth.misc_schiz_symp_year +
    disc_youth.symp_delusions +
    disc_youth.symp_halluc) %>% mutate(
  disc_missingness = 
    if_else(disc_missingness_index > 0, 1, 0, 0))

# Psychotic symptoms based on SCL

# Make data numeric
svip$scl_90_r <- svip$scl_90_r %>% 
  mutate_if(is.character, str_replace_all, pattern = "not-at-all",
            replacement = "0") %>%
  mutate_if(is.character, str_replace_all, pattern = "a-little-bit",
            replacement = "1") %>%
  mutate_if(is.character, str_replace_all, pattern = "moderately",
            replacement = "2") %>% 
  mutate_if(is.character, str_replace_all, pattern = "quite-a-bit",
            replacement = "3") %>% 
  mutate_if(is.character, str_replace_all, pattern = "extremely",
            replacement = "4")

scl_pattern <- str_c("scl_90_r",".scl_90_r",".q")
scl_questions <- svip$scl_90_r %>% 
  select(starts_with(scl_pattern)) %>% 
  colnames
svip$scl_90_r <- svip$scl_90_r %>% 
  modify_at(scl_questions, as.numeric)
copy_scl <- svip$scl_90_r 

copy_scl_2 <- filter(
  copy_scl, !is.na(scl_90_r.scl_90_r.q07_someone_control_thoughts))

copy_scl_2 <- copy_scl_2 %>% mutate(
  scl_missingness = 
    scl_90_r.scl_90_r.q07_someone_control_thoughts +
    scl_90_r.scl_90_r.q16_hearing_voices +
    scl_90_r.scl_90_r.q35_aware_private_thoughts + 
    scl_90_r.scl_90_r.q62_thoughts_not_your_own) %>% mutate(
  scl_missingness_binary =
    if_else(scl_missingness > 0, 1, 0, missing = NULL))

svip$scl_90_r[is.na(svip$scl_90_r)] <- 0

# Clean up temporary variables
rm(scl_pattern)
rm(scl_questions)

svip$scl_90_r <- svip$scl_90_r %>% mutate(
  scl_psychosis_index = 
    scl_90_r.scl_90_r.q07_someone_control_thoughts +
    scl_90_r.scl_90_r.q16_hearing_voices +
    scl_90_r.scl_90_r.q35_aware_private_thoughts + 
    scl_90_r.scl_90_r.q62_thoughts_not_your_own) %>% mutate(
  scl_positive =
    if_else(scl_psychosis_index > 0, 1, 0, 0))

# ASD based on clinical diagnosis
svip$diagnosis_summary <- svip$diagnosis_summary %>% mutate(
  clinical_asd = if_else(diagnosis_summary.clinical_asd_dx == "true", 1, 0, 0))

# Summary dataframe containing variables of interest
svip_summary <- 
  subset(svip$svip_subjects,
         select = c("sfari_id",
                    "sex",
                    "genetic_status_16p",
                    "family_type",
                    "family",
                    "mother",
                    "father",
                    "relationship_to_iip")) %>%
  left_join(select(disc_subset, disc_missingness, individual),
            by = c("sfari_id" = "individual")) %>%
  left_join(select(svip$svip_summary_variables, 
                   svip_summary_variables.age_months, individual),
            by = c("sfari_id" = "individual")) %>%
  left_join(select(svip$abcl_18_59, abcl_positive, individual),
            by = c("sfari_id" = "individual")) %>%
  left_join(select(copy_scl_2, scl_missingness_binary, individual),
            by = c("sfari_id" = "individual")) %>%
  left_join(select(svip$cbcl_6_18, cbcl_positive, individual),
            by = c("sfari_id" = "individual")) %>% 
  left_join(select(svip$abcl_18_59, abcl_missingness, individual),
            by = c("sfari_id" = "individual")) %>%
  left_join(select(svip$cbcl_6_18, cbcl_missingness, individual),
            by = c("sfari_id" = "individual")) %>% 
  left_join(select(svip$disc_youth, disc_positive, individual),
            by = c("sfari_id" = "individual")) %>%
  left_join(select(svip$scl_90_r, scl_positive, individual),
            by = c("sfari_id" = "individual")) %>%
  left_join(select(svip$sops, sops_positive, individual),
            by = c("sfari_id" = "individual")) %>%
  left_join(select(svip$diagnosis_summary,
                   diagnosis_summary.ados_css, individual),
            by = c("sfari_id" = "individual")) %>%
  left_join(select(svip$diagnosis_summary,
                   diagnosis_summary.best_full_scale_iq, individual),
            by = c("sfari_id" = "individual")) %>% 
  left_join(select(svip$diagnosis_summary, clinical_asd, individual),
            by = c("sfari_id" = "individual")) %>% 
  left_join(select(svip$diagnosis_summary,
                   diagnosis_summary.ados_rrb_css, individual),
            by = c("sfari_id" = "individual")) %>% 
  left_join(select(svip$diagnosis_summary, 
                   diagnosis_summary.ados_sa_css, individual),
            by = c("sfari_id" = "individual")) %>%
  left_join(select(svip$adi_r, adi_r.c_total, individual),
            by = c("sfari_id" = "individual")) %>% 
  left_join(select(svip$disc_youth, ocd, individual),
            by = c("sfari_id" = "individual")) %>% 
  left_join(select(svip$disc_youth, ocd_dx, individual),
            by = c("sfari_id" = "individual")) %>%
  left_join(select(svip$diagnosis_summary, ados_css_rrb_derived, individual),
            by = c("sfari_id" = "individual")) %>%
  left_join(select(svip$diagnosis_summary, ados_css_sa_derived, individual),
            by = c("sfari_id" = "individual")) %>%
  left_join(select(svip$diagnosis_summary, ados_css_total_derived, individual),
            by = c("sfari_id" = "individual")) %>% 
  left_join(select(svip$das_ii_early_years, 
                   das_ii_early_years.gca_lower_standard, individual),
            by = c("sfari_id" = "individual")) %>%
  left_join(select(svip$das_ii_early_years, 
                   das_ii_early_years.gca_upper_standard, individual),
            by = c("sfari_id" = "individual")) %>%
  left_join(select(svip$das_ii_school_age, 
                   das_ii_school_age.gca_standard, individual),
            by = c("sfari_id" = "individual")) %>%
  left_join(select(svip$wasi, wasi.measure.measure_type, individual),
            by = c("sfari_id" = "individual")) %>%
  left_join(select(svip$diagnosis_summary, ados_css_total_combined, individual),
            by = c("sfari_id" = "individual")) %>%
  left_join(select(svip$cbcl_6_18, cbcl_6_18.thought_problems_t_score, individual),
          by = c("sfari_id" = "individual")) %>%
  left_join(select(svip$abcl_18_59, abcl_18_59.thought_t_score, individual),
            by = c("sfari_id" = "individual")) %>%
  left_join(select(svip$scl_90_r, scl_90_r.measure.eval_age_months, individual),
            by = c("sfari_id" = "individual")) %>%
  left_join(select(svip$disc_youth, disc_youth.measure.measure_type, individual),
            by = c("sfari_id" = "individual")) %>%
  left_join(select(svip$sops, eval_age_months, individual),
            by = c("sfari_id" = "individual")) %>%
  left_join(select(svip$mullen, mullen.measure.measure_type, individual),
            by = c("sfari_id" = "individual")) %>%
  left_join(select(svip$mullen, mullen.express_lang_t, individual),
            by = c("sfari_id" = "individual")) %>%
  left_join(select(svip$mullen, mullen.fine_motor_t, individual),
            by = c("sfari_id" = "individual")) %>%
  left_join(select(svip$bapq, bapq.total, individual),
            by = c("sfari_id" = "individual")) %>%
  left_join(select(svip$scq_life, scq_life.summary_score, individual),
            by = c("sfari_id" = "individual")) %>%
  left_join(select(svip$srs_adult, srs_adult.total, individual),
            by = c("sfari_id" = "individual")) %>%
  left_join(select(svip$srs_parent, srs_parent.total, individual),
            by = c("sfari_id" = "individual")) %>%
  left_join(select(svip$das_ii_early_years, das_ii_early_years.verbal_lower_standard, individual),
            by = c("sfari_id" = "individual")) %>%
  left_join(select(svip$das_ii_early_years, das_ii_early_years.verbal_upper_standard, individual),
            by = c("sfari_id" = "individual")) %>%
  left_join(select(svip$das_ii_early_years, das_ii_early_years.nonverbal_reasoning_standard, individual),
            by = c("sfari_id" = "individual")) %>%
  left_join(select(svip$das_ii_school_age, das_ii_school_age.nonverbal_reasoning_standard, individual),
            by = c("sfari_id" = "individual")) %>%
  left_join(select(svip$das_ii_school_age, das_ii_school_age.verbal_standard, individual),
            by = c("sfari_id" = "individual")) %>%
  left_join(select(svip$das_ii_early_years, das_ii_early_years.measure.measure_type, individual),
            by = c("sfari_id" = "individual")) %>%
  left_join(select(svip$wasi, wasi.performance_composite, individual),
            by = c("sfari_id" = "individual")) %>%
  left_join(select(svip$wasi, wasi.verbal_comprehension_composite, individual),
            by = c("sfari_id" = "individual")) %>%
  left_join(select(svip$wasi, wasi.verbal_comprehension_composite, individual),
            by = c("sfari_id" = "individual")) %>%
  left_join(select(svip$das_ii_school_age, das_ii_school_age.measure.measure_type, individual),
          by = c("sfari_id" = "individual")) 

# Abbreviate name of age variable
svip_summary$age_months <- svip_summary$svip_summary_variables.age_months
svip_summary$svip_summary_variables.age_months <- NULL

# Establish distinction between `sops` and `sops_missingness`
svip_summary <- svip_summary %>% mutate(
  sops_missingness = sops_positive
)
svip_summary$sops_positive <- na.replace(svip_summary$sops_positive, 0)

# Determine how many participants we have data for by each measure.
# Also generate variables for sorting.
svip_summary <- svip_summary %>% mutate(
  bcl_missingness = case_when(abcl_missingness == 1 ~ 1,
                              cbcl_missingness == 1 ~ 1,
                              abcl_missingness == 0 ~ 0,
                              cbcl_missingness == 0 ~ 0)) %>% mutate(
  bcl_positive = if_else(abcl_positive > 0 | 
            cbcl_positive > 0, 1, 0, 0)) %>% mutate(
  bcl_data = case_when(!is.na(bcl_missingness) ~ 1,
                       is.na(bcl_missingness) ~ 0)) %>% mutate( 
  scl_data = case_when(!is.na(scl_missingness_binary) ~ 1,
                       is.na(scl_missingness_binary) ~ 0)) %>% mutate( 
  sops_data = case_when(!is.na(sops_missingness) ~ 1,
                        is.na(sops_missingness) ~ 0)) %>% mutate( 
  disc_data = case_when(!is.na(disc_missingness) ~ 1,
                        is.na(disc_missingness) ~ 0)) %>% mutate( 
  number_of_items = bcl_data + scl_data + sops_data + disc_data) %>% mutate(
  adult = if_else(age_months >= 216, 1, 0, 0)) %>% mutate(
  noncarrier_family_member = 
    if_else(genetic_status_16p == "negative", 1, 0, 0)) %>% mutate(
  nonfamilial_control = 
    if_else(family_type == "non-familial-control", 1, 0, 0)) %>% mutate(
  no_mutation =
    if_else(noncarrier_family_member == 1 | 
            nonfamilial_control == 1, 1, 0, 0)) %>% mutate(
  deletion = 
    if_else(genetic_status_16p == "deletion", 1, 0, 0)) %>% mutate(
  duplication_or_triplication = 
    if_else(genetic_status_16p == "duplication" |
            genetic_status_16p == "triplication", 1, 0, 0)) %>% mutate(
  duplication = 
    if_else(genetic_status_16p == "duplication", 1, 0, 0)) %>% mutate(
  mutation = 
    if_else(deletion == 1 | 
            duplication_or_triplication == 1, 1, 0, 0)) %>% mutate(
  deletion_or_duplication = 
    if_else(deletion == 1 | duplication == 1, 1, 0, 0)) %>% mutate(
  gender =
    if_else(sex == "female", 1, 0, 0)) 

# Create binary psychotic symptom presence variable
svip_summary <- svip_summary %>% mutate(
  binary_psychosis =
    if_else((bcl_positive + scl_positive + disc_positive + sops_positive > 1), 1, 0, 0))

# Binary psychotic symptom presence if ABCL/CBCL is removed
svip_summary <- svip_summary %>% mutate(
  binary_psychosis_no_bcl =
    if_else((scl_positive + disc_positive + sops_positive > 1), 1, 0, 0))

# Total set of deletions, duplications, and noncarrier family members
svip_summary <- svip_summary %>% mutate(
  total_sample =
    if_else(duplication == 1 | deletion == 1 | noncarrier_family_member == 1, 1, 0, 0))

# Create factors for group comparisons
svip_summary <- svip_summary %>% mutate(
  sample_group = ifelse(duplication == 1, "duplication",
                        ifelse(deletion == 1, "deletion",
                               ifelse(noncarrier_family_member==1, 
                                      "noncarrier", NA)))) 

svip_summary$sample_group <- as.factor(svip_summary$sample_group)

svip_summary <- svip_summary %>% mutate(
  sample_collapsed = 
    ifelse(duplication == 1 | deletion == 1, ",mutation",
           ifelse(noncarrier_family_member==1, "noncarrier", NA)))

svip_summary$sample_collapsed <- as.factor(svip_summary$sample_collapsed)