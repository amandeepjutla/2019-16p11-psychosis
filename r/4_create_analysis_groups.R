# create_analysis_groups.r

# Raw set of mutation carriers and non-carrier family members 
subjects <- list()
subjects$raw <- list()
subjects$raw$total <- svip_summary %>% filter(
  duplication == 1 | deletion == 1 | noncarrier_family_member == 1)

# Remove 3 observations with indeterminate ASD status
# > "14838.x3, 14711.x8, and 14893.x7 (16p11.2 collection): Have removed ADOS 
# > data and unclear ASD diagnosis due to ADOS administration or scoring issues,
# > and should not be used in analyses concerning ASD status. A missing ASD
# > Diagnosis in these individuals does not indicate a non-ASD diagnosis. Their
# > other clinical DSM diagnoses (such as language disorder, coordination, etc.) 
# > are considered accurate. Other cases missing ADOS data were intentional due 
# > to validity concerns secondary to age or motor/other disabilities."
# > -- Simons VIP Collection Version 10, Data Manager Release Notes, 20150330

SUBJECTS_INDETERMINATE_ASD <- c("14838.x3", "14711.x8", "14893.x7")

subjects$raw$total <- subjects$raw$total %>% filter(
  !(sfari_id %in% SUBJECTS_INDETERMINATE_ASD))

# Remove observations without IQ:
# > "[MHI = medical history interview which you did not use]
# > 14735.x3 -- great grandparent - mailed questionnaires and/or did MHI only
# > 14759.x8 -- grandmother - mailed questionnaires and/or did MHI only
# > 14762.x28 -- invalid -- non-english speaker
# > 14762.x29 -- proband -- invalid -- does not speak english
# > 14767.x10 -- grandmother -- mailed questionnaires and/or did MHI only
# > 14767.x24 -- aunt -- did MHI only
# > 14770.x15 -- cousin -- no data -- definitely exclude 
# > 14770.x5 -- grandfather -- no data -- definitely exclude
# > 14872.x38 -- uncle -- MHI only
# > 14872.x39 -- aunt -- MHI only
# > 14872.x42 -- cousin -- MHI only 
# > 14907.x3 -- mom -- MHI only
# > 14907.x7 -- proband -- MHI only 
# > 14925.x3, 14945.x12, 14945.x13, 14945.x14, 14945.x7, 14858.x6, 
# > 14924.x1, 15021.x21, 15077.x7 - etc...."
# > -- LeeAnne Snyder, personal communication, 20190318

SUBJECTS_NO_IQ <- 
  c("14735.x3", "14759.x8", "14762.x28", "14762.x29", "14767.x10",
    "14767.x24", "14770.x15", "14770.x5", "14872.x38", "14872.x39",
    "14872.x42", "14907.x3", "14907.x7", "14925.x3", "14945.x12",
    "14945.x13", "14945.x14", "14945.x7", "14858.x6", "14924.x1",
    "15021.x21", "15077.x7")

subjects$raw$total <- subjects$raw$total %>% filter(
  !(sfari_id %in% SUBJECTS_NO_IQ))

# Remove observations missing age and any others missing IQ
subjects$raw$total <- subjects$raw$total %>% filter(
  !is.na(diagnosis_summary.best_full_scale_iq))

# Carrier-defined subgroups of total sample
subjects$raw$duplication <- subjects$raw$total %>% filter(
  duplication==1)
subjects$raw$deletion <- subjects$raw$total %>% filter(
  deletion==1)
subjects$raw$noncarrier <- subjects$raw$total %>% filter(
  noncarrier_family_member==1)

# Create explicit analysis sample (although not different from total sample)
subjects$analysis <- list()
subjects$analysis$all <- subjects$raw$total 

# Create subgroups
subjects$analysis$duplication <- subjects$analysis$all %>% filter(
  duplication == 1)
subjects$analysis$deletion <- subjects$analysis$all %>% filter(
  deletion == 1)
subjects$analysis$mutation <- subjects$analysis$all %>% filter(
  duplication == 1 | deletion == 1)
subjects$analysis$noncarrier <- subjects$analysis$all %>% filter(
  noncarrier_family_member == 1)

# Transform age into years

subjects$analysis$all <- subjects$analysis$all %>% mutate(
  age_years = age_months/12
)

subjects$analysis$duplication <- subjects$analysis$duplication %>% mutate(
  age_years = age_months/12
)

subjects$analysis$deletion <- subjects$analysis$deletion %>% mutate(
  age_years = age_months/12
)

subjects$analysis$noncarrier <- subjects$analysis$noncarrier %>% mutate(
  age_years = age_months/12
)
