# 1_import.r

# Location and filenames for clean data release
csv_paths <- list.files(SVIP_CLEAN, 
                        pattern = "csv$", full.names = TRUE)
csv_filenames <- list.files(SVIP_CLEAN, 
                            pattern = "csv$", full.names = FALSE)

# Append location and filename for raw M-SOPS data
csv_paths <- c(csv_paths, SVIP_RAW) 
csv_filenames <- c(csv_filenames, "sops.csv")

# Generate and name dataframes
svip <- csv_paths %>% map(read_csv)
names(svip) <- gsub(".csv","", csv_filenames, fixed = TRUE)

# Manually insert missing age data for specific subjects into column 7 
# of svip_summary_variables (`age_months`):
# > "15059.x1 - I have an age of 8 yrs 4 mos. [100]
# > 15061.x1 -   17 yrs 11 mos [215]"
# > 15033.x3, - 45 years [540]
# > 15033.x6, - 47 years [564]
# > 15033.x7, -67 months [67]
# > 15037.x1, - 99 months [99]
# > 15061.x2, -118 months [118]
# > 15072.x7, - 163 months [163]
# > 15072.x8 -  134 months [134]
# -- Data provided by LeeAnne Snyder, personal communication, 20190318
svip$svip_summary_variables[svip$svip_summary_variables$individual==
                              "15059.x1",7]<-100
svip$svip_summary_variables[svip$svip_summary_variables$individual==
                              "15061.x1",7]<-215
svip$svip_summary_variables[svip$svip_summary_variables$individual==
                              "15033.x3",7]<-540
svip$svip_summary_variables[svip$svip_summary_variables$individual==
                              "15033.x6",7]<-564
svip$svip_summary_variables[svip$svip_summary_variables$individual==
                              "15033.x7",7]<-67
svip$svip_summary_variables[svip$svip_summary_variables$individual==
                              "15037.x1",7]<-99
svip$svip_summary_variables[svip$svip_summary_variables$individual==
                              "15061.x2",7]<-118
svip$svip_summary_variables[svip$svip_summary_variables$individual==
                              "15072.x7",7]<-163
svip$svip_summary_variables[svip$svip_summary_variables$individual==
                              "15072.x8",7]<-134

# Clean up temporary variables
rm(csv_filenames)
rm(csv_paths)