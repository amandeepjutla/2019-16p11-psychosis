# 0_initialize.r
# Last updated 20190416

# checkpoint will look for (and, if necessary, install) 
# all packages except fifer, which is not on MRAN).
library(checkpoint) 
checkpoint("2018-11-25")

library(here) # set top level of current project
library(tidyverse) # mutate, filter, etc
library(broom) # tidy
library(geepack) #geeglm
library(imputeTS) # na.replace
library(conflicted) # conflict_prefer
library(arm) # rescale

# Uncomment the below 2 lines to install fifer from GitHub:
# library(devtools) # install_github
# devtools:install_github("dustinfife/fifer")

library(fifer) # chisq.post.hoc

# These scripts pull data from two sources. "SVIP_CLEAN" is the 
# location of the extracted `Simons_VIP__16p11.2_Dataset_v10.0` 
# archive downloadable from the SFARI Base portal. "SVIP_RAW" 
# is the location of a single raw CSV file containing M-SOPS data, 
# which was not part of the "clean" data release, but which we received 
# by request from the Simons Foundation.

SVIP_CLEAN <- here("data","clean","Simons_VIP__16p11.2_Dataset_v10.0")
SVIP_RAW <- here("data","raw","sops.csv")

# Threshold for CBCL/ABCL Thought Problems T Score.
# 60 is 1 SD above mean (and was used in our main analyses).
# 70 is 2 SDs (more stringent, used in sensitivity analyses).

BCL_T_SCORE_CUTOFF = 60

# Give dplyr namespace priority
conflict_prefer("filter", "dplyr") 
conflict_prefer("select", "dplyr")

# Pull in data
source(here("r","1_import.r")) 

# Derive ADOS RRB and SA Calibrated Severity Scores
source(here("r","2_derive_ados_domain_css.r"))

# Derive index of positive psychotic symptoms
source(here("r","3_derive_psychosis_index.r"))

# Create groups for regressions
source(here("r","4_create_analysis_groups.r"))

# Run analyses and save data as series of CSV files
source(here("r","5_run_analyses.r"))