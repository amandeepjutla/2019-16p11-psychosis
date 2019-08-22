# 0_initialize.r
# Last updated 20190822

# checkpoint will look for (and, if necessary, install) 
# packages from the snapshot date 
library(checkpoint) 
checkpoint("2018-11-25")

library(here) # set top level of current project
library(tidyverse) # mutate, filter, etc
library(broom) # tidy
library(geepack) #geeglm
library(imputeTS) # na.replace
library(conflicted) # conflict_prefer
library(arm) # rescale

# This function is adapted from Dustin Fife's "fifer" package, which is not
# on MRAN/CRAN but is available at https://github.com/dustinfife/fifer
chisq.post.hoc <- function(
    tbl,
    test=c("fisher.test"), 
    popsInRows=TRUE,
    control=c("fdr","BH","BY","bonferroni","holm","hochberg","hommel"),
    digits=4, ...) {
        control <- match.arg(control) # extract correction method
        test = match.fun(test) # extract which test (fisher or chi square)

        if (!popsInRows) tbl <- t(tbl)  # test rows or columns
        popsNames <- rownames(tbl)
        
        prs <- combn(1:nrow(tbl),2) # come up with all possible comparisons
        
        tests <- ncol(prs) # preallocate
        pvals <- numeric(tests)
        lbls <- character(tests)
        
        for (i in 1:tests) {
            pvals[i] <- test(tbl[prs[,i],], ...)$p.value
            lbls[i] <- paste(popsNames[prs[,i]],collapse=" vs. ")
        }
    
        adj.pvals <- p.adjust(pvals,method=control)
        cat("Adjusted p-values used the",control,"method.\n\n")
        data.frame(
            comparison=lbls,
            raw.p=round(pvals,digits),
            adj.p=round(adj.pvals,digits))
}

# These scripts pull data from two sources. "SVIP_CLEAN" is the 
# location of the extracted `Simons_VIP__16p11.2_Dataset_v10.0` 
# archive downloadable from the SFARI Base portal. "SVIP_RAW" 
# is the location of a single raw CSV file containing M-SOPS data, 
# which was not part of the "clean" data release, but which we received 
# by request from the Simons Foundation.
SVIP_CLEAN <- here("data","clean","Simons_VIP__16p11.2_Dataset_v10.0")
SVIP_RAW <- here("data","raw","sops.csv")

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