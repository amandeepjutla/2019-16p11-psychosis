# 2_derive_ados_domain_css.r

ados1 <- svip$ados_1
ados2 <- svip$ados_2
ados3 <- svip$ados_3
ados4 <- svip$ados_4

ados1 <- filter(svip$ados_1, ados_1.algorithm=="no-words")
ados1 <- filter(svip$ados_1, ados_1.algorithm=="some-words")

# For modules 1 through 3, derive CSS (domain-specific and total) from 
# existing raw total data using the methods described in:
# 1.  Hus V, Gotham K, Lord C. Standardizing ADOS domain scores: separating 
#     severity of social affect and restricted and repetitive behaviors. 
#     J Autism Dev Disord. 2014;44(10):2400-2412. doi:10.1007/s10803-012-1719-1
# 2.  Gotham K, Pickles A, Lord C. Standardizing ADOS scores for a measure 
#     of severity in autism spectrum disorders. J Autism Dev Disord. 
#     2009;39(5):693-705. doi:10.1007/s10803-008-0674-3

ados1$rrb_css <- NA
ados1$sa_css <- NA
ados1$total_css <- NA

# Module 1 (algorithm for "no words")
for (row in 1:nrow(ados1)) {
  age <- ados1[row, "ados_1.measure.eval_age_months"]
  rrb <- ados1[row, "ados_1.restricted_repetitive"]
  sa <- ados1[row, "ados_1.social_affect"]
  total <- ados1[row, "ados_1.total"]
  
  if (ados1[row, "ados_1.algorithm"] == "no-words") {

    if (is.na(age)) { 
      ados1[row, "rrb_css"] <- NA
      ados1[row, "sa_css"] <- NA
    }
    
    # Age 2 (ie, under 3)
    else if (age < 36) {
      if (rrb == 0) { ados1[row, "rrb_css"] <- 1 }
      else if (rrb == 1) { ados1[row, "rrb_css"] <- 4 }
      else if (rrb == 2) { ados1[row, "rrb_css"] <- 6 }
      else if (rrb == 3) { ados1[row, "rrb_css"] <- 7 }
      else if (rrb == 4) { ados1[row, "rrb_css"] <- 8 }
      else if (rrb == 5) { ados1[row, "rrb_css"] <- 9 }
      else if (rrb >= 6) { ados1[row, "rrb_css"] <- 10 }
      
      if (sa == 0 | sa == 1 | sa == 2 | sa == 3) { 
        ados1[row, "sa_css"] <- 1 
      }
      else if (sa == 4 | sa == 5) { ados1[row, "sa_css"] <- 2 }
      else if (sa == 6 | sa == 7 | sa == 8) { 
        ados1[row, "sa_css"] <- 3 
      }
      else if (sa == 9) { ados1[row, "sa_css"] <- 4 }
      else if (sa == 10 | sa == 11 | sa == 12 | sa == 13) { 
        ados1[row, "sa_css"] <- 5 
      }
      else if (sa == 14 | sa == 15 | sa == 16) { 
        ados1[row, "sa_css"] <- 6 
      }
      else if (sa == 17) { ados1[row, "sa_css"] <- 7 }
      else if (sa == 18) { ados1[row, "sa_css"] <- 8 }
      else if (sa == 19) { ados1[row, "sa_css"] <- 9 }
      else if (sa == 20) { ados1[row, "sa_css"] <- 10 }
    }
    
    # Age 3 (at least 3 and under 4)
    else if (age >= 36 & age < 48) {
      if (rrb == 0) { ados1[row, "rrb_css"] <- 1 }
      else if (rrb == 1) { ados1[row, "rrb_css"] <- 4 }
      else if (rrb == 2 | rrb == 3) { ados1[row, "rrb_css"] <- 6 }
      else if (rrb == 4) { ados1[row, "rrb_css"] <- 7 }
      else if (rrb == 5) { ados1[row, "rrb_css"] <- 8 }
      else if (rrb == 6) { ados1[row, "rrb_css"] <- 9 }
      else if (rrb >= 7) { ados1[row, "rrb_css"] <- 10 }
      
      if (sa == 0 | sa == 1 | sa == 2 | sa == 3) { 
        ados1[row, "sa_css"] <- 1 
      }
      else if (sa == 4 | sa == 5) { ados1[row, "sa_css"] <- 2 }
      else if (sa == 6 | sa == 7 | sa == 8 | sa == 9) { 
        ados1[row, "sa_css"] <- 3 
      }
      else if (sa == 10) { ados1[row, "sa_css"] <- 4 }
      else if (sa == 11 | sa == 12) { ados1[row, "sa_css"] <- 5 }
      else if (sa == 13 | sa == 14 | sa == 15 | sa == 16) { 
        ados1[row, "sa_css"] <- 6 
      }
      else if (sa == 17) { ados1[row, "sa_css"] <- 7 }
      else if (sa == 18) { ados1[row, "sa_css"] <- 8 }
      else if (sa == 19) { ados1[row, "sa_css"] <- 9 }
      else if (sa == 20) { ados1[row, "sa_css"] <- 10 }
      
    }
  
    # Age 4-14 (at least 4)  
    else if (age >= 48) {
      if (rrb == 0) { ados1[row, "rrb_css"] <- 1 }
      else if (rrb == 1 | rrb == 2) { ados1[row, "rrb_css"] <- 4 }
      else if (rrb == 3) { ados1[row, "rrb_css"] <- 6 }
      else if (rrb == 4) { ados1[row, "rrb_css"] <- 7 }
      else if (rrb == 5 | rrb == 6) { ados1[row, "rrb_css"] <- 8 }
      else if (rrb == 7) { ados1[row, "rrb_css"] <- 9 }
      else if (rrb == 8) { ados1[row, "rrb_css"] <- 10 }
      
      if (sa == 0 | sa == 1 | sa == 2) { ados1[row, "sa_css"] <- 1 }
      else if (sa == 3 | sa == 4 | sa == 5) { ados1[row, "sa_css"] <- 2 }
      else if (sa == 6 | sa == 7 | sa == 8 | sa == 9) {
        ados1[row, "sa_css"] <- 3 
      }
      else if (sa == 10) { ados1[row, "sa_css"] <- 4 }
      else if (sa == 11 | sa == 12) { ados1[row, "sa_css"] <- 5 }
      else if (sa == 13 | sa == 14) { ados1[row, "sa_css"] <- 6 }
      else if (sa == 15 | sa == 16) { ados1[row, "sa_css"] <- 7 }
      else if (sa == 17 | sa == 18) { ados1[row, "sa_css"] <- 8 }
      else if (sa == 19) { ados1[row, "sa_css"] <- 9 }
      else if (sa == 20) { ados1[row, "sa_css"] <- 10 }
    }
    
    # Total score
    
    if (is.na(age)) { 
      ados1[row, "total_css"] <- NA
    }
    
    else if (age < 36) {
      if (total == 0 | total == 1 | total == 2 | total == 3 |
          total == 4 | total == 5 | total == 6) { 
        ados1[row, "total_css"] <- 1 
      }
      else if (total == 7 | total == 8) { ados1[row, "total_css"] <- 2 }
      else if (total == 9 | total == 10) { ados1[row, "total_css"] <- 3 }
      else if (total == 11 | total == 12 | total == 13) { 
        ados1[row, "total_css"] <- 4 
      }
      else if (total == 14 | total == 15) { ados1[row, "total_css"] <- 5 }
      else if (total == 16 | total == 17 | total == 18 | total == 19) { 
        ados1[row, "total_css"] <- 6 }
      else if (total == 20 | total == 21) { ados1[row, "total_css"] <- 7 }
      else if (total == 22) { ados1[row, "total_css"] <- 8 }
      else if (total == 23 | total == 24) { ados1[row, "total_css"] <- 9 }
      else if (total >= 25) { ados1[row, "total_css"] <- 10 }
    }
  
    else if (age >= 36 & age < 48) {
      if (total == 0 | total == 1 | total == 2 | total == 3 |
          total == 4 | total == 5 | total == 6) { 
        ados1[row, "total_css"] <- 1 
      }
      else if (total == 7 | total == 8) { ados1[row, "total_css"] <- 2 }
      else if (total == 9 | total == 10) { ados1[row, "total_css"] <- 3 }
      else if (total == 11 | total == 12 | total == 13 | total == 14) { 
        ados1[row, "total_css"] <- 4 
      }
      else if (total == 15) { ados1[row, "total_css"] <- 5 }
      else if (total == 16 | total == 17 | total == 18 | total == 19 |
               total == 20) { 
        ados1[row, "total_css"] <- 6 }
      else if (total == 21 | total == 22) { ados1[row, "total_css"] <- 7 }
      else if (total == 23) { ados1[row, "total_css"] <- 8 }
      else if (total == 24) { ados1[row, "total_css"] <- 9 }
      else if (total >= 25) { ados1[row, "total_css"] <- 10 }
    }
    
    # Age 4-5 (at least 4 and under 6)  
    else if (age >= 48 & age < 72) {  
      if (total == 0 | total == 1 | total == 2 | total == 3) { 
        ados1[row, "total_css"] <- 1 
      }
      else if (total == 4 | total == 5 | total == 6) { ados1[row, "total_css"] <- 2 }
      else if (total == 7 | total == 8 | total == 9 | total == 10) { ados1[row, "total_css"] <- 3 }
      else if (total == 11 | total == 12) { 
        ados1[row, "total_css"] <- 4 
      }
      else if (total == 13 | total == 14 | total == 15) { ados1[row, "total_css"] <- 5 }
      else if (total == 16 | total == 17 | total == 18 | total == 19) { 
        ados1[row, "total_css"] <- 6 }
      else if (total == 20 | total == 21) { ados1[row, "total_css"] <- 7 }
      else if (total == 22 | total == 23) { ados1[row, "total_css"] <- 8 }
      else if (total == 24 | total == 25) { ados1[row, "total_css"] <- 9 }
      else if (total >= 26) { ados1[row, "total_css"] <- 10 }
    }
    
    # Age 6-14 (at least 6 and under 15)  
    else if (age >= 72) {  
      if (total == 0 | total == 1 | total == 2 | total == 3) { 
        ados1[row, "total_css"] <- 1 
      }
      else if (total == 4 | total == 5 | total == 6) { ados1[row, "total_css"] <- 2 }
      else if (total == 7 | total == 8 | total == 9 | total == 10) { ados1[row, "total_css"] <- 3 }
      else if (total == 11 | total == 12 | total == 13) { 
        ados1[row, "total_css"] <- 4 
      }
      else if (total == 14 | total == 15) { ados1[row, "total_css"] <- 5 }
      else if (total == 16 | total == 17 | total == 18 | total == 19) { 
        ados1[row, "total_css"] <- 6 }
      else if (total == 20 | total == 21 | total == 22) { ados1[row, "total_css"] <- 7 }
      else if (total == 23 | total == 24) { ados1[row, "total_css"] <- 8 }
      else if (total == 25) { ados1[row, "total_css"] <- 9 }
      else if (total >= 26) { ados1[row, "total_css"] <- 10 }
    }
  }
  
  # Module 1 (algorithm for "some words")
  else if (ados1[row, "ados_1.algorithm"] == "some-words") {

      if (is.na(age)) { 
        ados1[row, "rrb_css"] <- NA
        ados1[row, "sa_css"] <- NA
      }
    
      # Age 2-3 (ie, under 4)
      else if (age < 48) {
        if (rrb == 0) { ados1[row, "rrb_css"] <- 1 }
        else if (rrb == 1) { ados1[row, "rrb_css"] <- 4 }
        else if (rrb == 2) { ados1[row, "rrb_css"] <- 6 }
        else if (rrb == 3) { ados1[row, "rrb_css"] <- 7 }
        else if (rrb == 4) { ados1[row, "rrb_css"] <- 8 }
        else if (rrb == 5) { ados1[row, "rrb_css"] <- 9 }
        else if (rrb >= 6) { ados1[row, "rrb_css"] <- 10 }
        
        if (sa == 0 | sa == 1) { 
          ados1[row, "sa_css"] <- 1 
        }
        else if (sa == 2 | sa == 3) { ados1[row, "sa_css"] <- 2 }
        else if (sa == 4 | sa == 5) { 
          ados1[row, "sa_css"] <- 3 
        }
        else if (sa == 6 | sa == 7) { ados1[row, "sa_css"] <- 4 }
        else if (sa == 8 | sa == 9) { 
          ados1[row, "sa_css"] <- 5 
        }
        else if (sa == 10 | sa == 11 | sa == 12) { 
          ados1[row, "sa_css"] <- 6 
        }
        else if (sa == 13) { ados1[row, "sa_css"] <- 7 }
        else if (sa == 14 | sa == 15) { ados1[row, "sa_css"] <- 8 }
        else if (sa == 16 | sa == 17) { ados1[row, "sa_css"] <- 9 }
        else if (sa >= 18) { ados1[row, "sa_css"] <- 10 } 
      }
      
      # Age 4 (at least 4 and under 5)
      if (age >= 48 & age < 60) {
        if (rrb == 0) { ados1[row, "rrb_css"] <- 1 }
        else if (rrb == 1) { ados1[row, "rrb_css"] <- 4 }
        else if (rrb == 2) { ados1[row, "rrb_css"] <- 6 }
        else if (rrb == 3 | rrb == 4) { ados1[row, "rrb_css"] <- 7 }
        else if (rrb == 5) { ados1[row, "rrb_css"] <- 8 }
        else if (rrb == 6) { ados1[row, "rrb_css"] <- 9 }
        else if (rrb >= 7) { ados1[row, "rrb_css"] <- 10 }
        
        if (sa == 0 | sa == 1) { 
          ados1[row, "sa_css"] <- 1 
        }
        else if (sa == 2 | sa == 3) { ados1[row, "sa_css"] <- 2 }
        else if (sa == 4 | sa == 5) { 
          ados1[row, "sa_css"] <- 3 
        }
        else if (sa == 6 | sa == 7) { ados1[row, "sa_css"] <- 4 }
        else if (sa == 8 | sa == 9) { 
          ados1[row, "sa_css"] <- 5 
        }
        else if (sa == 10 | sa == 11 | sa == 12) { 
          ados1[row, "sa_css"] <- 6 
        }
        else if (sa == 13) { ados1[row, "sa_css"] <- 7 }
        else if (sa == 14 | sa == 15) { ados1[row, "sa_css"] <- 8 }
        else if (sa == 16 | sa == 17) { ados1[row, "sa_css"] <- 9 }
        else if (sa >= 18) { ados1[row, "sa_css"] <- 10 } 
      }
      
      # Age 5+
      if (age >= 60) {
        if (rrb == 0) { ados1[row, "rrb_css"] <- 1 }
        else if (rrb == 1) { ados1[row, "rrb_css"] <- 4 }
        else if (rrb == 2 | rrb == 3) { ados1[row, "rrb_css"] <- 6 }
        else if (rrb == 4) { ados1[row, "rrb_css"] <- 7 }
        else if (rrb == 5) { ados1[row, "rrb_css"] <- 8 }
        else if (rrb == 6) { ados1[row, "rrb_css"] <- 9 }
        else if (rrb >= 7) { ados1[row, "rrb_css"] <- 10 }
        
        if (sa == 0 | sa == 1) { 
          ados1[row, "sa_css"] <- 1 
        }
        else if (sa == 2 | sa == 3) { ados1[row, "sa_css"] <- 2 }
        else if (sa == 4 | sa == 5) { 
          ados1[row, "sa_css"] <- 3 
        }
        else if (sa == 6 | sa == 7) { ados1[row, "sa_css"] <- 4 }
        else if (sa == 8 | sa == 9) { 
          ados1[row, "sa_css"] <- 5 
        }
        else if (sa == 10 | sa == 11 | sa == 12 | sa == 13) { 
          ados1[row, "sa_css"] <- 6 
        }
        else if (sa == 14 | sa == 15) { ados1[row, "sa_css"] <- 7 }
        else if (sa == 16) { ados1[row, "sa_css"] <- 8 }
        else if (sa == 17 | sa == 18) { ados1[row, "sa_css"] <- 9 }
        else if (sa >= 19) { ados1[row, "sa_css"] <- 10 } 
      }
      
      # Total score
      
      # 2 (ie, under 3)
    
      if (is.na(age)) { 
        ados1[row, "total_css"] <- NA
      }
    
      else if (age < 36) {
        if (total == 0 | total == 1 | total == 2 | total == 3) { 
          ados1[row, "total_css"] <- 1 
        }
        else if (total == 4 | total == 5) { ados1[row, "total_css"] <- 2 }
        else if (total == 6 | total == 7) { ados1[row, "total_css"] <- 3 }
        else if (total == 8 | total == 9 | total == 10) { 
          ados1[row, "total_css"] <- 4 
        }
        else if (total == 11) { ados1[row, "total_css"] <- 5 }
        else if (total == 12 | total == 13) { 
          ados1[row, "total_css"] <- 6 }
        else if (total == 14 | total == 15 | total == 16) { ados1[row, "total_css"] <- 7 }
        else if (total == 17 | total == 18 | total == 19) { ados1[row, "total_css"] <- 8 }
        else if (total == 20 | total == 21) { ados1[row, "total_css"] <- 9 }
        else if (total >= 22) { ados1[row, "total_css"] <- 10 }
      }
      
      # 3 (ie, at least 3 and under 4)
      else if (age >= 36 & age < 48) {
        if (total == 0 | total == 1 | total == 2 | total == 3 | total == 4) { 
          ados1[row, "total_css"] <- 1 
        }
        else if (total == 5 | total == 6) { ados1[row, "total_css"] <- 2 }
        else if (total == 7) { ados1[row, "total_css"] <- 3 }
        else if (total == 8 | total == 9) { 
          ados1[row, "total_css"] <- 4 
        }
        else if (total == 10 | total == 11) { ados1[row, "total_css"] <- 5 }
        else if (total == 12 | total == 13 | total == 14) { 
          ados1[row, "total_css"] <- 6 }
        else if (total == 15 | total == 16 | total == 17) { ados1[row, "total_css"] <- 7 }
        else if (total == 18 | total == 19) { ados1[row, "total_css"] <- 8 }
        else if (total == 20 | total == 21) { ados1[row, "total_css"] <- 9 }
        else if (total >= 22) { ados1[row, "total_css"] <- 10 }
      }
      
      # 4 (at least 4 and under 5)
      else if (age >= 48 & age < 60) {
        if (total == 0 | total == 1 | total == 2) { 
          ados1[row, "total_css"] <- 1 
        }
        else if (total == 3 | total == 4) { ados1[row, "total_css"] <- 2 }
        else if (total == 5 | total == 6 | total == 7) { ados1[row, "total_css"] <- 3 }
        else if (total == 8 | total == 9) { 
          ados1[row, "total_css"] <- 4 
        }
        else if (total == 10 | total == 11) { ados1[row, "total_css"] <- 5 }
        else if (total == 12 | total == 13 | total == 14 | total == 15) { 
          ados1[row, "total_css"] <- 6 }
        else if (total == 16 | total == 17 | total == 18) { ados1[row, "total_css"] <- 7 }
        else if (total == 19 | total == 20) { ados1[row, "total_css"] <- 8 }
        else if (total == 21 | total == 22) { ados1[row, "total_css"] <- 9 }
        else if (total >= 23) { ados1[row, "total_css"] <- 10 }
      }
      
      # 5-6 (at least 5 and under 7)
      else if (age >= 60 & age < 84) {
        if (total == 0 | total == 1 | total == 2) { 
          ados1[row, "total_css"] <- 1 
        }
        else if (total == 3 | total == 4) { ados1[row, "total_css"] <- 2 }
        else if (total == 5 | total == 6 | total == 7) { ados1[row, "total_css"] <- 3 }
        else if (total == 8 | total == 9 | total == 10) { 
          ados1[row, "total_css"] <- 4 
        }
        else if (total == 11) { ados1[row, "total_css"] <- 5 }
        else if (total == 12 | total == 13 | total == 14 | total == 15 | total == 16) { 
          ados1[row, "total_css"] <- 6 }
        else if (total == 17 | total == 18 | total == 19) { ados1[row, "total_css"] <- 7 }
        else if (total == 20 | total == 21) { ados1[row, "total_css"] <- 8 }
        else if (total == 22 | total == 23) { ados1[row, "total_css"] <- 9 }
        else if (total >= 24) { ados1[row, "total_css"] <- 10 }
      }
      
      # 7-14 (at least 7)
      else if (age >= 84) {
        if (total == 0 | total == 1 | total == 2) { 
          ados1[row, "total_css"] <- 1 
        }
        else if (total == 3 | total == 4 | total == 5) { ados1[row, "total_css"] <- 2 }
        else if (total == 6 | total == 7) { ados1[row, "total_css"] <- 3 }
        else if (total == 8 | total == 9) { 
          ados1[row, "total_css"] <- 4 
        }
        else if (total == 10 | total == 11) { ados1[row, "total_css"] <- 5 }
        else if (total == 12 | total == 13 | total == 14 | 
                 total == 15 | total == 16 | total == 17 | total == 18) { 
          ados1[row, "total_css"] <- 6 }
        else if (total == 19 | total == 20) { ados1[row, "total_css"] <- 7 }
        else if (total == 20 | total == 21) { ados1[row, "total_css"] <- 8 }
        else if (total == 22 | total == 23) { ados1[row, "total_css"] <- 9 }
        else if (total >= 24) { ados1[row, "total_css"] <- 10 }
      }
    }

    
}

# Module 2
ados2$rrb_css <- NA
ados2$sa_css <- NA
ados2$total_css <- NA

for (row in 1:nrow(ados2)) {
  age <- ados2[row, "ados_2.measure.eval_age_months"]
  rrb <- ados2[row, "ados_2.restricted_repetitive"]
  sa <- ados2[row, "ados_2.social_affect"]
  total <- ados2[row, "ados_2.total"]
  
  # Age 2-3 (ie, under 4)
  
  if (is.na(age)) { 
    ados2[row, "rrb_css"] <- NA
    ados2[row, "sa_css"] <- NA
  }
  
  else if (age < 48) {
    if (rrb == 0) { ados2[row, "rrb_css"] <- 1 }
    else if (rrb == 1) { ados2[row, "rrb_css"] <- 4 }
    else if (rrb == 2) { ados2[row, "rrb_css"] <- 6 }
    else if (rrb == 3) { ados2[row, "rrb_css"] <- 7 }
    else if (rrb == 4) { ados2[row, "rrb_css"] <- 8 }
    else if (rrb == 5 | rrb == 6) { ados2[row, "rrb_css"] <- 9 }
    else if (rrb >= 7) { ados2[row, "rrb_css"] <- 10 }
    
    if (sa == 0 | sa == 1) { ados2[row, "sa_css"] <- 1 }
    else if (sa == 2 | sa == 3) { ados2[row, "sa_css"] <- 2 }
    else if (sa == 4) { ados2[row, "sa_css"] <- 3 }
    else if (sa == 5) { ados2[row, "sa_css"] <- 4 }
    else if (sa == 6) { ados2[row, "sa_css"] <- 5 }
    else if (sa == 7 | sa == 8) { ados2[row, "sa_css"] <- 6 }
    else if (sa == 9 | sa == 10) { ados2[row, "sa_css"] <- 7 }
    else if (sa == 11) { ados2[row, "sa_css"] <- 8 }
    else if (sa == 12 | sa == 13 | sa == 14) { ados2[row, "sa_css"] <- 9 }
    else if (sa == 15 | sa == 16 | sa == 17 | sa == 18 | sa == 19 | sa == 20) { 
      ados2[row, "sa_css"] <- 10 
    }
  }
  
  #4 (at least 4 and under 5)
  else if (age >= 48 & age < 60) {
    if (rrb == 0) { ados2[row, "rrb_css"] <- 1 }
    else if (rrb == 1) { ados2[row, "rrb_css"] <- 4 }
    else if (rrb == 2 | rrb == 3) { ados2[row, "rrb_css"] <- 6 }
    else if (rrb == 4) { ados2[row, "rrb_css"] <- 7 }
    else if (rrb == 5) { ados2[row, "rrb_css"] <- 8 }
    else if (rrb == 6) { ados2[row, "rrb_css"] <- 9 }
    else if (rrb >= 7) { ados2[row, "rrb_css"] <- 10 }
    
    if (sa == 0 | sa == 1) { ados2[row, "sa_css"] <- 1 }
    else if (sa == 2) { ados2[row, "sa_css"] <- 2 }
    else if (sa == 3 | sa == 4) { ados2[row, "sa_css"] <- 3 }
    else if (sa == 5 | sa == 6) { ados2[row, "sa_css"] <- 4 }
    else if (sa == 7) { ados2[row, "sa_css"] <- 5 }
    else if (sa == 8 | sa == 9) { ados2[row, "sa_css"] <- 6 }
    else if (sa == 10 | sa == 11) { ados2[row, "sa_css"] <- 7 }
    else if (sa == 12 | sa == 13) { ados2[row, "sa_css"] <- 8 }
    else if (sa == 14 | sa == 15) { ados2[row, "sa_css"] <- 9 }
    else if (sa == 16 | sa == 17 | sa == 18 | sa == 19 | sa == 20) { 
      ados2[row, "sa_css"] <- 10 
    }
  }
  
  #5-6 (at least 5 and under 7)
  else if (age >= 60 & age < 84) {
    if (rrb == 0) { ados2[row, "rrb_css"] <- 1 }
    else if (rrb == 1) { ados2[row, "rrb_css"] <- 4 }
    else if (rrb == 2 | rrb == 3) { ados2[row, "rrb_css"] <- 6 }
    else if (rrb == 4) { ados2[row, "rrb_css"] <- 7 }
    else if (rrb == 5) { ados2[row, "rrb_css"] <- 8 }
    else if (rrb == 6) { ados2[row, "rrb_css"] <- 9 }
    else if (rrb >= 7) { ados2[row, "rrb_css"] <- 10 }
    
    if (sa == 0 | sa == 1) { ados2[row, "sa_css"] <- 1 }
    else if (sa == 2 | sa == 3) { ados2[row, "sa_css"] <- 2 }
    else if (sa == 4 | sa == 5) { ados2[row, "sa_css"] <- 3 }
    else if (sa == 6) { ados2[row, "sa_css"] <- 4 }
    else if (sa == 7) { ados2[row, "sa_css"] <- 5 }
    else if (sa == 8 | sa == 9) { ados2[row, "sa_css"] <- 6 }
    else if (sa == 10 | sa == 11) { ados2[row, "sa_css"] <- 7 }
    else if (sa == 12 | sa == 13 | sa == 14 | sa == 15) { ados2[row, "sa_css"] <- 8 }
    else if (sa == 16) { ados2[row, "sa_css"] <- 9 }
    else if (sa == 17 | sa == 18 | sa == 19 | sa == 20) { 
      ados2[row, "sa_css"] <- 10 
    }
  }

  #7-16 (at least 7 and under 17)
  else if (age >= 84) {
    if (rrb == 0) { ados2[row, "rrb_css"] <- 1 }
    else if (rrb == 1) { ados2[row, "rrb_css"] <- 4 }
    else if (rrb == 2 | rrb == 3) { ados2[row, "rrb_css"] <- 6 }
    else if (rrb == 4) { ados2[row, "rrb_css"] <- 7 }
    else if (rrb == 5) { ados2[row, "rrb_css"] <- 8 }
    else if (rrb == 6) { ados2[row, "rrb_css"] <- 9 }
    else if (rrb >= 7) { ados2[row, "rrb_css"] <- 10 }
    
    if (sa == 0 | sa == 1) { ados2[row, "sa_css"] <- 1 }
    else if (sa == 2) { ados2[row, "sa_css"] <- 2 }
    else if (sa == 3 | sa == 4) { ados2[row, "sa_css"] <- 3 }
    else if (sa == 5) { ados2[row, "sa_css"] <- 4 }
    else if (sa == 6 | sa == 7) { ados2[row, "sa_css"] <- 5 }
    else if (sa == 8 | sa == 9 | sa == 10) { ados2[row, "sa_css"] <- 6 }
    else if (sa == 11 | sa == 12 | sa == 13) { ados2[row, "sa_css"] <- 7 }
    else if (sa == 14 | sa == 15) { ados2[row, "sa_css"] <- 8 }
    else if (sa == 16 | sa == 17) { ados2[row, "sa_css"] <- 9 }
    else if (sa == 18 | sa == 19 | sa == 20) { 
      ados2[row, "sa_css"] <- 10 
    }
  }
  
  if (is.na(age)) { 
    ados2[row, "total_css"] <- NA
  }
  
  else if (age < 36) {
    if (total == 0 | total == 1 | total == 2) { 
      ados2[row, "total_css"] <- 1 
    }
    else if (total == 3 | total == 4 | total == 5) { ados2[row, "total_css"] <- 2 }
    else if (total == 6) { ados2[row, "total_css"] <- 3 }
    else if (total == 7 | total == 8) { 
      ados2[row, "total_css"] <- 4 
    }
    else if (total == 9) { ados2[row, "total_css"] <- 5 }
    else if (total == 10 | total == 11) { 
      ados2[row, "total_css"] <- 6 }
    else if (total == 12) { ados2[row, "total_css"] <- 7 }
    else if (total == 13 | total == 14) { ados2[row, "total_css"] <- 8 }
    else if (total == 15 | total == 16 | total == 17) { ados2[row, "total_css"] <- 9 }
    else if (total >= 18) { ados2[row, "total_css"] <- 10 }
  }
  
  # Age 3
  else if (age >= 36 & age < 48) {
    if (total == 0 | total == 1 | total == 2 | total == 3) { 
      ados2[row, "total_css"] <- 1 
    }
    else if (total == 4 | total == 5) { ados2[row, "total_css"] <- 2 }
    else if (total == 6) { ados2[row, "total_css"] <- 3 }
    else if (total == 7 | total == 8) { 
      ados2[row, "total_css"] <- 4 
    }
    else if (total == 9) { ados2[row, "total_css"] <- 5 }
    else if (total == 10 | total == 11 | total == 12) { 
      ados2[row, "total_css"] <- 6 }
    else if (total == 13 | total == 14) { ados2[row, "total_css"] <- 7 }
    else if (total == 15 | total == 16) { ados2[row, "total_css"] <- 8 }
    else if (total == 17 | total == 18) { ados2[row, "total_css"] <- 9 }
    else if (total >= 19) { ados2[row, "total_css"] <- 10 }
  }

  # Age 4
  else if (age >= 48 & age < 60) {
    if (total == 0 | total == 1 | total == 2 | total == 3) { 
      ados2[row, "total_css"] <- 1 
    }
    else if (total == 4 | total == 5) { ados2[row, "total_css"] <- 2 }
    else if (total == 6) { ados2[row, "total_css"] <- 3 }
    else if (total == 7) { 
      ados2[row, "total_css"] <- 4 
    }
    else if (total == 8 | total == 9) { ados2[row, "total_css"] <- 5 }
    else if (total == 10 | total == 11 | total == 12 | total == 13) { 
      ados2[row, "total_css"] <- 6 }
    else if (total == 14 | total == 15 | total == 16) { ados2[row, "total_css"] <- 7 }
    else if (total == 17 | total == 18) { ados2[row, "total_css"] <- 8 }
    else if (total == 19 | total == 20) { ados2[row, "total_css"] <- 9 }
    else if (total >= 21) { ados2[row, "total_css"] <- 10 }
  }
  
  # Age 5-6
  
  else if (age >= 60 & age < 72) {
    if (total == 0 | total == 1 | total == 2 | total == 3) { 
      ados2[row, "total_css"] <- 1 
    }
    else if (total == 4 | total == 5) { ados2[row, "total_css"] <- 2 }
    else if (total == 6 | total == 7) { ados2[row, "total_css"] <- 3 }
    else if (total == 8) { 
      ados2[row, "total_css"] <- 4 
    }
    else if (total == 9 | total == 10 | total == 11 | total == 12 | total == 13 | total == 14) { 
      ados2[row, "total_css"] <- 6 }
    else if (total == 15 | total == 16) { 
      ados2[row, "total_css"] <- 7 }
    else if (total == 17 | total == 18 | total == 19 | total == 20) { ados2[row, "total_css"] <- 8 }
    else if (total == 21 | total == 22) { ados2[row, "total_css"] <- 9 }
    else if (total >= 23) { ados2[row, "total_css"] <- 10 }
  }
  
  # Age 7-8
  else if (age >= 84 & age < 96) {
    if (total == 0 | total == 1 | total == 2) { 
      ados2[row, "total_css"] <- 1 
    }
    else if (total == 3 | total == 4 | total == 5) { ados2[row, "total_css"] <- 2 }
    else if (total == 6 | total == 7) { ados2[row, "total_css"] <- 3 }
    else if (total == 8) { 
      ados2[row, "total_css"] <- 4 
    }
    else if (total == 9 | total == 10 | total == 11 | total == 12 | total == 13 | total == 14) { 
      ados2[row, "total_css"] <- 6 }
    else if (total == 15 | total == 16 | total == 17) { 
      ados2[row, "total_css"] <- 7 }
    else if (total == 18 | total == 19 | total == 20 | total == 21) { ados2[row, "total_css"] <- 8 }
    else if (total == 22 | total == 23) { ados2[row, "total_css"] <- 9 }
    else if (total >= 24) { ados2[row, "total_css"] <- 10 }
  }
  
  # Age 9-16
  else if (age >= 108) {
    if (total == 0 | total == 1 | total == 2) { 
      ados2[row, "total_css"] <- 1 
    }
    else if (total == 3 | total == 4 | total == 5) { ados2[row, "total_css"] <- 2 }
    else if (total == 6 | total == 7) { ados2[row, "total_css"] <- 3 }
    else if (total == 8) { 
      ados2[row, "total_css"] <- 4 
    }
    else if (total == 9 | total == 10 | total == 11 | total == 12 | total == 13 | total == 14) { 
      ados2[row, "total_css"] <- 6 }
    else if (total == 15 | total == 16 | total == 17) { 
      ados2[row, "total_css"] <- 7 }
    else if (total == 18 | total == 19 | total == 20) { ados2[row, "total_css"] <- 8 }
    else if (total == 21 | total == 22 | total == 23) { ados2[row, "total_css"] <- 9 }
    else if (total >= 24) { ados2[row, "total_css"] <- 10 }
  }
}

# Module 3
ados3$rrb_css <- NA
ados3$sa_css <- NA
ados3$total_css <- NA

for (row in 1:nrow(ados3)) {
  age <- ados3[row, "ados_3.measure.eval_age_months"]
  rrb <- ados3[row, "ados_3.restricted_repetitive"]
  sa <- ados3[row, "ados_3.social_affect"]
  total <- ados3[row, "ados_3.total"]
  
  # Age 3-5 (ie, under 6)
  if (is.na(age)) { 
    ados3[row, "rrb_css"] <- NA
    ados3[row, "sa_css"] <- NA
  }
  
  else if (age < 72) {
    if (rrb == 0) { ados3[row, "rrb_css"] <- 1 }
    else if (rrb == 1) { ados3[row, "rrb_css"] <- 4 }
    else if (rrb == 2) { ados3[row, "rrb_css"] <- 7 }
    else if (rrb == 3) { ados3[row, "rrb_css"] <- 8 }
    else if (rrb == 4) { ados3[row, "rrb_css"] <- 9 }
    else if (rrb >= 5) { ados3[row, "rrb_css"] <- 10 }
    
    if (sa == 0 | sa == 1 | sa == 2) { ados3[row, "sa_css"] <- 1 }
    else if (sa == 3) { ados3[row, "sa_css"] <- 2 }
    else if (sa == 4) { ados3[row, "sa_css"] <- 3 }
    else if (sa == 5) { ados3[row, "sa_css"] <- 4 }
    else if (sa == 6) { ados3[row, "sa_css"] <- 5 }
    else if (sa == 7 | sa == 8) { ados3[row, "sa_css"] <- 6 }
    else if (sa == 9 | sa == 10) { ados3[row, "sa_css"] <- 7 }
    else if (sa == 11 | sa == 12) { ados3[row, "sa_css"] <- 8 }
    else if (sa == 13 | sa == 14) { ados3[row, "sa_css"] <- 9 }
    else if (sa >= 15) { ados3[row, "sa_css"] <- 10 }
    
  }
  
  #Age 6-16 (at least 6)
  else if (age >= 72) {
    if (rrb == 0) { ados3[row, "rrb_css"] <- 1 }
    else if (rrb == 1) { ados3[row, "rrb_css"] <- 4 }
    else if (rrb == 2) { ados3[row, "rrb_css"] <- 7 }
    else if (rrb == 3) { ados3[row, "rrb_css"] <- 8 }
    else if (rrb == 4 | rrb == 5) { ados3[row, "rrb_css"] <- 9 }
    else if (rrb >= 6) { ados3[row, "rrb_css"] <- 10 }
    
    if (sa == 0 | sa == 1) { ados3[row, "sa_css"] <- 1 }
    else if (sa == 2) { ados3[row, "sa_css"] <- 2 }
    else if (sa == 3 | sa == 4) { ados3[row, "sa_css"] <- 3 }
    else if (sa == 5) { ados3[row, "sa_css"] <- 4 }
    else if (sa == 6) { ados3[row, "sa_css"] <- 5 }
    else if (sa == 7 | sa == 8) { ados3[row, "sa_css"] <- 6 }
    else if (sa == 8 | sa == 9) { ados3[row, "sa_css"] <- 7 }
    else if (sa == 10 | sa == 11) { ados3[row, "sa_css"] <- 8 }
    else if (sa == 12 | sa == 13 | sa == 14) { ados3[row, "sa_css"] <- 9 }
    else if (sa >= 15) { ados3[row, "sa_css"] <- 10 }
  }
  
  if (is.na(age)) { 
    ados3[row, "total_css"] <- NA
  }
  #2-5
  
  else if (age < 60) {
    if (total == 0 | total == 1 | total == 2 | total == 3) {
      ados3[row, "total_css"] < - 1
    }
    else if (total == 4) { ados3[row, "total_css"] <- 2}
    else if (total == 5 | total == 6) { ados3[row, "total_css"] <- 3}
    else if (total == 7) { ados3[row, "total_css"] <- 4}
    else if (total == 8) { ados3[row, "total_css"] <- 5}
    else if (total == 9 | total == 10 | total == 11) { ados3[row, "total_css"] <- 6}
    else if (total == 12) { ados3[row, "total_css"] <- 7}
    else if (total == 13 | total == 14 | total == 15) { ados3[row, "total_css"] <- 8}
    else if (total == 16 | total == 17) { ados3[row, "total_css"] <- 9}
    else if (total >= 18) { ados3[row, "total_css"] <- 10}
  }

  #6-9
  else if (age >= 72 & age < 108) {
    if (total == 0 | total == 1 | total == 2) {
      ados3[row, "total_css"] < - 1
    }
    else if (total == 3 | total == 4) { ados3[row, "total_css"] <- 2}
    else if (total == 5 | total == 6) { ados3[row, "total_css"] <- 3}
    else if (total == 7) { ados3[row, "total_css"] <- 4}
    else if (total == 8) { ados3[row, "total_css"] <- 5}
    else if (total == 9 | total == 10) { ados3[row, "total_css"] <- 6}
    else if (total == 11 | total == 12) { ados3[row, "total_css"] <- 7}
    else if (total == 13 | total == 14) { ados3[row, "total_css"] <- 8}
    else if (total == 15 | total == 16 | total == 17) { ados3[row, "total_css"] <- 9}
    else if (total >= 18) { ados3[row, "total_css"] <- 10}
  }
  
  else if (age >= 120) {
    if (total == 0 | total == 1 | total == 2 | total == 3) {
      ados3[row, "total_css"] < - 1
    }
    else if (total == 4) { ados3[row, "total_css"] <- 2}
    else if (total == 5 | total == 6) { ados3[row, "total_css"] <- 3}
    else if (total == 7) { ados3[row, "total_css"] <- 4}
    else if (total == 8) { ados3[row, "total_css"] <- 5}
    else if (total == 9 | total == 10) { ados3[row, "total_css"] <- 6}
    else if (total == 11 | total == 12) { ados3[row, "total_css"] <- 7}
    else if (total == 13 | total == 14) { ados3[row, "total_css"] <- 8}
    else if (total == 15 | total == 16 | total == 17) { ados3[row, "total_css"] <- 9}
    else if (total >= 18) { ados3[row, "total_css"] <- 10}
  } 
}

# For module 4, calculate raw totals from raw data for individual items and
# then calculate domain-specific and total CSS, using method described in:
# 3.  Hus V, Lord C. The Autism diagnostic observation schedule, module 4: 
#     revised algorithm and standardized severity scores. 
#     J Autism Dev Disord. 2014;44(8):1996-2012. doi:10.1007/s10803-014-2080-3

ados4$rrb_raw < - NA
ados4$sa_raw <- NA
ados4$total_raw <- NA

# Collapse scores of 3 to 2 per the scoring algorithm
for (row in 1:nrow(ados4)) {
  
  if (!is.na(ados4[row, "ados_4.ados_4_raw.a8_conversation_4"])) {
    if (ados4[row, "ados_4.ados_4_raw.a8_conversation_4"] == 3 |
        ados4[row, "ados_4.ados_4_raw.a8_conversation_4"] == 2) {
          ados4[row, "ados_4.ados_4_raw.a8_conversation_4"] <- 2 
    }
  }

  if (!is.na(ados4[row, "ados_4.ados_4_raw.a10_emphatic_gestures_4"])) {
    if (ados4[row, "ados_4.ados_4_raw.a10_emphatic_gestures_4"] == 3 |
        ados4[row, "ados_4.ados_4_raw.a10_emphatic_gestures_4"] == 2) {
      ados4[row, "ados_4.ados_4_raw.a10_emphatic_gestures_4"] <- 2 
    }
  }
  
  if (!is.na(ados4[row, "ados_4.ados_4_raw.b01_eye_contact_4"])) {
    if (ados4[row, "ados_4.ados_4_raw.b01_eye_contact_4"] == 3 |
        ados4[row, "ados_4.ados_4_raw.b01_eye_contact_4"] == 2) {
      ados4[row, "ados_4.ados_4_raw.b01_eye_contact_4"] <- 2 
    }
  }

  if (!is.na(ados4[row, "ados_4.ados_4_raw.b02_facial_others_4"])) {
    if (ados4[row, "ados_4.ados_4_raw.b02_facial_others_4"] == 3 |
        ados4[row, "ados_4.ados_4_raw.b02_facial_others_4"] == 2) {
      ados4[row, "ados_4.ados_4_raw.b02_facial_others_4"] <- 2 
    }
  }
  
  if (!is.na(ados4[row, "ados_4.ados_4_raw.b05_own_affect_4"])) {
    if (ados4[row, "ados_4.ados_4_raw.b05_own_affect_4"] == 3 |
        ados4[row, "ados_4.ados_4_raw.b05_own_affect_4"] == 2) {
      ados4[row, "ados_4.ados_4_raw.b05_own_affect_4"] <- 2 
    }
  }
  
  if (!is.na(ados4[row, "ados_4.ados_4_raw.b07_insight_4"])) {
    if (ados4[row, "ados_4.ados_4_raw.b07_insight_4"] == 3 |
        ados4[row, "ados_4.ados_4_raw.b07_insight_4"] == 2) {
      ados4[row, "ados_4.ados_4_raw.b07_insight_4"] <- 2 
    }
  }
  
  if (!is.na(ados4[row, "ados_4.ados_4_raw.b09_social_overture_4"])) {
    if (ados4[row, "ados_4.ados_4_raw.b09_social_overture_4"] == 3 |
        ados4[row, "ados_4.ados_4_raw.b09_social_overture_4"] == 2) {
      ados4[row, "ados_4.ados_4_raw.b09_social_overture_4"] <- 2 
    }
  }
  
  if (!is.na(ados4[row, "ados_4.ados_4_raw.b10_social_response_4"])) {
    if (ados4[row, "ados_4.ados_4_raw.b10_social_response_4"] == 3 |
        ados4[row, "ados_4.ados_4_raw.b10_social_response_4"] == 2) {
      ados4[row, "ados_4.ados_4_raw.b10_social_response_4"] <- 2 
    }
  }
  
  if (!is.na(ados4[row, "ados_4.ados_4_raw.b11_reciprocal_social_4"])) {
    if (ados4[row, "ados_4.ados_4_raw.b11_reciprocal_social_4"] == 3 |
        ados4[row, "ados_4.ados_4_raw.b11_reciprocal_social_4"] == 2) {
      ados4[row, "ados_4.ados_4_raw.b11_reciprocal_social_4"] <- 2 
    }
  }
  
  if (!is.na(ados4[row, "ados_4.ados_4_raw.b12_quality_rapport"])) {
    if (ados4[row, "ados_4.ados_4_raw.b12_quality_rapport"] == 3 |
        ados4[row, "ados_4.ados_4_raw.b12_quality_rapport"] == 2) {
      ados4[row, "ados_4.ados_4_raw.b12_quality_rapport"] <- 2 
    }
  }
  
  if (!is.na(ados4[row, "ados_4.ados_4_raw.a2_speech_abnormalities_4"])) {
    if (ados4[row, "ados_4.ados_4_raw.a2_speech_abnormalities_4"] == 3 |
        ados4[row, "ados_4.ados_4_raw.a2_speech_abnormalities_4"] == 2) {
      ados4[row, "ados_4.ados_4_raw.a2_speech_abnormalities_4"] <- 2 
    }
  }
  
  if (!is.na(ados4[row, "ados_4.ados_4_raw.a4_stereotyped_words_4"])) {
    if (ados4[row, "ados_4.ados_4_raw.a4_stereotyped_words_4"] == 3 |
        ados4[row, "ados_4.ados_4_raw.a4_stereotyped_words_4"] == 2) {
      ados4[row, "ados_4.ados_4_raw.a4_stereotyped_words_4"] <- 2 
    }
  }
  
  if (!is.na(ados4[row, "ados_4.ados_4_raw.d1_unusual_sensory_4"])) {
    if (ados4[row, "ados_4.ados_4_raw.d1_unusual_sensory_4"] == 3 |
        ados4[row, "ados_4.ados_4_raw.d1_unusual_sensory_4"] == 2) {
      ados4[row, "ados_4.ados_4_raw.d1_unusual_sensory_4"] <- 2 
    }
  }
  
  if (!is.na(ados4[row, "ados_4.ados_4_raw.d2_complex_mannerisms_4"])) {
    if (ados4[row, "ados_4.ados_4_raw.d2_complex_mannerisms_4"] == 3 |
        ados4[row, "ados_4.ados_4_raw.d2_complex_mannerisms_4"] == 2) {
      ados4[row, "ados_4.ados_4_raw.d2_complex_mannerisms_4"] <- 2 
    }
  }
  
  if (!is.na(ados4[row, "ados_4.ados_4_raw.d4_interest_objects_4"])) {
    if (ados4[row, "ados_4.ados_4_raw.d4_interest_objects_4"] == 3 |
        ados4[row, "ados_4.ados_4_raw.d4_interest_objects_4"] == 2) {
      ados4[row, "ados_4.ados_4_raw.d4_interest_objects_4"] <- 2 
    }
  }

  # Raw SA score:
  # a8 + a10 + b1 + b2 + b5 + b7 + b9 + b10 + b11 + b12
  
  ados4[row,"sa_raw"] <-
    ados4[row,"ados_4.ados_4_raw.a8_conversation_4"] +
    ados4[row,"ados_4.ados_4_raw.a10_emphatic_gestures_4"] +
    ados4[row,"ados_4.ados_4_raw.b01_eye_contact_4"] + 
    ados4[row,"ados_4.ados_4_raw.b02_facial_others_4"] + 
    ados4[row,"ados_4.ados_4_raw.b05_own_affect_4"] + 
    ados4[row,"ados_4.ados_4_raw.b07_insight_4"] + 
    ados4[row,"ados_4.ados_4_raw.b09_social_overture_4"] + 
    ados4[row,"ados_4.ados_4_raw.b10_social_response_4"] + 
    ados4[row,"ados_4.ados_4_raw.b11_reciprocal_social_4"] + 
    ados4[row,"ados_4.ados_4_raw.b12_quality_rapport"]

  # Raw RRB score: 
  # a2 + a4 + d1 + d2 + d4
  
  ados4[row, "rrb_raw"] <- 
    ados4[row,"ados_4.ados_4_raw.a2_speech_abnormalities_4"] + 
    ados4[row,"ados_4.ados_4_raw.a4_stereotyped_words_4"] + 
    ados4[row,"ados_4.ados_4_raw.d1_unusual_sensory_4"] + 
    ados4[row,"ados_4.ados_4_raw.d2_complex_mannerisms_4"] + 
    ados4[row,"ados_4.ados_4_raw.d4_interest_objects_4"]
  
  # Raw total score: SA + RRB
  ados4[row, "total_raw"] <- 
    ados4[row, "sa_raw"] +
    ados4[row, "rrb_raw"]
}

# Now derive CSS
ados4$rrb_css <- NA
ados4$sa_css <- NA
ados4$total_css <- NA

for (row in 1:nrow(ados4)) {
  rrb <- ados4[row, "rrb_raw"]
  sa <- ados4[row, "sa_raw"]
  total <- ados4[row, "total_raw"]
  
  if (!is.na(total)) {
    if (total == 0 | total == 1 | total == 2) { ados4[row, "total_css"] <- 1 }
    else if (total == 3 | total == 4 | total == 5) { 
      ados4[row, "total_css"] <- 2 
    }
    else if (total == 6 | total == 7) { ados4[row, "total_css"] <- 3 }
    else if (total == 8) { ados4[row, "total_css"] <- 4 }
    else if (total == 9) { ados4[row, "total_css"] <- 5 }
    else if (total == 10 | total == 11) { ados4[row, "total_css"] <- 6 }
    else if (total == 12 | total == 13) { ados4[row, "total_css"] <- 7 }
    else if (total == 14 | total == 15) { ados4[row, "total_css"] <- 8 }
    else if (total == 16 | total == 17 | total == 18 | total == 19) { 
      ados4[row, "total_css"] <- 9 
    }
    else if (total >= 20) { ados4[row, "total_css"] <- 10 }
  }  

  if (!is.na(sa)) {
    if (sa == 0 | sa == 1) { ados4[row, "sa_css"] < - 1 }
    else if (sa == 2 | sa == 3) { ados4[row, "sa_css"] <- 2 }
    else if (sa == 4) {ados4[row, "sa_css"] <- 3 }
    else if (sa == 5) {ados4[row, "sa_css"] <- 4 }
    else if (sa == 6) {ados4[row, "sa_css"] <- 5 }
    else if (sa == 7 | sa == 8) {ados4[row, "sa_css"] <- 6 }
    else if (sa == 9 | sa == 10) {ados4[row, "sa_css"] <- 7 }
    else if (sa == 11 | sa == 12) {ados4[row, "sa_css"] <- 8 }
    else if (sa == 13 | sa == 14 | sa == 15) {ados4[row, "sa_css"] <- 9 }
    else if (sa >= 16) { ados4[row, "sa_css"] <- 10 }
  }

  if (!is.na(rrb)) {
    if (rrb == 0) { ados4[row, "rrb_css"] < - 1 }
    else if (rrb == 1) { ados4[row, "rrb_css"] <- 5 }
    else if (rrb == 2) {ados4[row, "rrb_css"] <- 6 }
    else if (rrb == 3) {ados4[row, "rrb_css"] <- 7 }
    else if (rrb == 4) {ados4[row, "rrb_css"] <- 8 }
    else if (rrb == 5) {ados4[row, "sa_css"] <- 9 }
    else if (rrb >= 6) {ados4[row, "sa_css"] <- 10 }
  }
}

svip$diagnosis_summary <- svip$diagnosis_summary %>% mutate(ados_css_total_derived = NA)
svip$diagnosis_summary <- svip$diagnosis_summary %>% mutate(ados_css_sa_derived = NA)
svip$diagnosis_summary <- svip$diagnosis_summary %>% mutate(ados_css_rrb_derived = NA)

# Merge ADOS dataframes

for (row in 1:nrow(svip$diagnosis_summary)) {
  individual <- svip$diagnosis_summary[row, "individual"]
  
  # total
  if(individual == ados4[row, "individual"] & !is.na(ados4[row, "total_css"])) {
    svip$diagnosis_summary[row,"ados_css_total_derived"] <- ados4[row, "total_css"]
  }
  else if(individual == ados3[row, "individual"] & !is.na(ados3[row, "total_css"])) {
    svip$diagnosis_summary[row,"ados_css_total_derived"] <- ados3[row, "total_css"]
  }
  else if(individual == ados2[row, "individual"] & !is.na(ados2[row, "total_css"])) {
    svip$diagnosis_summary[row,"ados_css_total_derived"] <- ados2[row, "total_css"]
  }
  else if(individual == ados1[row, "individual"] & !is.na(ados1[row, "total_css"])) {
    svip$diagnosis_summary[row,"ados_css_total_derived"] <- ados1[row, "total_css"]
  }

  # SA
  if(individual == ados4[row, "individual"] & !is.na(ados4[row, "sa_css"])) {
    svip$diagnosis_summary[row,"ados_css_sa_derived"] <- ados4[row, "sa_css"]
  }
  else if(individual == ados3[row, "individual"] & !is.na(ados3[row, "sa_css"])) {
    svip$diagnosis_summary[row,"ados_css_sa_derived"] <- ados3[row, "sa_css"]
  }
  else if(individual == ados2[row, "individual"] & !is.na(ados2[row, "sa_css"])) {
    svip$diagnosis_summary[row,"ados_css_sa_derived"] <- ados2[row, "sa_css"]
  }
  else if(individual == ados1[row, "individual"] & !is.na(ados1[row, "sa_css"])) {
    svip$diagnosis_summary[row,"ados_css_sa_derived"] <- ados1[row, "sa_css"]
  }

  # RRB
  if(individual == ados4[row, "individual"] & !is.na(ados4[row, "rrb_css"])) {
    svip$diagnosis_summary[row,"ados_css_rrb_derived"] <- ados4[row, "rrb_css"]
  }
  else if(individual == ados3[row, "individual"] & !is.na(ados3[row, "rrb_css"])) {
    svip$diagnosis_summary[row,"ados_css_rrb_derived"] <- ados3[row, "rrb_css"]
  }
  else if(individual == ados2[row, "individual"] & !is.na(ados2[row, "rrb_css"])) {
    svip$diagnosis_summary[row,"ados_css_rrb_derived"] <- ados2[row, "rrb_css"]
  }
  else if(individual == ados1[row, "individual"] & !is.na(ados1[row, "rrb_css"])) {
    svip$diagnosis_summary[row,"ados_css_rrb_derived"] <- ados1[row, "rrb_css"]
  }
}

# ados_css_total_combined should be the extant ADOS CSS data with
# ADOS module 4 CSS added.

svip$diagnosis_summary <- svip$diagnosis_summary %>% mutate(
  ados_css_total_combined = diagnosis_summary.ados_css)

for (row in 1:nrow(svip$diagnosis_summary)) {
  
  if(is.na(svip$diagnosis_summary[row, "diagnosis_summary.ados_css"]) &
     !is.na(svip$diagnosis_summary[row, "ados_css_total_derived"])) {
    svip$diagnosis_summary[row,"ados_css_total_combined"] <- svip$diagnosis_summary[row, "ados_css_total_derived"]
  }
}

#clean up temporary variables
rm(ados1)
rm(ados2)
rm(ados3)
rm(ados4)
rm(age)
rm(individual)
rm(rrb)
rm(sa)
rm(row)
rm(total)