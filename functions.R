# Zeynep Gunes Ozkan 2022, Functions
# Bournemouth University

# UCLA 
# 1, 5, 6, 9, 10, 15, 16, 19, 20 Reversed [1:4]
# Oonline cognition scale 
# 12 Reversed [1:7]

## Reverse UCLA
uclaRev <- function(data){
  
  for(i in 1:nrow(data)){
    
    if(data$ucla_1[i] == 1){
      data$ucla_1[i] <- 4
    }else{
      if(data$ucla_1[i] == 2){
        data$ucla_1[i] <- 3
      }else{
        if(data$ucla_1[i] == 3){
          data$ucla_1[i] <- 2
        }else{
          if(data$ucla_1[i] == 4){
            data$ucla_1[i] <- 1
          }
        }
      }
    }
    
    if(data$ucla_5[i] == 1){
      data$ucla_5[i] <- 4
    }else{
      if(data$ucla_5[i] == 2){
        data$ucla_5[i] <- 3
      }else{
        if(data$ucla_5[i] == 3){
          data$ucla_5[i] <- 2
        }else{
          if(data$ucla_5[i] == 4){
            data$ucla_5[i] <- 1
          }
        }
      }
    }
    
    if(data$ucla_6[i] == 1){
      data$ucla_6[i] <- 4
    }else{
      if(data$ucla_6[i] == 2){
        data$ucla_6[i] <- 3
      }else{
        if(data$ucla_6[i] == 3){
          data$ucla_6[i] <- 2
        }else{
          if(data$ucla_6[i] == 4){
            data$ucla_6[i] <- 1
          }
        }
      }
    }
    
    if(data$ucla_9[i] == 1){
      data$ucla_9[i] <- 4
    }else{
      if(data$ucla_9[i] == 2){
        data$ucla_9[i] <- 3
      }else{
        if(data$ucla_9[i] == 3){
          data$ucla_9[i] <- 2
        }else{
          if(data$ucla_9[i] == 4){
            data$ucla_9[i] <- 1
          }
        }
      }
    }
    
    if(data$ucla_10[i] == 1){
      data$ucla_10[i] <- 4
    }else{
      if(data$ucla_10[i] == 2){
        data$ucla_10[i] <- 3
      }else{
        if(data$ucla_10[i] == 3){
          data$ucla_10[i] <- 2
        }else{
          if(data$ucla_10[i] == 4){
            data$ucla_10[i] <- 1
          }
        }
      }
    }
    
    if(data$ucla_15[i] == 1){
      data$ucla_15[i] <- 4
    }else{
      if(data$ucla_15[i] == 2){
        data$ucla_15[i] <- 3
      }else{
        if(data$ucla_15[i] == 3){
          data$ucla_15[i] <- 2
        }else{
          if(data$ucla_15[i] == 4){
            data$ucla_15[i] <- 1
          }
        }
      }
    }
    
    if(data$ucla_16[i] == 1){
      data$ucla_16[i] <- 4
    }else{
      if(data$ucla_16[i] == 2){
        data$ucla_16[i] <- 3
      }else{
        if(data$ucla_16[i] == 3){
          data$ucla_16[i] <- 2
        }else{
          if(data$ucla_16[i] == 4){
            data$ucla_16[i] <- 1
          }
        }
      }
    }
    
    if(data$ucla_19[i] == 1){
      data$ucla_19[i] <- 4
    }else{
      if(data$ucla_19[i] == 2){
        data$ucla_19[i] <- 3
      }else{
        if(data$ucla_19[i] == 3){
          data$ucla_19[i] <- 2
        }else{
          if(data$ucla_19[i] == 4){
            data$ucla_19[i] <- 1
          }
        }
      }
    }
    
    if(data$ucla_20[i] == 1){
      data$ucla_20[i] <- 4
    }else{
      if(data$ucla_20[i] == 2){
        data$ucla_20[i] <- 3
      }else{
        if(data$ucla_20[i] == 3){
          data$ucla_20[i] <- 2
        }else{
          if(data$ucla_20[i] == 4){
            data$ucla_20[i] <- 1
          }
        }
      }
    }
  }# for ends 
  return(data)
}

## UCLA score

UclaScore <- function(data){
  for(i in 1:nrow(data)){
    data$Ucla_score[i] <-sum(data$ucla_1[i],data$ucla_2[i],data$ucla_3[i],data$ucla_4[i],data$ucla_5[i],
                             data$ucla_6[i],data$ucla_7[i],data$ucla_8[i],data$ucla_9[i],data$ucla_10[i],
                             data$ucla_11[i],data$ucla_12[i],data$ucla_13[i],data$ucla_14[i],data$ucla_15[i],
                             data$ucla_16[i],data$ucla_17[i],data$ucla_18[i],data$ucla_19[i],data$ucla_20[i])
  }
  return(data)
}

## Reverse OCI

OCIrev <- function(data){
  for(i in 1:nrow(data)){
    if(data$OCI_12[i] == 1){
      data$OCI_12[i] <- 7
    }else{
      if(data$OCI_12[i] == 2){
        data$OCI_12[i] <- 6
      }else{
        if(data$OCI_12[i] == 3){
          data$OCI_12[i] <- 5
        }else{
          if(data$OCI_12[i] == 4){
            data$OCI_12[i] <- 4
          }else{
            if(data$OCI_12[i] == 5){
              data$OCI_12[i] <- 3
            }else{
              if(data$OCI_12[i] == 6){
                data$OCI_12[i] <- 2
              }else{
                if(data$OCI_12[i] == 7){
                  data$OCI_12[i] <- 1
                }
              }
            }
          }
        }
      }
    }
  }#for ends
  return(data)
}


#Total OCI

OCItotal <- function(data){
  for(i in 1:nrow(data)){
    data$OCI_score[i] <- sum(data$OCI_1[i],data$OCI_2[i],data$OCI_3[i],data$OCI_4[i],data$OCI_5[i],data$OCI_6[i],data$OCI_7[i],
                          data$OCI_8[i],data$OCI_9[i],data$OCI_10[i],data$OCI_11[i],data$OCI_12[i],data$OCI_13[i],data$OCI_14[i],
                          data$OCI_15[i],data$OCI_16[i],data$OCI_17[i],data$OCI_18[i],data$OCI_19[i],data$OCI_20[i],data$OCI_21[i],
                          data$OCI_22[i],data$OCI_23[i],data$OCI_24[i],data$OCI_25[i],data$OCI_26[i],data$OCI_27[i],data$OCI_28[i],
                          data$OCI_29[i],data$OCI_30[i],data$OCI_31[i],data$OCI_32[i],data$OCI_33[i],data$OCI_34[i],data$OCI_35[i],
                          data$OCI_36[i])
  }
  return(data)
}

# OCI_socsup: 1,3,6,7,8,9,13,14,16,18,19,26,31

OCIsocsup <- function(data){
  for(i in 1:nrow(data)){
    data$OCI_socsup[i] <- sum(data$OCI_1[i],data$OCI_3[i],data$OCI_6[i],data$OCI_7[i],data$OCI_8[i],data$OCI_9[i],
                             data$OCI_13[i],data$OCI_14[i],data$OCI_16[i],data$OCI_18[i],data$OCI_19[i],data$OCI_26[i],
                             data$OCI_31[i])
  }
  return(data)
}

# OCI_dep: 2,22,23,24,25,35

OCIdep <- function(data){
  for(i in 1:nrow(data)){
    data$OCI_dep[i] <- sum(data$OCI_2[i],data$OCI_22[i],data$OCI_23[i],data$OCI_24[i],data$OCI_25[i],data$OCI_35[i])
  }
  return(data)
}

# OCI_imp: 4,5,10,11,12,15,17,21,34,36

OCIimp <- function(data){
  for(i in 1:nrow(data)){
    data$OCI_imp[i] <- sum(data$OCI_4[i],data$OCI_5[i],data$OCI_10[i],data$OCI_11[i],data$OCI_12[i],data$OCI_15[i],
                            data$OCI_17[i],data$OCI_21[i],data$OCI_34[i],data$OCI_36[i])
  }
  return(data)
}

# OCI_att: 20,27,28,29,30,32,33

OCIatt <- function(data){
  for(i in 1:nrow(data)){
    data$OCI_att[i] <- sum(data$OCI_20[i],data$OCI_27[i],data$OCI_28[i],data$OCI_29[i],data$OCI_30[i],data$OCI_32[i],
                           data$OCI_33[i])
  }
  return(data)
}


