# Convert missing data for 'refused', 'not ascertained', 'don't know', 'inconsistent' and 'NIU' from all columns

recode_to_NA_all <- function(data){
  
  # Specify values for missing variables 
  data$AGE[data$AGE==997|data$AGE==998|data$AGE==999] <- NA  # checked
  data$SEX[data$SEX==7|data$SEX==8|data$SEX==9] <- NA # checked
  data$SEXORIEN[data$SEXORIEN==7|data$SEXORIEN==8] <- NA # checked
  data$EDUCREC2[data$EDUCREC2==96|data$EDUCREC2==97|data$EDUCREC2==98|data$EDUCREC2==99] <- NA # checked
  data$RACENEW[data$RACENEW==997|data$RACENEW==998|data$RACENEW==999] <- NA # checked
  data$USBORN[data$USBORN==96|data$USBORN==97|data$USBORN==98|data$USBORN==99] <- NA
  data$CITIZEN[data$CITIZEN==7|data$CITIZEN==8|data$CITIZEN==9] <- NA
  data$INCFAM97ON2[data$INCFAM97ON2==97|data$INCFAM97ON2==98|data$INCFAM97ON2==99] <- NA
  data$ALCAMT[data$ALCAMT==0|data$ALCAMT==96|data$ALCAMT==97|data$ALCAMT==98|data$ALCAMT==99] <- NA
  data$ALC5UPYR[data$ALC5UPYR==996 |data$ALC5UPYR==997 | data$ALC5UPYR==998 | data$ALC5UPYR==999] <- NA
  data$ALCSTAT1[data$ALCSTAT1==0|data$ALCSTAT1==9] <- NA
  data$ALCDAYSYR[data$ALCDAYSYR==995|data$ALCDAYSYR==996|data$ALCDAYSYR==997|data$ALCDAYSYR==998|data$ALCDAYSYR==999] <- NA
  data$HISPYN[data$HISPYN==7|data$HISPYN==8|data$HISPYN==9] <- NA
  data$BIRTHYR[data$BIRTHYR==9997|data$BIRTHYR==9998|data$BIRTHYR==9999] <- NA

  return(data)
  
  }

