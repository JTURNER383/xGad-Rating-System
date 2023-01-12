###############################################################################

## Start of Season 1

###############################################################################

one_sea <- func.data %>%  filter(func.data$Season == "2014/2015")

one_sea <- one_sea %>% 
  mutate(Row = row_number())

one_sea$Row <- as.character(one_sea$Row)

one_sea <- one_sea %>% 
  group_by(HID) %>% 
  dplyr::mutate(Row.HID = dplyr::lead(Row, n=1L, default = NA)) %>% 
  ungroup()

one_sea <- one_sea %>% 
  group_by(AID) %>% 
  dplyr::mutate(Row.AID = dplyr::lead(Row, n=1L, default = NA)) %>% 
  ungroup()

K <- 20
HA <- 25
HAD <- 25

###############################################################################

## 2014/15 Loop

###############################################################################


for(i in 1:nrow(one_sea)){
  
  one_sea[i, 'DAH'] <- (one_sea$EloHomeAtt[i]+HA)-one_sea$EloAwayDef[i]
  one_sea[i, 'DAA'] <- one_sea$EloAwayAtt[i]-(one_sea$EloHomeDef[i]+HA)
  one_sea[i, 'DDH'] <- (one_sea$EloHomeDef[i]+HAD)-(one_sea$EloAwayAtt[i])
  one_sea[i, 'DDA'] <- one_sea$EloAwayDef[i]-(one_sea$EloHomeAtt[i]+HAD)
  one_sea[i, 'EAH'] <- 1/(1+10^((-(one_sea$DAH[i]))/400))
  one_sea[i, 'EAA'] <- 1/(1+10^((-(one_sea$DAA[i]))/400))
  one_sea[i, 'EDH'] <- 1/(1+10^((-(one_sea$DDH[i]))/400))
  one_sea[i, 'EDA'] <- 1/(1+10^((-(one_sea$DDA[i]))/400))
  one_sea[i, 'PAH'] <- one_sea$OAH[i] - one_sea$EAH[i]
  one_sea[i, 'PAA'] <- one_sea$OAA[i] - one_sea$EAA[i]
  one_sea[i, 'PDH'] <- one_sea$ODH[i] - one_sea$EDH[i]
  one_sea[i, 'PDA'] <- one_sea$ODA[i] - one_sea$EDA[i]
  
  if(one_sea$PAH[i] < 0) {
    one_sea[i, 'GAH'] <- (2)/(1+one_sea$Home.Expected.Goals[i])
  } else{
    one_sea[i, 'GAH'] <- (sqrt(one_sea$Home.Expected.Goals[i]))/1
  }
  
  if(one_sea$PAA[i] < 0) {
    one_sea[i, 'GAA'] <- (2)/(1+one_sea$Away.Expected.Goals[i])
  } else{
    one_sea[i, 'GAA'] <- (sqrt(one_sea$Away.Expected.Goals[i]))/1
  }
  
  if(one_sea$PDH[i] < 0) {
    one_sea[i, 'GDH'] <- (sqrt(one_sea$Away.Expected.Goals[i]))/1
  } else{
    one_sea[i, 'GDH'] <- (2)/(1+one_sea$Away.Expected.Goals[i])
  }
  
  if(one_sea$PDA[i] < 0) {
    one_sea[i, 'GDA'] <- (sqrt(one_sea$Home.Expected.Goals[i]))/1
  } else{
    one_sea[i, 'GDA'] <- (2)/(1+one_sea$Home.Expected.Goals[i])
  }
  
  if(is.na(one_sea$Row.HID[i]) == TRUE) {
    
    one_sea[i,'EloHomeAtt'] <- one_sea$EloHomeAtt[i] + ((K*(one_sea$GAH[i]))*(one_sea$OAH[i] - (one_sea$EAH[i])))
    one_sea[i,'EloHomeDef'] <- one_sea$EloHomeDef[i] + ((K*(one_sea$GDH[i]))*(one_sea$ODH[i] - (one_sea$EDH[i])))
    one_sea[i, 'CAH'] <- ((K*(one_sea$GAH[i]))*(one_sea$OAH[i] - (one_sea$EAH[i])))
    one_sea[i, 'CDH'] <- ((K*(one_sea$GDH[i]))*(one_sea$ODH[i] - (one_sea$EDH[i])))
    
  } else{
    
    one_sea[one_sea$Row.HID[i],'EloHomeAtt'] <- one_sea$EloHomeAtt[i] + ((K*(one_sea$GAH[i]))*(one_sea$OAH[i] - (one_sea$EAH[i])))
    one_sea[one_sea$Row.HID[i],'EloHomeDef'] <- one_sea$EloHomeDef[i] + ((K*(one_sea$GDH[i]))*(one_sea$ODH[i] - (one_sea$EDH[i])))
    one_sea[i, 'CAH'] <- ((K*(one_sea$GAH[i]))*(one_sea$OAH[i] - (one_sea$EAH[i])))
    one_sea[i, 'CDH'] <- ((K*(one_sea$GDH[i]))*(one_sea$ODH[i] - (one_sea$EDH[i])))
    
  }
  
  if(is.na(one_sea$Row.AID[i]) == TRUE) {
    
    one_sea[i,'EloAwayDef'] <- one_sea$EloAwayDef[i] + ((K*(one_sea$GDA[i]))*(one_sea$ODA[i] - (one_sea$EDA[i])))
    one_sea[i,'EloAwayAtt'] <- one_sea$EloAwayAtt[i] + ((K*(one_sea$GAA[i]))*(one_sea$OAA[i] - (one_sea$EAA[i])))
    one_sea[i, 'CDA'] <- ((K*(one_sea$GDA[i]))*(one_sea$ODA[i] - (one_sea$EDA[i])))
    one_sea[i, 'CAA'] <- ((K*(one_sea$GAA[i]))*(one_sea$OAA[i] - (one_sea$EAA[i])))
    
  } else{
    
    one_sea[one_sea$Row.AID[i],'EloAwayDef'] <- one_sea$EloAwayDef[i] + ((K*(one_sea$GDA[i]))*(one_sea$ODA[i] - (one_sea$EDA[i])))
    one_sea[one_sea$Row.AID[i],'EloAwayAtt'] <- one_sea$EloAwayAtt[i] + ((K*(one_sea$GAA[i]))*(one_sea$OAA[i] - (one_sea$EAA[i]))) 
    one_sea[i, 'CDA'] <- ((K*(one_sea$GDA[i]))*(one_sea$ODA[i] - (one_sea$EDA[i])))
    one_sea[i, 'CAA'] <- ((K*(one_sea$GAA[i]))*(one_sea$OAA[i] - (one_sea$EAA[i])))
    
  }
}



################################################################################

## End of Season 1

################################################################################

two_sea <- func.data %>%  filter(func.data$Season == "2015/2016")

lastH <- one_sea %>% 
  group_by(HID) %>% 
  summarise(EloHomeAtt = last(EloHomeAtt),
            EloHomeDef = last(EloHomeDef))
lastA <- one_sea %>% 
  group_by(AID) %>% 
  summarise(EloAwayAtt = last(EloAwayAtt),
            EloAwayDef = last(EloAwayDef))

last <- cbind(lastH, lastA)

rm(lastA)
rm(lastH)

for(i in 1:nrow(last)){
  last$EloHomeAtt[i] <- (last$EloHomeAtt[i]*.8)+(.2*1500)
  last$EloHomeDef[i] <- (last$EloHomeDef[i]*.8)+(.2*1500)
  last$EloAwayAtt[i] <- (last$EloAwayAtt[i]*.8)+(.2*1500)
  last$EloAwayDef[i] <- (last$EloAwayDef[i]*.8)+(.2*1500)
}

rel.prom <- last[c(13,14,15),]

rel.prom$HID[rel.prom$HID == 21] <- 3
rel.prom$HID[rel.prom$HID == 22] <- 6
rel.prom$HID[rel.prom$HID == 24] <- 19
rel.prom$AID[rel.prom$AID == 21] <- 3
rel.prom$AID[rel.prom$AID == 22] <- 6
rel.prom$AID[rel.prom$AID == 24] <- 19

rel.prom$EloHomeAtt <- mean(rel.prom$EloHomeAtt)
rel.prom$EloHomeDef <- mean(rel.prom$EloHomeDef)
rel.prom$EloAwayAtt <- mean(rel.prom$EloAwayAtt)
rel.prom$EloAwayDef <- mean(rel.prom$EloAwayDef)

last[13,] <- rel.prom[1,]
last[14,] <- rel.prom[2,]
last[15,] <- rel.prom[3,]
rm(rel.prom)

for(i in 1:20){
  two_sea$EloHomeAtt[i] <- last$EloHomeAtt[two_sea$HID[i] == last$HID]
  two_sea$EloHomeDef[i] <- last$EloHomeDef[two_sea$HID[i] == last$HID]
  two_sea$EloAwayAtt[i] <- last$EloAwayAtt[two_sea$AID[i] == last$AID]
  two_sea$EloAwayDef[i] <- last$EloAwayDef[two_sea$AID[i] == last$AID]
}

two_sea <- two_sea %>% 
  mutate(Row = row_number())

two_sea$Row <- as.character(two_sea$Row)

two_sea <- two_sea %>% 
  group_by(HID) %>% 
  dplyr::mutate(Row.HID = dplyr::lead(Row, n=1L, default = NA)) %>% 
  ungroup()

two_sea <- two_sea %>% 
  group_by(AID) %>% 
  dplyr::mutate(Row.AID = dplyr::lead(Row, n=1L, default = NA)) %>% 
  ungroup()

###############################################################################

## 2015/16 loop

###############################################################################

for(i in 1:nrow(two_sea)){
  
  two_sea[i, 'DAH'] <- (two_sea$EloHomeAtt[i]+HA)-two_sea$EloAwayDef[i]
  two_sea[i, 'DAA'] <- two_sea$EloAwayAtt[i]-(two_sea$EloHomeDef[i]+HA)
  two_sea[i, 'DDH'] <- (two_sea$EloHomeDef[i]+HAD)-(two_sea$EloAwayAtt[i])
  two_sea[i, 'DDA'] <- two_sea$EloAwayDef[i]-(two_sea$EloHomeAtt[i]+HAD)
  two_sea[i, 'EAH'] <- 1/(1+10^((-(two_sea$DAH[i]))/400))
  two_sea[i, 'EAA'] <- 1/(1+10^((-(two_sea$DAA[i]))/400))
  two_sea[i, 'EDH'] <- 1/(1+10^((-(two_sea$DDH[i]))/400))
  two_sea[i, 'EDA'] <- 1/(1+10^((-(two_sea$DDA[i]))/400))
  two_sea[i, 'PAH'] <- two_sea$OAH[i] - two_sea$EAH[i]
  two_sea[i, 'PAA'] <- two_sea$OAA[i] - two_sea$EAA[i]
  two_sea[i, 'PDH'] <- two_sea$ODH[i] - two_sea$EDH[i]
  two_sea[i, 'PDA'] <- two_sea$ODA[i] - two_sea$EDA[i]
  
  if(two_sea$PAH[i] < 0) {
    two_sea[i, 'GAH'] <- (2)/(1+two_sea$Home.Expected.Goals[i])
  } else{
    two_sea[i, 'GAH'] <- (sqrt(two_sea$Home.Expected.Goals[i]))/1
  }
  
  if(two_sea$PAA[i] < 0) {
    two_sea[i, 'GAA'] <- (2)/(1+two_sea$Away.Expected.Goals[i])
  } else{
    two_sea[i, 'GAA'] <- (sqrt(two_sea$Away.Expected.Goals[i]))/1
  }
  
  if(two_sea$PDH[i] < 0) {
    two_sea[i, 'GDH'] <- (sqrt(two_sea$Away.Expected.Goals[i]))/1
  } else{
    two_sea[i, 'GDH'] <- (2)/(1+two_sea$Away.Expected.Goals[i])
  }
  
  if(two_sea$PDA[i] < 0) {
    two_sea[i, 'GDA'] <- (sqrt(two_sea$Home.Expected.Goals[i]))/1
  } else{
    two_sea[i, 'GDA'] <- (2)/(1+two_sea$Home.Expected.Goals[i])
  }
  
  
  if(is.na(two_sea$Row.HID[i]) == TRUE) {
    
    two_sea[i,'EloHomeAtt'] <- two_sea$EloHomeAtt[i] + ((K*(two_sea$GAH[i]))*(two_sea$OAH[i] - (two_sea$EAH[i])))
    two_sea[i,'EloHomeDef'] <- two_sea$EloHomeDef[i] + ((K*(two_sea$GDH[i]))*(two_sea$ODH[i] - (two_sea$EDH[i])))
    two_sea[i, 'CAH'] <- ((K*(two_sea$GAH[i]))*(two_sea$OAH[i] - (two_sea$EAH[i])))
    two_sea[i, 'CDH'] <- ((K*(two_sea$GDH[i]))*(two_sea$ODH[i] - (two_sea$EDH[i])))
    
  } else{
    
    two_sea[two_sea$Row.HID[i],'EloHomeAtt'] <- two_sea$EloHomeAtt[i] + ((K*(two_sea$GAH[i]))*(two_sea$OAH[i] - (two_sea$EAH[i])))
    two_sea[two_sea$Row.HID[i],'EloHomeDef'] <- two_sea$EloHomeDef[i] + ((K*(two_sea$GDH[i]))*(two_sea$ODH[i] - (two_sea$EDH[i])))
    two_sea[i, 'CAH'] <- ((K*(two_sea$GAH[i]))*(two_sea$OAH[i] - (two_sea$EAH[i])))
    two_sea[i, 'CDH'] <- ((K*(two_sea$GDH[i]))*(two_sea$ODH[i] - (two_sea$EDH[i])))
    
  }
  
  if(is.na(two_sea$Row.AID[i]) == TRUE) {
    
    two_sea[i,'EloAwayDef'] <- two_sea$EloAwayDef[i] + ((K*(two_sea$GDA[i]))*(two_sea$ODA[i] - (two_sea$EDA[i])))
    two_sea[i,'EloAwayAtt'] <- two_sea$EloAwayAtt[i] + ((K*(two_sea$GAA[i]))*(two_sea$OAA[i] - (two_sea$EAA[i])))
    two_sea[i, 'CDA'] <- ((K*(two_sea$GDA[i]))*(two_sea$ODA[i] - (two_sea$EDA[i])))
    two_sea[i, 'CAA'] <- ((K*(two_sea$GAA[i]))*(two_sea$OAA[i] - (two_sea$EAA[i])))
    
  } else{
    
    two_sea[two_sea$Row.AID[i],'EloAwayDef'] <- two_sea$EloAwayDef[i] + ((K*(two_sea$GDA[i]))*(two_sea$ODA[i] - (two_sea$EDA[i])))
    two_sea[two_sea$Row.AID[i],'EloAwayAtt'] <- two_sea$EloAwayAtt[i] + ((K*(two_sea$GAA[i]))*(two_sea$OAA[i] - (two_sea$EAA[i]))) 
    two_sea[i, 'CDA'] <- ((K*(two_sea$GDA[i]))*(two_sea$ODA[i] - (two_sea$EDA[i])))
    two_sea[i, 'CAA'] <- ((K*(two_sea$GAA[i]))*(two_sea$OAA[i] - (two_sea$EAA[i])))
    
  }
  
  
  
}


################################################################################################

## End of Season 2

################################################################################################

three_sea <- func.data %>%  filter(func.data$Season == "2016/2017")

lastH <- two_sea %>% 
  group_by(HID) %>% 
  summarise(EloHomeAtt = last(EloHomeAtt),
            EloHomeDef = last(EloHomeDef))
lastA <- two_sea %>% 
  group_by(AID) %>% 
  summarise(EloAwayAtt = last(EloAwayAtt),
            EloAwayDef = last(EloAwayDef))

last <- cbind(lastH, lastA)

rm(lastA)
rm(lastH)

for(i in 1:nrow(last)){
  last$EloHomeAtt[i] <- (last$EloHomeAtt[i]*.8)+(.2*1500)
  last$EloHomeDef[i] <- (last$EloHomeDef[i]*.8)+(.2*1500)
  last$EloAwayAtt[i] <- (last$EloAwayAtt[i]*.8)+(.2*1500)
  last$EloAwayDef[i] <- (last$EloAwayDef[i]*.8)+(.2*1500)
}

rel.prom <- last[c(15,11,3),]

rel.prom$HID[rel.prom$HID == 4] <- 21
rel.prom$HID[rel.prom$HID == 19] <- 22
rel.prom$HID[rel.prom$HID == 11] <- 23
rel.prom$AID[rel.prom$AID == 4] <- 21
rel.prom$AID[rel.prom$AID == 19] <- 22
rel.prom$AID[rel.prom$AID == 11] <- 23

rel.prom$EloHomeAtt <- mean(rel.prom$EloHomeAtt)
rel.prom$EloHomeDef <- mean(rel.prom$EloHomeDef)
rel.prom$EloAwayAtt <- mean(rel.prom$EloAwayAtt)
rel.prom$EloAwayDef <- mean(rel.prom$EloAwayDef)

last[15,] <- rel.prom[1,]
last[11,] <- rel.prom[2,]
last[3,] <- rel.prom[3,]
rm(rel.prom)

for(i in 1:20){
  three_sea$EloHomeAtt[i] <- last$EloHomeAtt[three_sea$HID[i] == last$HID]
  three_sea$EloHomeDef[i] <- last$EloHomeDef[three_sea$HID[i] == last$HID]
  three_sea$EloAwayAtt[i] <- last$EloAwayAtt[three_sea$AID[i] == last$AID]
  three_sea$EloAwayDef[i] <- last$EloAwayDef[three_sea$AID[i] == last$AID]
}

three_sea <- three_sea %>% 
  mutate(Row = row_number())

three_sea$Row <- as.character(three_sea$Row)

three_sea <- three_sea %>% 
  group_by(HID) %>% 
  dplyr::mutate(Row.HID = dplyr::lead(Row, n=1L, default = NA)) %>% 
  ungroup()

three_sea <- three_sea %>% 
  group_by(AID) %>% 
  dplyr::mutate(Row.AID = dplyr::lead(Row, n=1L, default = NA)) %>% 
  ungroup()

## Dealing with Burnley first 2 games being at home and Liverpool away twice

three_sea[13, 'EloHomeAtt'] <- NA
three_sea[13, 'EloHomeDef'] <- NA
three_sea[13, 'EloAwayDef'] <- NA
three_sea[13, 'EloAwayAtt'] <- NA
three_sea[25, 'EloAwayAtt'] <- last[11, 'EloAwayAtt']
three_sea[25, 'EloAwayDef'] <- last[11, 'EloAwayDef']
three_sea[38, 'EloHomeAtt'] <- last[8, 'EloHomeAtt']
three_sea[38, 'EloHomeDef'] <- last[8, 'EloHomeDef']
#####################################################################################

## 2016/17 Loop

#####################################################################################

for(i in 1:nrow(three_sea)){
  
  three_sea[i, 'DAH'] <- (three_sea$EloHomeAtt[i]+HA)-three_sea$EloAwayDef[i]
  three_sea[i, 'DAA'] <- three_sea$EloAwayAtt[i]-(three_sea$EloHomeDef[i]+HA)
  three_sea[i, 'DDH'] <- (three_sea$EloHomeDef[i]+HAD)-(three_sea$EloAwayAtt[i])
  three_sea[i, 'DDA'] <- three_sea$EloAwayDef[i]-(three_sea$EloHomeAtt[i]+HAD)
  three_sea[i, 'EAH'] <- 1/(1+10^((-(three_sea$DAH[i]))/400))
  three_sea[i, 'EAA'] <- 1/(1+10^((-(three_sea$DAA[i]))/400))
  three_sea[i, 'EDH'] <- 1/(1+10^((-(three_sea$DDH[i]))/400))
  three_sea[i, 'EDA'] <- 1/(1+10^((-(three_sea$DDA[i]))/400))
  three_sea[i, 'PAH'] <- three_sea$OAH[i] - three_sea$EAH[i]
  three_sea[i, 'PAA'] <- three_sea$OAA[i] - three_sea$EAA[i]
  three_sea[i, 'PDH'] <- three_sea$ODH[i] - three_sea$EDH[i]
  three_sea[i, 'PDA'] <- three_sea$ODA[i] - three_sea$EDA[i]
  
  if(three_sea$PAH[i] < 0) {
    three_sea[i, 'GAH'] <- (2)/(1+three_sea$Home.Expected.Goals[i])
  } else{
    three_sea[i, 'GAH'] <- (sqrt(three_sea$Home.Expected.Goals[i]))/1
  }
  
  if(three_sea$PAA[i] < 0) {
    three_sea[i, 'GAA'] <- (2)/(1+three_sea$Away.Expected.Goals[i])
  } else{
    three_sea[i, 'GAA'] <- (sqrt(three_sea$Away.Expected.Goals[i]))/1
  }
  
  if(three_sea$PDH[i] < 0) {
    three_sea[i, 'GDH'] <- (sqrt(three_sea$Away.Expected.Goals[i]))/1
  } else{
    three_sea[i, 'GDH'] <- (2)/(1+three_sea$Away.Expected.Goals[i])
  }
  
  if(three_sea$PDA[i] < 0) {
    three_sea[i, 'GDA'] <- (sqrt(three_sea$Home.Expected.Goals[i]))/1
  } else{
    three_sea[i, 'GDA'] <- (2)/(1+three_sea$Home.Expected.Goals[i])
  }
  
  
  if(is.na(three_sea$Row.HID[i]) == TRUE) {
    
    three_sea[i,'EloHomeAtt'] <- three_sea$EloHomeAtt[i] + ((K*(three_sea$GAH[i]))*(three_sea$OAH[i] - (three_sea$EAH[i])))
    three_sea[i,'EloHomeDef'] <- three_sea$EloHomeDef[i] + ((K*(three_sea$GDH[i]))*(three_sea$ODH[i] - (three_sea$EDH[i])))
    three_sea[i, 'CAH'] <- ((K*(three_sea$GAH[i]))*(three_sea$OAH[i] - (three_sea$EAH[i])))
    three_sea[i, 'CDH'] <- ((K*(three_sea$GDH[i]))*(three_sea$ODH[i] - (three_sea$EDH[i])))
    
  } else{
    
    three_sea[three_sea$Row.HID[i],'EloHomeAtt'] <- three_sea$EloHomeAtt[i] + ((K*(three_sea$GAH[i]))*(three_sea$OAH[i] - (three_sea$EAH[i])))
    three_sea[three_sea$Row.HID[i],'EloHomeDef'] <- three_sea$EloHomeDef[i] + ((K*(three_sea$GDH[i]))*(three_sea$ODH[i] - (three_sea$EDH[i])))
    three_sea[i, 'CAH'] <- ((K*(three_sea$GAH[i]))*(three_sea$OAH[i] - (three_sea$EAH[i])))
    three_sea[i, 'CDH'] <- ((K*(three_sea$GDH[i]))*(three_sea$ODH[i] - (three_sea$EDH[i])))
    
  }
  
  if(is.na(three_sea$Row.AID[i]) == TRUE) {
    
    three_sea[i,'EloAwayDef'] <- three_sea$EloAwayDef[i] + ((K*(three_sea$GDA[i]))*(three_sea$ODA[i] - (three_sea$EDA[i])))
    three_sea[i,'EloAwayAtt'] <- three_sea$EloAwayAtt[i] + ((K*(three_sea$GAA[i]))*(three_sea$OAA[i] - (three_sea$EAA[i])))
    three_sea[i, 'CDA'] <- ((K*( three_sea$GDA[i]))*( three_sea$ODA[i] - ( three_sea$EDA[i])))
    three_sea[i, 'CAA'] <- ((K*( three_sea$GAA[i]))*( three_sea$OAA[i] - ( three_sea$EAA[i])))
    
  } else{
    
    three_sea[three_sea$Row.AID[i],'EloAwayDef'] <- three_sea$EloAwayDef[i] + ((K*(three_sea$GDA[i]))*(three_sea$ODA[i] - (three_sea$EDA[i])))
    three_sea[three_sea$Row.AID[i],'EloAwayAtt'] <- three_sea$EloAwayAtt[i] + ((K*(three_sea$GAA[i]))*(three_sea$OAA[i] - (three_sea$EAA[i]))) 
    three_sea[i, 'CDA'] <- ((K*( three_sea$GDA[i]))*( three_sea$ODA[i] - ( three_sea$EDA[i])))
    three_sea[i, 'CAA'] <- ((K*( three_sea$GAA[i]))*( three_sea$OAA[i] - ( three_sea$EAA[i])))
    
  }
  
}

#################################################################################

## End of Season 3

#################################################################################

four_sea <- func.data %>%  filter(func.data$Season == "2017/2018")

lastH <- three_sea %>% 
  group_by(HID) %>% 
  summarise(EloHomeAtt = last(EloHomeAtt),
            EloHomeDef = last(EloHomeDef))
lastA <- three_sea %>% 
  group_by(AID) %>% 
  summarise(EloAwayAtt = last(EloAwayAtt),
            EloAwayDef = last(EloAwayDef))

last <- cbind(lastH, lastA)

rm(lastA)
rm(lastH)

for(i in 1:nrow(last)){
  last$EloHomeAtt[i] <- (last$EloHomeAtt[i]*.8)+(.2*1500)
  last$EloHomeDef[i] <- (last$EloHomeDef[i]*.8)+(.2*1500)
  last$EloAwayAtt[i] <- (last$EloAwayAtt[i]*.8)+(.2*1500)
  last$EloAwayDef[i] <- (last$EloAwayDef[i]*.8)+(.2*1500)
}

rel.prom <- last[c(19,14,12),]

rel.prom$HID[rel.prom$HID == 8] <- 11
rel.prom$HID[rel.prom$HID == 23] <- 25
rel.prom$HID[rel.prom$HID == 21] <- 26
rel.prom$AID[rel.prom$AID == 8] <- 11
rel.prom$AID[rel.prom$AID == 23] <- 25
rel.prom$AID[rel.prom$AID == 21] <- 26

rel.prom$EloHomeAtt <- mean(rel.prom$EloHomeAtt)
rel.prom$EloHomeDef <- mean(rel.prom$EloHomeDef)
rel.prom$EloAwayAtt <- mean(rel.prom$EloAwayAtt)
rel.prom$EloAwayDef <- mean(rel.prom$EloAwayDef)

last[19,] <- rel.prom[1,]
last[14,] <- rel.prom[2,]
last[12,] <- rel.prom[3,]
rm(rel.prom)

for(i in 1:20){
  four_sea$EloHomeAtt[i] <- last$EloHomeAtt[four_sea$HID[i] == last$HID]
  four_sea$EloHomeDef[i] <- last$EloHomeDef[four_sea$HID[i] == last$HID]
  four_sea$EloAwayAtt[i] <- last$EloAwayAtt[four_sea$AID[i] == last$AID]
  four_sea$EloAwayDef[i] <- last$EloAwayDef[four_sea$AID[i] == last$AID]
}

four_sea <- four_sea %>% 
  mutate(Row = row_number())

four_sea$Row <- as.character(four_sea$Row)

four_sea <- four_sea %>% 
  group_by(HID) %>% 
  dplyr::mutate(Row.HID = dplyr::lead(Row, n=1L, default = NA)) %>% 
  ungroup()

four_sea <- four_sea %>% 
  group_by(AID) %>% 
  dplyr::mutate(Row.AID = dplyr::lead(Row, n=1L, default = NA)) %>% 
  ungroup()

four_sea[12, 'EloHomeAtt'] <- NA
four_sea[12, 'EloHomeDef'] <- NA
four_sea[12, 'EloAwayDef'] <- NA
four_sea[12, 'EloAwayAtt'] <- NA
four_sea[25, 'EloAwayAtt'] <- last[3, 'EloAwayAtt']
four_sea[25, 'EloAwayDef'] <- last[3, 'EloAwayDef']
four_sea[40, 'EloHomeAtt'] <- last[5, "EloHomeAtt"]
four_sea[40, 'EloHomeDef'] <- last[5, "EloHomeDef"]

##########################################################################

## 2017/18 season

##########################################################################

for(i in 1:nrow(four_sea)){
  
  four_sea[i, 'DAH'] <- (four_sea$EloHomeAtt[i]+HA)-four_sea$EloAwayDef[i]
  four_sea[i, 'DAA'] <- four_sea$EloAwayAtt[i]-(four_sea$EloHomeDef[i]+HA)
  four_sea[i, 'DDH'] <- (four_sea$EloHomeDef[i]+HAD)-(four_sea$EloAwayAtt[i])
  four_sea[i, 'DDA'] <- four_sea$EloAwayDef[i]-(four_sea$EloHomeAtt[i]+HAD)
  four_sea[i, 'EAH'] <- 1/(1+10^((-(four_sea$DAH[i]))/400))
  four_sea[i, 'EAA'] <- 1/(1+10^((-(four_sea$DAA[i]))/400))
  four_sea[i, 'EDH'] <- 1/(1+10^((-(four_sea$DDH[i]))/400))
  four_sea[i, 'EDA'] <- 1/(1+10^((-(four_sea$DDA[i]))/400))
  four_sea[i, 'PAH'] <- four_sea$OAH[i] - four_sea$EAH[i]
  four_sea[i, 'PAA'] <- four_sea$OAA[i] - four_sea$EAA[i]
  four_sea[i, 'PDH'] <- four_sea$ODH[i] - four_sea$EDH[i]
  four_sea[i, 'PDA'] <- four_sea$ODA[i] - four_sea$EDA[i]
  
  if(four_sea$PAH[i] < 0) {
    four_sea[i, 'GAH'] <- (2)/(1+four_sea$Home.Expected.Goals[i])
  } else{
    four_sea[i, 'GAH'] <- (sqrt(four_sea$Home.Expected.Goals[i]))/1
  }
  
  if(four_sea$PAA[i] < 0) {
    four_sea[i, 'GAA'] <- (2)/(1+four_sea$Away.Expected.Goals[i])
  } else{
    four_sea[i, 'GAA'] <- (sqrt(four_sea$Away.Expected.Goals[i]))/1
  }
  
  if(four_sea$PDH[i] < 0) {
    four_sea[i, 'GDH'] <- (sqrt(four_sea$Away.Expected.Goals[i]))/1
  } else{
    four_sea[i, 'GDH'] <- (2)/(1+four_sea$Away.Expected.Goals[i])
  }
  
  if(four_sea$PDA[i] < 0) {
    four_sea[i, 'GDA'] <- (sqrt(four_sea$Home.Expected.Goals[i]))/1
  } else{
    four_sea[i, 'GDA'] <- (2)/(1+four_sea$Home.Expected.Goals[i])
  }
  
  
  if(is.na(four_sea$Row.HID[i]) == TRUE) {
    
    four_sea[i,'EloHomeAtt'] <- four_sea$EloHomeAtt[i] + ((K*(four_sea$GAH[i]))*(four_sea$OAH[i] - (four_sea$EAH[i])))
    four_sea[i,'EloHomeDef'] <- four_sea$EloHomeDef[i] + ((K*(four_sea$GDH[i]))*(four_sea$ODH[i] - (four_sea$EDH[i])))
    four_sea[i, 'CAH'] <- ((K*(four_sea$GAH[i]))*(four_sea$OAH[i] - (four_sea$EAH[i])))
    four_sea[i, 'CDH'] <- ((K*(four_sea$GDH[i]))*(four_sea$ODH[i] - (four_sea$EDH[i])))
    
  } else{
    
    four_sea[four_sea$Row.HID[i],'EloHomeAtt'] <- four_sea$EloHomeAtt[i] + ((K*(four_sea$GAH[i]))*(four_sea$OAH[i] - (four_sea$EAH[i])))
    four_sea[four_sea$Row.HID[i],'EloHomeDef'] <- four_sea$EloHomeDef[i] + ((K*(four_sea$GDH[i]))*(four_sea$ODH[i] - (four_sea$EDH[i])))
    four_sea[i, 'CAH'] <- ((K*(four_sea$GAH[i]))*(four_sea$OAH[i] - (four_sea$EAH[i])))
    four_sea[i, 'CDH'] <- ((K*(four_sea$GDH[i]))*(four_sea$ODH[i] - (four_sea$EDH[i])))
    
  }
  
  if(is.na(four_sea$Row.AID[i]) == TRUE) {
    
    four_sea[i,'EloAwayDef'] <- four_sea$EloAwayDef[i] + ((K*(four_sea$GDA[i]))*(four_sea$ODA[i] - (four_sea$EDA[i])))
    four_sea[i,'EloAwayAtt'] <- four_sea$EloAwayAtt[i] + ((K*(four_sea$GAA[i]))*(four_sea$OAA[i] - (four_sea$EAA[i])))
    four_sea[i, 'CDA'] <- ((K*( four_sea$GDA[i]))*( four_sea$ODA[i] - ( four_sea$EDA[i])))
    four_sea[i, 'CAA'] <- ((K*( four_sea$GAA[i]))*( four_sea$OAA[i] - ( four_sea$EAA[i])))
    
  } else{
    
    four_sea[four_sea$Row.AID[i],'EloAwayDef'] <- four_sea$EloAwayDef[i] + ((K*(four_sea$GDA[i]))*(four_sea$ODA[i] - (four_sea$EDA[i])))
    four_sea[four_sea$Row.AID[i],'EloAwayAtt'] <- four_sea$EloAwayAtt[i] + ((K*(four_sea$GAA[i]))*(four_sea$OAA[i] - (four_sea$EAA[i]))) 
    four_sea[i, 'CDA'] <- ((K*( four_sea$GDA[i]))*( four_sea$ODA[i] - ( four_sea$EDA[i])))
    four_sea[i, 'CAA'] <- ((K*( four_sea$GAA[i]))*( four_sea$OAA[i] - ( four_sea$EAA[i])))
    
  }
  
}

###############################################################################

## End of season 4

###############################################################################

five_sea <- func.data %>%  filter(func.data$Season == "2018/2019")

lastH <- four_sea %>% 
  group_by(HID) %>% 
  summarise(EloHomeAtt = last(EloHomeAtt),
            EloHomeDef = last(EloHomeDef))
lastA <- four_sea %>% 
  group_by(AID) %>% 
  summarise(EloAwayAtt = last(EloAwayAtt),
            EloAwayDef = last(EloAwayDef))

last <- cbind(lastH, lastA)

rm(lastA)
rm(lastH)

for(i in 1:nrow(last)){
  last$EloHomeAtt[i] <- (last$EloHomeAtt[i]*.8)+(.2*1500)
  last$EloHomeDef[i] <- (last$EloHomeDef[i]*.8)+(.2*1500)
  last$EloAwayAtt[i] <- (last$EloAwayAtt[i]*.8)+(.2*1500)
  last$EloAwayDef[i] <- (last$EloAwayDef[i]*.8)+(.2*1500)
}

rel.prom <- last[c(2,7,9),]

rel.prom$HID[rel.prom$HID == 10] <- 28
rel.prom$HID[rel.prom$HID == 15] <- 29
rel.prom$HID[rel.prom$HID == 17] <- 27
rel.prom$AID[rel.prom$AID == 10] <- 28
rel.prom$AID[rel.prom$AID == 15] <- 29
rel.prom$AID[rel.prom$AID == 17] <- 27

rel.prom$EloHomeAtt <- mean(rel.prom$EloHomeAtt)
rel.prom$EloHomeDef <- mean(rel.prom$EloHomeDef)
rel.prom$EloAwayAtt <- mean(rel.prom$EloAwayAtt)
rel.prom$EloAwayDef <- mean(rel.prom$EloAwayDef)

last[2,] <- rel.prom[1,]
last[7,] <- rel.prom[2,]
last[9,] <- rel.prom[3,]
rm(rel.prom)

for(i in 1:20){
  five_sea$EloHomeAtt[i] <- last$EloHomeAtt[five_sea$HID[i] == last$HID]
  five_sea$EloHomeDef[i] <- last$EloHomeDef[five_sea$HID[i] == last$HID]
  five_sea$EloAwayAtt[i] <- last$EloAwayAtt[five_sea$AID[i] == last$AID]
  five_sea$EloAwayDef[i] <- last$EloAwayDef[five_sea$AID[i] == last$AID]
}

five_sea <- five_sea %>% 
  mutate(Row = row_number())

five_sea$Row <- as.character(five_sea$Row)

five_sea <- five_sea %>% 
  group_by(HID) %>% 
  dplyr::mutate(Row.HID = dplyr::lead(Row, n=1L, default = NA)) %>% 
  ungroup()

five_sea <- five_sea %>% 
  group_by(AID) %>% 
  dplyr::mutate(Row.AID = dplyr::lead(Row, n=1L, default = NA)) %>% 
  ungroup()

###########################################################################

## 2018/19 Loop

###########################################################################

for(i in 1:nrow(five_sea)){
  
  five_sea[i, 'DAH'] <- (five_sea$EloHomeAtt[i]+HA)-five_sea$EloAwayDef[i]
  five_sea[i, 'DAA'] <- five_sea$EloAwayAtt[i]-(five_sea$EloHomeDef[i]+HA)
  five_sea[i, 'DDH'] <- (five_sea$EloHomeDef[i]+HAD)-(five_sea$EloAwayAtt[i])
  five_sea[i, 'DDA'] <- five_sea$EloAwayDef[i]-(five_sea$EloHomeAtt[i]+HAD)
  five_sea[i, 'EAH'] <- 1/(1+10^((-(five_sea$DAH[i]))/400))
  five_sea[i, 'EAA'] <- 1/(1+10^((-(five_sea$DAA[i]))/400))
  five_sea[i, 'EDH'] <- 1/(1+10^((-(five_sea$DDH[i]))/400))
  five_sea[i, 'EDA'] <- 1/(1+10^((-(five_sea$DDA[i]))/400))
  five_sea[i, 'PAH'] <- five_sea$OAH[i] - five_sea$EAH[i]
  five_sea[i, 'PAA'] <- five_sea$OAA[i] - five_sea$EAA[i]
  five_sea[i, 'PDH'] <- five_sea$ODH[i] - five_sea$EDH[i]
  five_sea[i, 'PDA'] <- five_sea$ODA[i] - five_sea$EDA[i]
  
  if(five_sea$PAH[i] < 0) {
    five_sea[i, 'GAH'] <- (2)/(1+five_sea$Home.Expected.Goals[i])
  } else{
    five_sea[i, 'GAH'] <- (sqrt(five_sea$Home.Expected.Goals[i]))/1
  }
  
  if(five_sea$PAA[i] < 0) {
    five_sea[i, 'GAA'] <- (2)/(1+five_sea$Away.Expected.Goals[i])
  } else{
    five_sea[i, 'GAA'] <- (sqrt(five_sea$Away.Expected.Goals[i]))/1
  }
  
  if(five_sea$PDH[i] < 0) {
    five_sea[i, 'GDH'] <- (sqrt(five_sea$Away.Expected.Goals[i]))/1
  } else{
    five_sea[i, 'GDH'] <- (2)/(1+five_sea$Away.Expected.Goals[i])
  }
  
  if(five_sea$PDA[i] < 0) {
    five_sea[i, 'GDA'] <- (sqrt(five_sea$Home.Expected.Goals[i]))/1
  } else{
    five_sea[i, 'GDA'] <- (2)/(1+five_sea$Home.Expected.Goals[i])
  }
  
  
  if(is.na(five_sea$Row.HID[i]) == TRUE) {
    
    five_sea[i,'EloHomeAtt'] <- five_sea$EloHomeAtt[i] + ((K*(five_sea$GAH[i]))*(five_sea$OAH[i] - (five_sea$EAH[i])))
    five_sea[i,'EloHomeDef'] <- five_sea$EloHomeDef[i] + ((K*(five_sea$GDH[i]))*(five_sea$ODH[i] - (five_sea$EDH[i])))
    five_sea[i, 'CAH'] <- ((K*(five_sea$GAH[i]))*(five_sea$OAH[i] - (five_sea$EAH[i])))
    five_sea[i, 'CDH'] <- ((K*(five_sea$GDH[i]))*(five_sea$ODH[i] - (five_sea$EDH[i])))
    
  } else{
    
    five_sea[five_sea$Row.HID[i],'EloHomeAtt'] <- five_sea$EloHomeAtt[i] + ((K*(five_sea$GAH[i]))*(five_sea$OAH[i] - (five_sea$EAH[i])))
    five_sea[five_sea$Row.HID[i],'EloHomeDef'] <- five_sea$EloHomeDef[i] + ((K*(five_sea$GDH[i]))*(five_sea$ODH[i] - (five_sea$EDH[i])))
    five_sea[i, 'CAH'] <- ((K*(five_sea$GAH[i]))*(five_sea$OAH[i] - (five_sea$EAH[i])))
    five_sea[i, 'CDH'] <- ((K*(five_sea$GDH[i]))*(five_sea$ODH[i] - (five_sea$EDH[i])))
    
  }
  
  if(is.na(five_sea$Row.AID[i]) == TRUE) {
    
    five_sea[i,'EloAwayDef'] <- five_sea$EloAwayDef[i] + ((K*(five_sea$GDA[i]))*(five_sea$ODA[i] - (five_sea$EDA[i])))
    five_sea[i,'EloAwayAtt'] <- five_sea$EloAwayAtt[i] + ((K*(five_sea$GAA[i]))*(five_sea$OAA[i] - (five_sea$EAA[i])))
    five_sea[i, 'CDA'] <- ((K*( five_sea$GDA[i]))*( five_sea$ODA[i] - ( five_sea$EDA[i])))
    five_sea[i, 'CAA'] <- ((K*( five_sea$GAA[i]))*( five_sea$OAA[i] - ( five_sea$EAA[i])))
    
  } else{
    
    five_sea[five_sea$Row.AID[i],'EloAwayDef'] <- five_sea$EloAwayDef[i] + ((K*(five_sea$GDA[i]))*(five_sea$ODA[i] - (five_sea$EDA[i])))
    five_sea[five_sea$Row.AID[i],'EloAwayAtt'] <- five_sea$EloAwayAtt[i] + ((K*(five_sea$GAA[i]))*(five_sea$OAA[i] - (five_sea$EAA[i]))) 
    five_sea[i, 'CDA'] <- ((K*( five_sea$GDA[i]))*( five_sea$ODA[i] - ( five_sea$EDA[i])))
    five_sea[i, 'CAA'] <- ((K*( five_sea$GAA[i]))*( five_sea$OAA[i] - ( five_sea$EAA[i])))
    
  }
  
}

##############################################################################

## End of Season 5

##############################################################################

six_sea <- func.data %>%  filter(func.data$Season == "2019/2020")

lastH <- five_sea %>% 
  group_by(HID) %>% 
  summarise(EloHomeAtt = last(EloHomeAtt),
            EloHomeDef = last(EloHomeDef))
lastA <- five_sea %>% 
  group_by(AID) %>% 
  summarise(EloAwayAtt = last(EloAwayAtt),
            EloAwayDef = last(EloAwayDef))

last <- cbind(lastH, lastA)

rm(lastA)
rm(lastH)

for(i in 1:nrow(last)){
  last$EloHomeAtt[i] <- (last$EloHomeAtt[i]*.8)+(.2*1500)
  last$EloHomeDef[i] <- (last$EloHomeDef[i]*.8)+(.2*1500)
  last$EloAwayAtt[i] <- (last$EloAwayAtt[i]*.8)+(.2*1500)
  last$EloAwayDef[i] <- (last$EloAwayDef[i]*.8)+(.2*1500)
}

rel.prom <- last[c(15,13,12),]

rel.prom$HID[rel.prom$HID == 29] <- 19
rel.prom$HID[rel.prom$HID == 27] <- 30
rel.prom$HID[rel.prom$HID == 26] <- 4
rel.prom$AID[rel.prom$AID == 29] <- 19
rel.prom$AID[rel.prom$AID == 27] <- 30
rel.prom$AID[rel.prom$AID == 26] <- 4

rel.prom$EloHomeAtt <- mean(rel.prom$EloHomeAtt)
rel.prom$EloHomeDef <- mean(rel.prom$EloHomeDef)
rel.prom$EloAwayAtt <- mean(rel.prom$EloAwayAtt)
rel.prom$EloAwayDef <- mean(rel.prom$EloAwayDef)

last[15,] <- rel.prom[1,]
last[13,] <- rel.prom[2,]
last[12,] <- rel.prom[3,]
rm(rel.prom)

for(i in 1:20){
  six_sea$EloHomeAtt[i] <- last$EloHomeAtt[six_sea$HID[i] == last$HID]
  six_sea$EloHomeDef[i] <- last$EloHomeDef[six_sea$HID[i] == last$HID]
  six_sea$EloAwayAtt[i] <- last$EloAwayAtt[six_sea$AID[i] == last$AID]
  six_sea$EloAwayDef[i] <- last$EloAwayDef[six_sea$AID[i] == last$AID]
}

six_sea <- six_sea %>% 
  mutate(Row = row_number())

six_sea$Row <- as.character(six_sea$Row)

six_sea <- six_sea %>% 
  group_by(HID) %>% 
  dplyr::mutate(Row.HID = dplyr::lead(Row, n=1L, default = NA)) %>% 
  ungroup()

six_sea <- six_sea %>% 
  group_by(AID) %>% 
  dplyr::mutate(Row.AID = dplyr::lead(Row, n=1L, default = NA)) %>% 
  ungroup()

##############################################################################

## 2019/20 Season Loop

##############################################################################

for(i in 1:nrow(six_sea)){
  
  six_sea[i, 'DAH'] <- (six_sea$EloHomeAtt[i]+HA)-six_sea$EloAwayDef[i]
  six_sea[i, 'DAA'] <- six_sea$EloAwayAtt[i]-(six_sea$EloHomeDef[i]+HA)
  six_sea[i, 'DDH'] <- (six_sea$EloHomeDef[i]+HAD)-(six_sea$EloAwayAtt[i])
  six_sea[i, 'DDA'] <- six_sea$EloAwayDef[i]-(six_sea$EloHomeAtt[i]+HAD)
  six_sea[i, 'EAH'] <- 1/(1+10^((-(six_sea$DAH[i]))/400))
  six_sea[i, 'EAA'] <- 1/(1+10^((-(six_sea$DAA[i]))/400))
  six_sea[i, 'EDH'] <- 1/(1+10^((-(six_sea$DDH[i]))/400))
  six_sea[i, 'EDA'] <- 1/(1+10^((-(six_sea$DDA[i]))/400))
  six_sea[i, 'PAH'] <- six_sea$OAH[i] - six_sea$EAH[i]
  six_sea[i, 'PAA'] <- six_sea$OAA[i] - six_sea$EAA[i]
  six_sea[i, 'PDH'] <- six_sea$ODH[i] - six_sea$EDH[i]
  six_sea[i, 'PDA'] <- six_sea$ODA[i] - six_sea$EDA[i]
  
  if(six_sea$PAH[i] < 0) {
    six_sea[i, 'GAH'] <- (2)/(1+six_sea$Home.Expected.Goals[i])
  } else{
    six_sea[i, 'GAH'] <- (sqrt(six_sea$Home.Expected.Goals[i]))/1
  }
  
  if(six_sea$PAA[i] < 0) {
    six_sea[i, 'GAA'] <- (2)/(1+six_sea$Away.Expected.Goals[i])
  } else{
    six_sea[i, 'GAA'] <- (sqrt(six_sea$Away.Expected.Goals[i]))/1
  }
  
  if(six_sea$PDH[i] < 0) {
    six_sea[i, 'GDH'] <- (sqrt(six_sea$Away.Expected.Goals[i]))/1
  } else{
    six_sea[i, 'GDH'] <- (2)/(1+six_sea$Away.Expected.Goals[i])
  }
  
  if(six_sea$PDA[i] < 0) {
    six_sea[i, 'GDA'] <- (sqrt(six_sea$Home.Expected.Goals[i]))/1
  } else{
    six_sea[i, 'GDA'] <- (2)/(1+six_sea$Home.Expected.Goals[i])
  }
  
  
  if(is.na(six_sea$Row.HID[i]) == TRUE) {
    
    six_sea[i,'EloHomeAtt'] <- six_sea$EloHomeAtt[i] + ((K*(six_sea$GAH[i]))*(six_sea$OAH[i] - (six_sea$EAH[i])))
    six_sea[i,'EloHomeDef'] <- six_sea$EloHomeDef[i] + ((K*(six_sea$GDH[i]))*(six_sea$ODH[i] - (six_sea$EDH[i])))
    six_sea[i, 'CAH'] <- ((K*(six_sea$GAH[i]))*(six_sea$OAH[i] - (six_sea$EAH[i])))
    six_sea[i, 'CDH'] <- ((K*(six_sea$GDH[i]))*(six_sea$ODH[i] - (six_sea$EDH[i])))
    
  } else{
    
    six_sea[six_sea$Row.HID[i],'EloHomeAtt'] <- six_sea$EloHomeAtt[i] + ((K*(six_sea$GAH[i]))*(six_sea$OAH[i] - (six_sea$EAH[i])))
    six_sea[six_sea$Row.HID[i],'EloHomeDef'] <- six_sea$EloHomeDef[i] + ((K*(six_sea$GDH[i]))*(six_sea$ODH[i] - (six_sea$EDH[i])))
    six_sea[i, 'CAH'] <- ((K*(six_sea$GAH[i]))*(six_sea$OAH[i] - (six_sea$EAH[i])))
    six_sea[i, 'CDH'] <- ((K*(six_sea$GDH[i]))*(six_sea$ODH[i] - (six_sea$EDH[i])))
    
  }
  
  if(is.na(six_sea$Row.AID[i]) == TRUE) {
    
    six_sea[i,'EloAwayDef'] <- six_sea$EloAwayDef[i] + ((K*(six_sea$GDA[i]))*(six_sea$ODA[i] - (six_sea$EDA[i])))
    six_sea[i,'EloAwayAtt'] <- six_sea$EloAwayAtt[i] + ((K*(six_sea$GAA[i]))*(six_sea$OAA[i] - (six_sea$EAA[i])))
    six_sea[i, 'CDA'] <- ((K*( six_sea$GDA[i]))*( six_sea$ODA[i] - ( six_sea$EDA[i])))
    six_sea[i, 'CAA'] <- ((K*( six_sea$GAA[i]))*( six_sea$OAA[i] - ( six_sea$EAA[i])))
    
  } else{
    
    six_sea[six_sea$Row.AID[i],'EloAwayDef'] <- six_sea$EloAwayDef[i] + ((K*(six_sea$GDA[i]))*(six_sea$ODA[i] - (six_sea$EDA[i])))
    six_sea[six_sea$Row.AID[i],'EloAwayAtt'] <- six_sea$EloAwayAtt[i] + ((K*(six_sea$GAA[i]))*(six_sea$OAA[i] - (six_sea$EAA[i]))) 
    six_sea[i, 'CDA'] <- ((K*( six_sea$GDA[i]))*( six_sea$ODA[i] - ( six_sea$EDA[i])))
    six_sea[i, 'CAA'] <- ((K*( six_sea$GAA[i]))*( six_sea$OAA[i] - ( six_sea$EAA[i])))
    
  }
  
}


################################################################################

## End of Season 6

################################################################################

last_sea <- func.data %>% filter(Season == "2020/2021")

lastH <- six_sea %>% 
  group_by(HID) %>% 
  summarise(EloHomeAtt = last(EloHomeAtt),
            EloHomeDef = last(EloHomeDef))
lastA <- six_sea %>% 
  group_by(AID) %>% 
  summarise(EloAwayAtt = last(EloAwayAtt),
            EloAwayDef = last(EloAwayDef))

last <- cbind(lastH, lastA)

rm(lastA)
rm(lastH)

for(i in 1:nrow(last)){
  last$EloHomeAtt[i] <- (last$EloHomeAtt[i]*.8)+(.2*1500)
  last$EloHomeDef[i] <- (last$EloHomeDef[i]*.8)+(.2*1500)
  last$EloAwayAtt[i] <- (last$EloAwayAtt[i]*.8)+(.2*1500)
  last$EloAwayDef[i] <- (last$EloAwayDef[i]*.8)+(.2*1500)
}

rel.prom <- last[c(14,8,18),]

rel.prom$HID[rel.prom$HID == 3] <- 31
rel.prom$HID[rel.prom$HID == 19] <- 17
rel.prom$HID[rel.prom$HID == 6] <- 27
rel.prom$AID[rel.prom$AID == 3] <- 31
rel.prom$AID[rel.prom$AID == 19] <- 17
rel.prom$AID[rel.prom$AID == 6] <- 27

rel.prom$EloHomeAtt <- mean(rel.prom$EloHomeAtt)
rel.prom$EloHomeDef <- mean(rel.prom$EloHomeDef)
rel.prom$EloAwayAtt <- mean(rel.prom$EloAwayAtt)
rel.prom$EloAwayDef <- mean(rel.prom$EloAwayDef)

last[14,] <- rel.prom[1,]
last[8,] <- rel.prom[2,]
last[18,] <- rel.prom[3,]
rm(rel.prom)




for(i in 1:20){
  last_sea$EloHomeAtt[i] <- last$EloHomeAtt[last_sea$HID[i] == last$HID]
  last_sea$EloHomeDef[i] <- last$EloHomeDef[last_sea$HID[i] == last$HID]
  last_sea$EloAwayAtt[i] <- last$EloAwayAtt[last_sea$AID[i] == last$AID]
  last_sea$EloAwayDef[i] <- last$EloAwayDef[last_sea$AID[i] == last$AID]
}

last_sea <- last_sea %>% 
  mutate(Row = row_number())

last_sea$Row <- as.character(last_sea$Row)

last_sea <- last_sea %>% 
  group_by(HID) %>% 
  dplyr::mutate(Row.HID = dplyr::lead(Row, n=1L, default = NA)) %>% 
  ungroup()

last_sea <- last_sea %>% 
  group_by(AID) %>% 
  dplyr::mutate(Row.AID = dplyr::lead(Row, n=1L, default = NA)) %>% 
  ungroup()

last_sea[20, 'EloHomeAtt'] <- NA
last_sea[20, 'EloHomeDef'] <- NA
last_sea[20, 'EloAwayAtt'] <- NA
last_sea[20, 'EloAwayDef'] <- NA
last_sea[19, 'EloHomeAtt'] <- NA
last_sea[19, 'EloHomeDef'] <- NA

last_sea[22, 'EloHomeAtt'] <- last[11, 'EloHomeAtt']
last_sea[22, 'EloHomeDef'] <- last[11, 'EloHomeDef']

last_sea[25, 'EloHomeAtt'] <- last[7, 'EloHomeAtt']
last_sea[25, 'EloHomeDef'] <- last[7, 'EloHomeDef']

last_sea[27, 'EloAwayAtt'] <- last[16, 'EloAwayAtt']
last_sea[27, 'EloAwayDef'] <- last[16, 'EloAwayDef']

##################################################################################

## 2020/21 season

##################################################################################

for(i in 1:nrow(last_sea)){
  
  last_sea[i, 'DAH'] <- (last_sea$EloHomeAtt[i]+HA)-last_sea$EloAwayDef[i]
  last_sea[i, 'DAA'] <- last_sea$EloAwayAtt[i]-(last_sea$EloHomeDef[i]+HA)
  last_sea[i, 'DDH'] <- (last_sea$EloHomeDef[i]+HAD)-(last_sea$EloAwayAtt[i])
  last_sea[i, 'DDA'] <- last_sea$EloAwayDef[i]-(last_sea$EloHomeAtt[i]+HAD)
  last_sea[i, 'EAH'] <- 1/(1+10^((-(last_sea$DAH[i]))/400))
  last_sea[i, 'EAA'] <- 1/(1+10^((-(last_sea$DAA[i]))/400))
  last_sea[i, 'EDH'] <- 1/(1+10^((-(last_sea$DDH[i]))/400))
  last_sea[i, 'EDA'] <- 1/(1+10^((-(last_sea$DDA[i]))/400))
  last_sea[i, 'PAH'] <- last_sea$OAH[i] - last_sea$EAH[i]
  last_sea[i, 'PAA'] <- last_sea$OAA[i] - last_sea$EAA[i]
  last_sea[i, 'PDH'] <- last_sea$ODH[i] - last_sea$EDH[i]
  last_sea[i, 'PDA'] <- last_sea$ODA[i] - last_sea$EDA[i]
  
  if(last_sea$PAH[i] < 0) {
    last_sea[i, 'GAH'] <- (2)/(1+last_sea$Home.Expected.Goals[i])
  } else{
    last_sea[i, 'GAH'] <- (sqrt(last_sea$Home.Expected.Goals[i]))/1
  }
  
  if(last_sea$PAA[i] < 0) {
    last_sea[i, 'GAA'] <- (2)/(1+last_sea$Away.Expected.Goals[i])
  } else{
    last_sea[i, 'GAA'] <- (sqrt(last_sea$Away.Expected.Goals[i]))/1
  }
  
  if(last_sea$PDH[i] < 0) {
    last_sea[i, 'GDH'] <- (sqrt(last_sea$Away.Expected.Goals[i]))/1
  } else{
    last_sea[i, 'GDH'] <- (2)/(1+last_sea$Away.Expected.Goals[i])
  }
  
  if(last_sea$PDA[i] < 0) {
    last_sea[i, 'GDA'] <- (sqrt(last_sea$Home.Expected.Goals[i]))/1
  } else{
    last_sea[i, 'GDA'] <- (2)/(1+last_sea$Home.Expected.Goals[i])
  }
  
  
  if(is.na(last_sea$Row.HID[i]) == TRUE) {
    
    last_sea[i,'EloHomeAtt'] <- last_sea$EloHomeAtt[i] + ((K*(last_sea$GAH[i]))*(last_sea$OAH[i] - (last_sea$EAH[i])))
    last_sea[i,'EloHomeDef'] <- last_sea$EloHomeDef[i] + ((K*(last_sea$GDH[i]))*(last_sea$ODH[i] - (last_sea$EDH[i])))
    last_sea[i, 'CAH'] <- ((K*(last_sea$GAH[i]))*(last_sea$OAH[i] - (last_sea$EAH[i])))
    last_sea[i, 'CDH'] <- ((K*(last_sea$GDH[i]))*(last_sea$ODH[i] - (last_sea$EDH[i])))
    
  } else{
    
    last_sea[last_sea$Row.HID[i],'EloHomeAtt'] <- last_sea$EloHomeAtt[i] + ((K*(last_sea$GAH[i]))*(last_sea$OAH[i] - (last_sea$EAH[i])))
    last_sea[last_sea$Row.HID[i],'EloHomeDef'] <- last_sea$EloHomeDef[i] + ((K*(last_sea$GDH[i]))*(last_sea$ODH[i] - (last_sea$EDH[i])))
    last_sea[i, 'CAH'] <- ((K*(last_sea$GAH[i]))*(last_sea$OAH[i] - (last_sea$EAH[i])))
    last_sea[i, 'CDH'] <- ((K*(last_sea$GDH[i]))*(last_sea$ODH[i] - (last_sea$EDH[i])))
    
  }
  
  if(is.na(last_sea$Row.AID[i]) == TRUE) {
    
    last_sea[i,'EloAwayDef'] <- last_sea$EloAwayDef[i] + ((K*(last_sea$GDA[i]))*(last_sea$ODA[i] - (last_sea$EDA[i])))
    last_sea[i,'EloAwayAtt'] <- last_sea$EloAwayAtt[i] + ((K*(last_sea$GAA[i]))*(last_sea$OAA[i] - (last_sea$EAA[i])))
    last_sea[i, 'CDA'] <- ((K*(last_sea$GDA[i]))*(last_sea$ODA[i] - (last_sea$EDA[i])))
    last_sea[i, 'CAA'] <- ((K*(last_sea$GAA[i]))*(last_sea$OAA[i] - (last_sea$EAA[i])))
    
  } else{
    
    last_sea[last_sea$Row.AID[i],'EloAwayDef'] <- last_sea$EloAwayDef[i] + ((K*(last_sea$GDA[i]))*(last_sea$ODA[i] - (last_sea$EDA[i])))
    last_sea[last_sea$Row.AID[i],'EloAwayAtt'] <- last_sea$EloAwayAtt[i] + ((K*(last_sea$GAA[i]))*(last_sea$OAA[i] - (last_sea$EAA[i]))) 
    last_sea[i, 'CDA'] <- ((K*(last_sea$GDA[i]))*(last_sea$ODA[i] - (last_sea$EDA[i])))
    last_sea[i, 'CAA'] <- ((K*(last_sea$GAA[i]))*(last_sea$OAA[i] - (last_sea$EAA[i])))
    
  }
  
}

############################################################################

## End of Season 7

############################################################################

eight_sea <- func.data %>% filter(Season == "2021/2022")

lastH <- last_sea %>% 
  group_by(HID) %>% 
  summarise(EloHomeAtt = last(EloHomeAtt),
            EloHomeDef = last(EloHomeDef))
lastA <- last_sea %>% 
  group_by(AID) %>% 
  summarise(EloAwayAtt = last(EloAwayAtt),
            EloAwayDef = last(EloAwayDef))

last <- cbind(lastH, lastA)

rm(lastA)
rm(lastH)

for(i in 1:nrow(last)){
  last$EloHomeAtt[i] <- (last$EloHomeAtt[i]*.8)+(.2*1500)
  last$EloHomeDef[i] <- (last$EloHomeDef[i]*.8)+(.2*1500)
  last$EloAwayAtt[i] <- (last$EloAwayAtt[i]*.8)+(.2*1500)
  last$EloAwayDef[i] <- (last$EloAwayDef[i]*.8)+(.2*1500)
}

rel.prom <- last[c(13,7,15),]

rel.prom$HID[rel.prom$HID == 27] <- 19
rel.prom$HID[rel.prom$HID == 17] <- 6
rel.prom$HID[rel.prom$HID == 30] <- 32
rel.prom$AID[rel.prom$AID == 27] <- 19
rel.prom$AID[rel.prom$AID == 17] <- 6
rel.prom$AID[rel.prom$AID == 30] <- 32

rel.prom$EloHomeAtt <- mean(rel.prom$EloHomeAtt)
rel.prom$EloHomeDef <- mean(rel.prom$EloHomeDef)
rel.prom$EloAwayAtt <- mean(rel.prom$EloAwayAtt)
rel.prom$EloAwayDef <- mean(rel.prom$EloAwayDef)

last[13,] <- rel.prom[1,]
last[7,] <- rel.prom[2,]
last[15,] <- rel.prom[3,]
rm(rel.prom)




for(i in 1:20){
  eight_sea$EloHomeAtt[i] <- last$EloHomeAtt[eight_sea$HID[i] == last$HID]
  eight_sea$EloHomeDef[i] <- last$EloHomeDef[eight_sea$HID[i] == last$HID]
  eight_sea$EloAwayAtt[i] <- last$EloAwayAtt[eight_sea$AID[i] == last$AID]
  eight_sea$EloAwayDef[i] <- last$EloAwayDef[eight_sea$AID[i] == last$AID]
}

eight_sea <- eight_sea %>% 
  mutate(Row = row_number())

eight_sea$Row <- as.character(eight_sea$Row)

eight_sea <- eight_sea %>% 
  group_by(HID) %>% 
  dplyr::mutate(Row.HID = dplyr::lead(Row, n=1L, default = NA)) %>% 
  ungroup()

eight_sea <- eight_sea %>% 
  group_by(AID) %>% 
  dplyr::mutate(Row.AID = dplyr::lead(Row, n=1L, default = NA)) %>% 
  ungroup()

##############################################################################

## 2021/22

##############################################################################

for(i in 1:nrow(eight_sea)){
  
  eight_sea[i, 'DAH'] <- (eight_sea$EloHomeAtt[i]+HA)-eight_sea$EloAwayDef[i]
  eight_sea[i, 'DAA'] <- eight_sea$EloAwayAtt[i]-(eight_sea$EloHomeDef[i]+HA)
  eight_sea[i, 'DDH'] <- (eight_sea$EloHomeDef[i]+HAD)-(eight_sea$EloAwayAtt[i])
  eight_sea[i, 'DDA'] <- eight_sea$EloAwayDef[i]-(eight_sea$EloHomeAtt[i]+HAD)
  eight_sea[i, 'EAH'] <- 1/(1+10^((-(eight_sea$DAH[i]))/400))
  eight_sea[i, 'EAA'] <- 1/(1+10^((-(eight_sea$DAA[i]))/400))
  eight_sea[i, 'EDH'] <- 1/(1+10^((-(eight_sea$DDH[i]))/400))
  eight_sea[i, 'EDA'] <- 1/(1+10^((-(eight_sea$DDA[i]))/400))
  eight_sea[i, 'PAH'] <- eight_sea$OAH[i] - eight_sea$EAH[i]
  eight_sea[i, 'PAA'] <- eight_sea$OAA[i] - eight_sea$EAA[i]
  eight_sea[i, 'PDH'] <- eight_sea$ODH[i] - eight_sea$EDH[i]
  eight_sea[i, 'PDA'] <- eight_sea$ODA[i] - eight_sea$EDA[i]
  
  if(eight_sea$PAH[i] < 0) {
    eight_sea[i, 'GAH'] <- (2)/(1+eight_sea$Home.Expected.Goals[i])
  } else{
    eight_sea[i, 'GAH'] <- (sqrt(eight_sea$Home.Expected.Goals[i]))/1
  }
  
  if(eight_sea$PAA[i] < 0) {
    eight_sea[i, 'GAA'] <- (2)/(1+eight_sea$Away.Expected.Goals[i])
  } else{
    eight_sea[i, 'GAA'] <- (sqrt(eight_sea$Away.Expected.Goals[i]))/1
  }
  
  if(eight_sea$PDH[i] < 0) {
    eight_sea[i, 'GDH'] <- (sqrt(eight_sea$Away.Expected.Goals[i]))/1
  } else{
    eight_sea[i, 'GDH'] <- (2)/(1+eight_sea$Away.Expected.Goals[i])
  }
  
  if(eight_sea$PDA[i] < 0) {
    eight_sea[i, 'GDA'] <- (sqrt(eight_sea$Home.Expected.Goals[i]))/1
  } else{
    eight_sea[i, 'GDA'] <- (2)/(1+eight_sea$Home.Expected.Goals[i])
  }
  
  
  if(is.na(eight_sea$Row.HID[i]) == TRUE) {
    
    eight_sea[i,'EloHomeAtt'] <- eight_sea$EloHomeAtt[i] + ((K*(eight_sea$GAH[i]))*(eight_sea$OAH[i] - (eight_sea$EAH[i])))
    eight_sea[i,'EloHomeDef'] <- eight_sea$EloHomeDef[i] + ((K*(eight_sea$GDH[i]))*(eight_sea$ODH[i] - (eight_sea$EDH[i])))
    eight_sea[i, 'CAH'] <- ((K*(eight_sea$GAH[i]))*(eight_sea$OAH[i] - (eight_sea$EAH[i])))
    eight_sea[i, 'CDH'] <- ((K*(eight_sea$GDH[i]))*(eight_sea$ODH[i] - (eight_sea$EDH[i])))
    
  } else{
    
    eight_sea[eight_sea$Row.HID[i],'EloHomeAtt'] <- eight_sea$EloHomeAtt[i] + ((K*(eight_sea$GAH[i]))*(eight_sea$OAH[i] - (eight_sea$EAH[i])))
    eight_sea[eight_sea$Row.HID[i],'EloHomeDef'] <- eight_sea$EloHomeDef[i] + ((K*(eight_sea$GDH[i]))*(eight_sea$ODH[i] - (eight_sea$EDH[i])))
    eight_sea[i, 'CAH'] <- ((K*(eight_sea$GAH[i]))*(eight_sea$OAH[i] - (eight_sea$EAH[i])))
    eight_sea[i, 'CDH'] <- ((K*(eight_sea$GDH[i]))*(eight_sea$ODH[i] - (eight_sea$EDH[i])))
    
  }
  
  if(is.na(eight_sea$Row.AID[i]) == TRUE) {
    
    eight_sea[i,'EloAwayDef'] <- eight_sea$EloAwayDef[i] + ((K*(eight_sea$GDA[i]))*(eight_sea$ODA[i] - (eight_sea$EDA[i])))
    eight_sea[i,'EloAwayAtt'] <- eight_sea$EloAwayAtt[i] + ((K*(eight_sea$GAA[i]))*(eight_sea$OAA[i] - (eight_sea$EAA[i])))
    eight_sea[i, 'CDA'] <- ((K*(eight_sea$GDA[i]))*(eight_sea$ODA[i] - (eight_sea$EDA[i])))
    eight_sea[i, 'CAA'] <- ((K*(eight_sea$GAA[i]))*(eight_sea$OAA[i] - (eight_sea$EAA[i])))
    
  } else{
    
    eight_sea[eight_sea$Row.AID[i],'EloAwayDef'] <- eight_sea$EloAwayDef[i] + ((K*(eight_sea$GDA[i]))*(eight_sea$ODA[i] - (eight_sea$EDA[i])))
    eight_sea[eight_sea$Row.AID[i],'EloAwayAtt'] <- eight_sea$EloAwayAtt[i] + ((K*(eight_sea$GAA[i]))*(eight_sea$OAA[i] - (eight_sea$EAA[i]))) 
    eight_sea[i, 'CDA'] <- ((K*(eight_sea$GDA[i]))*(eight_sea$ODA[i] - (eight_sea$EDA[i])))
    eight_sea[i, 'CAA'] <- ((K*(eight_sea$GAA[i]))*(eight_sea$OAA[i] - (eight_sea$EAA[i])))
  }
  
}

