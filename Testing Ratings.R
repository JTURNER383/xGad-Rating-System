## Build Model 

Elo <- rbind(one_sea, two_sea, three_sea, four_sea, five_sea, six_sea, last_sea, eight_sea)

Elo <- Elo %>% 
  mutate(hover_1.5 = case_when(
    Home.Goals > 1.5 ~ 1,
    Home.Goals < 1.5 ~ 0
  ))

Elo <- Elo %>% 
  mutate(aover_1.5 = case_when(
    Away.Goals > 1.5 ~ 1,
    Away.Goals < 1.5 ~ 0
  ))

Elo <- Elo %>% 
  mutate(over_2.5 = case_when(
    tot.goals > 2.5 ~ 1,
    tot.goals < 2.5 ~ 0
  ))

#################################################################################

## Assumption Testing

#################################################################################

library(MASS)

mod1 <- glm(Home.Goals ~ DAH + DAA, data = Elo, family = "poisson")
summary(mod1)

mod2 <- glm(Away.Goals ~ DAA + DAH, data = Elo, family = "poisson")
summary(mod2)

#################################################################################

## Predictive Modelling

################################################################################ 

## Home Goals
set.seed(123)
indexhg <- createDataPartition(Elo$hover_1.5, p = .70, list = FALSE)
trainhg <- Elo[indexhg, ]
testhg <- Elo[-indexhg, ]

mod3 <- glm(hover_1.5 ~ EloHomeAtt + EloAwayDef + EloAwayAtt + EloHomeDef, family = "binomial", data = trainhg)
summary(mod3)


pred_prob_hg <- predict(mod3, testhg, type = "response")
testhg$pred_prob_hg <- pred_prob_hg
testhg <- testhg %>% 
  mutate(Predict_cha_hg = case_when(
    pred_prob_hg < 0.45 ~ "Under",
    pred_prob_hg > 0.45 & pred_prob_hg < 0.55 ~ "Leave",
    pred_prob_hg > 0.55 ~ "Over"
  ))

homeg<- table(testhg$hover_1.5, testhg$Predict_cha_hg)
rownames(homeg) <- c("Obs. under", "Obs. over")
colnames(homeg) <- c("Pred. Leave","Pred. Over", "Pred. Under")
homeg

## Away Goals

indexag <- createDataPartition(Elo$aover_1.5, p = .70, list = FALSE)
trainag <- Elo[indexag, ]
testag <- Elo[-indexag, ]

mod4 <- glm(aover_1.5 ~ EloAwayAtt + EloHomeDef + EloHomeAtt + EloAwayDef, family = "binomial", data = trainag)
summary(mod4)

pred_prob_ag <- predict(mod4, testag, type = "response")
testag$pred_prob_ag <- pred_prob_ag
testag <- testag %>% 
  mutate(Predict_cha_ag = case_when(
    pred_prob_ag < 0.45 ~ "Under",
    pred_prob_ag > 0.45 & pred_prob_ag < 0.55 ~ "Leave",
    pred_prob_ag > 0.55 ~ "Over"
  ))

awayg<- table(testag$aover_1.5, testag$Predict_cha_ag)
rownames(awayg) <- c("Obs. under", "Obs. over")
colnames(awayg) <- c("Pred. Leave","Pred. Over", "Pred. Under")
awayg

roc(aover_1.5~testag$pred_prob_ag, data = testag, plot = TRUE, main = "Figure 10: Over 2.5 Total Goals ROC Curve", col= "blue")

### Binary Logistic Regression

mod5 <- glm(over_2.5 ~ EloHomeAtt + EloAwayDef + EloAwayAtt + EloHomeDef , data = Elo, family = "binomial")
summary(mod5)

library(pROC)

roc(over_2.5~mod5$fitted.values, data = Elo, plot = TRUE, main = "Figure 10: Over 2.5 Total Goals ROC Curve", col= "blue")

## Confusion Matrix

Elo$Predict <- mod5$fitted.values
Elo <- Elo %>% 
  mutate(Predict_cha = case_when(
    Predict < 0.45 ~ "Under",
    Predict > 0.45 & Predict < 0.55 ~ "Leave",
    Predict > 0.55 ~ "Over"
  ))

mytable <- table(Elo$over_2.5, Elo$Predict_cha)
rownames(mytable) <- c("Obs. under", "Obs. over")
colnames(mytable) <- c("Pred. Leave","Pred. Over", "Pred. Under")
mytable
