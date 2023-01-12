########################################

## Data Vis

########################################

## Load Packages

#########################################

library(gridExtra)
library(grid)
library(ggrepel)

#########################################

## HA GOals - Figure 1

######################################

HAgoals <- Elo %>% 
  pivot_longer(cols = c('Home.Goals', 'Away.Goals'),
               values_to = 'Goals',
               names_to = 'HA')

HAgoals2 <- HAgoals %>% 
  group_by(HA, Goals) %>% 
  summarise(n = n())

HAgoals2 <- HAgoals2 %>% 
  mutate(HA2 = case_when(
    HA == "Home.Goals" ~ "Home Team",
    HA == "Away.Goals" ~ "Away Team"
  ))

HAgoals2$HA2 <- factor(HAgoals2$HA2, levels = c("Home Team", "Away Team")) 

HAgoals2 %>% 
  ggplot(aes(x=as.factor(Goals), y=n, fill = HA2))+
  geom_bar(position="dodge", stat="identity")+
  xlab("Goals")+ylab("Frequency")+
  ggtitle("Figure 1: Home and Away Team Goals in EPL (2014-2022)")+
  labs(fill="",
       caption = "Data Source: Understat")+theme_bw()+
  geom_text(aes(x = 9, y = 950,
                label = "Average Goals
Home = 1.5
Away = 1.2"),
            stat = "unique")
 

#############################################

## Total Goals - Figure 2

##############################################

tot.g <- table(Elo$tot.goals)
tot.g <- as.data.frame(tot.g)

tot.g %>% 
  ggplot(aes(x=Var1, y= Freq))+geom_bar(stat = "identity", fill = "darkgreen", colour = "black")+
  xlab("Total Goals")+ylab("Frequency")+theme_bw()+
  ggtitle("Figure 2: EPL Total Goal Distribution (2014-2022)")+
  labs(caption = "Data Source: Understat")+
  geom_text(aes(x = 9, y = 800,
                label = "Average Goals 2.7"),
stat = "unique")
  

##############################################

## xG Density - Figure 3

###############################################

## Make sure this plot is made pre changing xG for the LOOP!!!!!!

xG <- Elo %>% 
  select(Home.Expected.Goals, Away.Expected.Goals)

xG <- xG %>% 
  pivot_longer(cols = c('Home.Expected.Goals', 'Away.Expected.Goals'),
               names_to = 'HA',
               values_to = 'xG')

xG <- xG %>% 
  mutate(xG2 = case_when(
    HA == "Home.Expected.Goals" ~ "Home",
    HA == "Away.Expected.Goals" ~ "Away",
  ))


xG$xG2 <- factor(xG$xG2, levels = c("Home", "Away")) 

xG %>% 
  ggplot(aes(x=xG, fill = xG2))+geom_density(alpha = 0.3)+theme_bw()+
  ggtitle("Figure 3: EPL Expected Goals Distribution (2014-2022)")+ylab("Density")+
  labs(fill='',
       caption="Data Source: Understat")+
  geom_text(aes(x = 5, y = 0.54,
                label = "Average xG
Home = 1.50
Away = 1.20"),
            stat = "unique")

  

#############################################

## Observed Outcome - Figure 4

##############################################

Oatt <- Elo %>% ggplot(aes(x=Home.Goals,y=OAH))+geom_line(colour = "blue")+geom_point()+
  xlab("Goals")+ylab("Attacking Observed Outcome")+
  theme_bw()+scale_x_continuous(breaks = c(0,1,2,3,4,5,6,7,8,9))

Odef <- Elo %>% ggplot(aes(x=Home.Goals,y=ODA))+geom_line(colour = "darkred")+geom_point()+
  xlab("Goals")+ylab("Defending Observed Outcome")+
  theme_bw()+scale_x_continuous(breaks = c(0,1,2,3,4,5,6,7,8,9))

grid.arrange(Oatt, Odef, nrow = 2, top=textGrob("Figure 4: Observed Outcome Distribution"))

#############################################

## D & E - FIgure 5

###############################################

Elo %>% ggplot(aes(x=DAH, y=EAH))+geom_point()+ylim(c(0,1))+theme_bw()+
  xlab("D Term")+ylab("E Term")+
  ggtitle("Figure 5: D to E Term Distribution")


##########################################

## P Term to Goals - Figure 6

#############################################

Pterm <- Elo %>% 
  pivot_longer(cols = c('PAH', 'PAA', 'PDH', 'PDA'),
               names_to = 'class',
               values_to = 'P')

attP <- Pterm %>% 
  filter(class == "PAH" | class == "PAA")

attP$goals <- NA

for(i in 1:nrow(attP)){
 if(attP$class[i] == "PAH"){
   attP[i, 'goals'] <- attP$Home.Goals[i]
 } else{
   attP[i, 'goals'] <- attP$Away.Goals[i]
 }
  }

defP <- Pterm %>% 
  filter(class == "PDH" | class == "PDA")

defP$goals <- NA

for(i in 1:nrow(defP)){
  if(defP$class[i] == "PDH"){
    defP[i, 'goals'] <- defP$Away.Goals[i]
  } else{
    defP[i, 'goals'] <- defP$Home.Goals[i]
  }
}

attPplot <- attP %>% 
  ggplot(aes(x=as.factor(goals), y= P))+geom_boxplot()+
  xlab("Goals")+ggtitle("Attacking P Term")

defPplot <- defP %>% 
  ggplot(aes(x=as.factor(goals), y= P))+geom_boxplot()+
  xlab("Goals")+ggtitle("Defending P Term")+
  labs(caption = "n.b. X axis is either home
       or away goals")
 grid.arrange(attPplot, defPplot, nrow = 2, top = textGrob("Figure 6: Attacking and Defending P Terms"))
 
 #################################################
 
 ## xG and G term - Figure 7
 
 #################################################
 
 Elo <- Elo %>% 
   mutate(GDH_Neg = case_when(
     PDH < 0 ~ "Negative",
     PDH > 0 ~ "Positive"
   ))
 
 Elo <- Elo %>% 
   mutate(GAH_Neg = case_when(
     PAH < 0 ~ "Negative",
     PAH > 0 ~ "Positive"
   ))
 
 Elo <- Elo %>% 
   mutate(GAA_Neg = case_when(
     PAA < 0 ~ "Negative",
     PAA > 0 ~ "Positive"
   ))
 
 Elo <- Elo %>% 
   mutate(GDA_Neg = case_when(
     PDA < 0 ~ "Negative",
     PDA > 0 ~ "Positive"
   ))

 Gterm <- Elo %>% 
   pivot_longer(cols = c('GAH', 'GAA', 'GDH', 'GDA'),
                names_to = 'class',
                values_to = 'G')
 
 Gterm$xG <- NA
 Gterm$PN <- NA

 attG <- Gterm %>% 
   filter(class == "GAH" | class == "GAA")

 for(i in 1:nrow(attG)){
   if(attG$class[i] == "GAH"){
     attG[i, 'xG'] <- attG$Home.Expected.Goals[i]
   } else{
     attG[i, 'xG'] <- attG$Away.Expected.Goals[i]
   }
 }
 
 for(i in 1:nrow(attG)){
   if(attG$class[i] == "GAH"){
     attG[i, 'PN'] <- attG$GAH_Neg[i]
   } else{
     attG[i, 'PN'] <- attG$GAA_Neg[i]
   }
 }
 
 defG <- Gterm %>% 
   filter(class == "GDH" | class == "GDA")
 
 for(i in 1:nrow(defG)){
   if(defG$class[i] == "GDH"){
     defG[i, 'xG'] <- defG$Away.Expected.Goals[i]
   } else{
     defG[i, 'xG'] <- defG$Home.Expected.Goals[i]
   }
 }
 
 for(i in 1:nrow(defG)){
   if(defG$class[i] == "GDH"){
     defG[i, 'PN'] <- defG$GDH_Neg[i]
   } else{
     defG[i, 'PN'] <- defG$GDA_Neg[i]
   }
 }


attGplot <- attG %>% 
   ggplot(aes(x=xG, y= G, colour = PN))+geom_point()+
   xlab("Expected Goals")+ggtitle("Attacking G Term")+
  labs(colour = "P Term")
 
 defGplot <- defG %>% 
   ggplot(aes(x=xG, y= G, colour = PN))+geom_point()+
   xlab("Expected Goals")+ggtitle("Defending G Term")+
   labs(colour = "P Term",
        caption = "n.b. Home and Away Data merged")
 
 grid.arrange(attGplot, defGplot, nrow = 2, top = textGrob("Figure 7: Attacking and Defending G Terms"))
 
#######################################
 
 ## Last Season Home Ratings - Figure 9
 
rtest <- subset(eight_sea, is.na(eight_sea$Row.HID))
   
hatt <- rtest %>% select(Home.Team,EloHomeAtt)
hdef <- rtest %>% select(Home.Team,EloHomeDef)

home <- hatt %>% full_join(hdef, by = "Home.Team")

home <- home %>% 
  mutate(table = case_when(
    Home.Team == "Manchester City" ~ "Champion",
    Home.Team == "Liverpool" ~ "Top 4",
    Home.Team == "Chelsea" ~ "Top 4",
    Home.Team == "Tottenham" ~ "Top 4",
    Home.Team == "Arsenal" ~ "5-10",
    Home.Team == "Manchester United" ~ "5-10",
    Home.Team == "Leicester" ~ "5-10",
    Home.Team == "West Ham" ~ "5-10",
    Home.Team == "Brighton" ~ "5-10",
    Home.Team == "Wolverhampton Wanderers" ~ "11-15",
    Home.Team == "Newcastle United" ~ "11-15",
    Home.Team == "Crystal Palace" ~ "11-15",
    Home.Team == "Brentford" ~ "11-15",
    Home.Team == "Aston Villa" ~ "11-15",
    Home.Team == "Southampton" ~ "11-15",
    Home.Team == "Everton" ~ "16-17",
    Home.Team == "Leeds" ~ "16-17",
    Home.Team == "Burnley" ~ "Relegated",
    Home.Team == "Watford" ~ "Relegated",
    Home.Team == "Norwich" ~ "Relegated"
  ))

home$table <- factor(home$table, levels = c("Champion", "Top 4", "5-10",
                                            "11-15", "16-17", "Relegated"))

home_plot <- home %>% 
  ggplot(aes(y=EloHomeAtt, x=EloHomeDef, label = Home.Team, colour = table))+
  geom_vline(xintercept = 1500, linetype = "dotted")+
  geom_hline(yintercept = 1500, linetype = "dotted")+
  xlim(c(1350, 1700))+
  geom_point()+
  geom_label_repel(max.overlaps = Inf) +
  theme_classic()+xlab("xGad Home Defence")+
  ylab("xGad Home Attacking")+
  ggtitle("Home xGad Ratings")+
  scale_color_manual(breaks = c("Champion", "Top 4", "5-10", "11-15", "16-17", "Relegated"),
                     values=c("gold", "dark green", "darkolivegreen3", "orange", "darkorange3", "red"))+
  labs(colour = "")

## Last Season Away Ratings - Figure 9 

rtest <- subset(eight_sea, is.na(eight_sea$Row.AID))

aatt <- rtest %>% select(Away.Team,EloAwayAtt)
adef <- rtest %>% select(Away.Team,EloAwayDef)

away <- aatt %>% full_join(adef, by = "Away.Team")
away <- away %>% 
  mutate(table = case_when(
    Away.Team == "Manchester City" ~ "Champion",
    Away.Team == "Liverpool" ~ "Top 4",
    Away.Team == "Chelsea" ~ "Top 4",
    Away.Team == "Tottenham" ~ "Top 4",
    Away.Team == "Arsenal" ~ "5-10",
    Away.Team == "Manchester United" ~ "5-10",
    Away.Team == "Leicester" ~ "5-10",
    Away.Team == "West Ham" ~ "5-10",
    Away.Team == "Brighton" ~ "5-10",
    Away.Team == "Wolverhampton Wanderers" ~ "11-15",
    Away.Team == "Newcastle United" ~ "11-15",
    Away.Team == "Crystal Palace" ~ "11-15",
    Away.Team == "Brentford" ~ "11-15",
    Away.Team == "Aston Villa" ~ "11-15",
    Away.Team == "Southampton" ~ "11-15",
    Away.Team == "Everton" ~ "16-17",
    Away.Team == "Leeds" ~ "16-17",
    Away.Team == "Burnley" ~ "Relegated",
    Away.Team == "Watford" ~ "Relegated",
    Away.Team == "Norwich" ~ "Relegated"
  ))

away$table <- factor(away$table, levels = c("Champion", "Top 4", "5-10",
                                               "11-15", "16-17", "Relegated"))
  

away_plot <- away %>% 
  ggplot(aes(y=EloAwayAtt, x=EloAwayDef, label = Away.Team, colour = table))+
  geom_vline(xintercept = 1500, linetype = "dotted")+
  geom_hline(yintercept = 1500, linetype = "dotted")+
  geom_point()+
  xlim(c(1350, 1700))+
  geom_label_repel(max.overlaps = Inf) +
  theme_classic()+ylab("xGad Away Attack")+
  xlab("xGad Away Defence")+
  ggtitle("Away xGad Ratings")+
  scale_color_manual(breaks = c("Champion", "Top 4", "5-10", "11-15", "16-17", "Relegated"),
                     values=c("gold", "dark green", "darkolivegreen3", "orange", "darkorange3", "red"))+
  labs(colour = "")
  

grid.arrange(home_plot, away_plot, nrow = 2, top = textGrob("Figure 9: 2021/22 EPL Final xGad Ratings"))
######################################
 
 ## Rating Density - Figure 9
 
#######################################
 
 rating <- Elo %>% 
   select(EloHomeAtt, EloHomeDef, EloAwayAtt, EloAwayDef)
 
 rating <- rating %>% 
   pivot_longer(cols = c('EloHomeAtt', 'EloHomeDef','EloAwayAtt', 'EloAwayDef'),
                names_to = 'Elo',
                values_to = 'Rating')
 
 rating <- rating %>% 
   mutate(xGad = case_when(
     Elo == "EloHomeAtt" ~ "HomeAtt",
     Elo == "EloHomeDef" ~ "HomeDef",
     Elo == "EloAwayAtt" ~ "AwayAtt",
     Elo == "EloAwayDef" ~ "AwayDef"
   ))
 
 
 rating %>% 
   ggplot(aes(x=Rating, colour = xGad))+geom_density(size = 1.5)+
   ggtitle("Figure 11: Distribution of EPL xGad Ratings (2014-2022)")+theme_classic()+
   labs(colour = "")+ylab("Density")
 
 #######################################
 
 ## Strength of P Term - Figure 9
 
 #######################################
 
 strength <- Elo %>% 
   pivot_longer(cols = c('CAH', 'CAA', 'CDH', 'CDA'),
                names_to = 'C',
                values_to = 'change')
 
 strength %>% 
   ggplot(aes(x=change, colour=C))+geom_density(size = 1.5)+
   xlab("P")+ylab("Density")+ggtitle("Figure 9: Strength of the P Term")+
   labs(colour= "")
 
 #############################
 
 ## City
 
 ##########################
 
 mancity <- Elo %>% 
   filter(Home.Team == "Tottenham")

 mean(mancity$Home.Expected.Goals)
 mean(mancity$Home.Goals)
 
 