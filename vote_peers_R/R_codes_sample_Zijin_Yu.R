# Zijin Yu

# Import data
setwd("//Users/zijinyu/Documents/R/vote_peers")
library(ggplot2)
library(stargazer)

vote <- read.table('vote_peers.dat', sep=' ')
names(vote) <- c('R0000100',
  'R0070300',
  'R0070400',
  'R0070500',
  'R0070600',
  'R0070700',
  'R0070800',
  'R0070900',
  'R0071000',
  'R0071100',
  'R0071300',
  'R0536300',
  'R0536402',
  'R1482600',
  'T3162000',
  'T3162200')

# Preprocessing Data ---------------------------------------------------------

# Handle missing values
vote[vote == -1] = NA  # Refused 
vote[vote == -2] = NA  # Dont know 
vote[vote == -3] = NA  # Invalid missing 
vote[vote == -4] = NA  # Valid missing 
vote[vote == -5] = NA  # Non-interview 

vote$vote08 <- ifelse(vote$T3162000 <= 3, 0,
                      + ifelse(vote$T3162000== 4, 1, NA))

# If there are values not categorized they will be shown as NA
vallabels = function(data) {
  data$R0000100[1.0 <= data$R0000100 & data$R0000100 <= 999.0] <- 1.0
  data$R0000100[1000.0 <= data$R0000100 & data$R0000100 <= 1999.0] <- 1000.0
  data$R0000100[2000.0 <= data$R0000100 & data$R0000100 <= 2999.0] <- 2000.0
  data$R0000100[3000.0 <= data$R0000100 & data$R0000100 <= 3999.0] <- 3000.0
  data$R0000100[4000.0 <= data$R0000100 & data$R0000100 <= 4999.0] <- 4000.0
  data$R0000100[5000.0 <= data$R0000100 & data$R0000100 <= 5999.0] <- 5000.0
  data$R0000100[6000.0 <= data$R0000100 & data$R0000100 <= 6999.0] <- 6000.0
  data$R0000100[7000.0 <= data$R0000100 & data$R0000100 <= 7999.0] <- 7000.0
  data$R0000100[8000.0 <= data$R0000100 & data$R0000100 <= 8999.0] <- 8000.0
  data$R0000100[9000.0 <= data$R0000100 & data$R0000100 <= 9999.0] <- 9000.0
  data$R0000100 <- factor(data$R0000100, 
    levels=c(0.0,1.0,1000.0,2000.0,3000.0,4000.0,5000.0,6000.0,7000.0,8000.0,9000.0), 
    labels=c("0",
      "1 TO 999",
      "1000 TO 1999",
      "2000 TO 2999",
      "3000 TO 3999",
      "4000 TO 4999",
      "5000 TO 5999",
      "6000 TO 6999",
      "7000 TO 7999",
      "8000 TO 8999",
      "9000 TO 9999"))
  data$R0070300 <- factor(data$R0070300, 
    levels=c(1.0,2.0,3.0,4.0,5.0), 
    labels=c("1.  Almost none (less than 10%)",
      "2.  About 25%",
      "3.  About half (50%)",
      "4.  About 75%",
      "5.  Almost all (more than 90%)"))
  data$R0070400 <- factor(data$R0070400, 
    levels=c(1.0,2.0,3.0,4.0,5.0), 
    labels=c("1.  Almost none (less than 10%)",
      "2.  About 25%",
      "3.  About half (50%)",
      "4.  About 75%",
      "5.  Almost all (more than 90%)"))
  data$R0070500 <- factor(data$R0070500, 
    levels=c(1.0,2.0,3.0,4.0,5.0), 
    labels=c("1.  Almost none (less than 10%)",
      "2.  About 25%",
      "3.  About half (50%)",
      "4.  About 75%",
      "5.  Almost all (more than 90%)"))
  data$R0070600 <- factor(data$R0070600, 
    levels=c(1.0,2.0,3.0,4.0,5.0), 
    labels=c("1.  Almost none (less than 10%)",
      "2.  About 25%",
      "3.  About half (50%)",
      "4.  About 75%",
      "5.  Almost all (more than 90%)"))
  data$R0070700 <- factor(data$R0070700, 
    levels=c(1.0,2.0,3.0,4.0,5.0), 
    labels=c("1.  Almost none (less than 10%)",
      "2.  About 25%",
      "3.  About half (50%)",
      "4.  About 75%",
      "5.  Almost all (more than 90%)"))
  data$R0070800 <- factor(data$R0070800, 
    levels=c(1.0,2.0,3.0,4.0,5.0), 
    labels=c("1.  Almost none (less than 10%)",
      "2.  About 25%",
      "3.  About half (50%)",
      "4.  About 75%",
      "5.  Almost all (more than 90%)"))
  data$R0070900 <- factor(data$R0070900, 
    levels=c(1.0,2.0,3.0,4.0,5.0), 
    labels=c("1.  Almost none (less than 10%)",
      "2.  About 25%",
      "3.  About half (50%)",
      "4.  About 75%",
      "5.  Almost all (more than 90%)"))
  data$R0071000 <- factor(data$R0071000, 
    levels=c(1.0,2.0,3.0,4.0,5.0), 
    labels=c("1.  Almost none (less than 10%)",
      "2.  About 25%",
      "3.  About half (50%)",
      "4.  About 75%",
      "5.  Almost all (more than 90%)"))
  data$R0071100 <- factor(data$R0071100, 
    levels=c(1.0,2.0,3.0,4.0,5.0), 
    labels=c("1.  Almost none (less than 10%)",
      "2.  About 25%",
      "3.  About half (50%)",
      "4.  About 75%",
      "5.  Almost all (more than 90%)"))
  data$R0071300 <- factor(data$R0071300, 
    levels=c(1.0,2.0,3.0,4.0,5.0), 
    labels=c("1.  Almost none (less than 10%)",
      "2.  About 25%",
      "3.  About half (50%)",
      "4.  About 75%",
      "5.  Almost all (more than 90%)"))
  data$R0536300 <- factor(data$R0536300, 
    levels=c(0.0,1.0,2.0), 
    labels=c("No Information",
      "Male",
      "Female"))
  data$R1482600 <- factor(data$R1482600, 
    levels=c(1.0,2.0,3.0,4.0), 
    labels=c("Black",
      "Hispanic",
      "Mixed Race (Non-Hispanic)",
      "Non-Black / Non-Hispanic"))
  data$T3162000 <- factor(data$T3162000, 
    levels=c(1.0,2.0,3.0,4.0,5.0), 
    labels=c("I DID NOT VOTE (IN THE ELECTION THIS NOVEMBER)",
      "I THOUGHT ABOUT VOTING THIS TIME, BUT DIDN'T",
      "I USUALLY VOTE, BUT DIDN'T THIS TIME",
      "I AM SURE I VOTED",
      "R NOT ELIGIBLE TO VOTE"))
  data$T3162200 <- factor(data$T3162200, 
    levels=c(0.0,1.0), 
    labels=c("NO",
      "YES"))
  data$vote08 <- factor(data$vote08,
    levels=c(0.0,1.0),
    labels=c("NO",
      "YES"))
  return(data)
}

varlabels <- c("PUBID - YTH ID CODE 1997",
  "% PEERS GO TO CHURCH REGULARLY 1997",
  "% PEERS SMOKE 1997",
  "% PEERS DRUNK 1+/MO 1997",
  "% PEERS SPORTS, CLUBS, SCH ACTS 1997",
  "% PEERS BELONG TO GANG 1997",
  "% PEERS PLAN TO GO TO COLL 1997",
  "% PEERS VOLUNTEER 1997",
  "% PEERS USE ILLEGAL DRUGS 1997",
  "% PEERS CUT CLASS/SCH 1997",
  "% PEERS HAD SEX 1997",
  "KEY!SEX (SYMBOL) 1997",
  "KEY!BDATE M/Y (SYMBOL) 1997",
  "KEY!RACE_ETHNICITY (SYMBOL) 1997",
  "DID R VOTE IN NOVEMBER, 2008? 2008",
  "R REGISTERED TO VOTE IN 2008 (PRO)? 2008",
  "DID R VOTE IN NOVEMBER, 2008 (BINARY)? 2008"
)

# Create a data file called "categories" with value labels. 
categories <- vallabels(vote)

# Produce summaries for the raw (uncategorized) data file
summary(vote)

# Produce summaries for the "categories" data file.
categories <- vallabels(vote)
summary(categories)

# Build new dataset
voteR <- na.omit(vote[,c(
  "R0000100",
  "R0536300",
  "R1482600",
  "R0070300",
  "R0070500",
  "R0070700",
  "R0070800",
  "R0070900",
  "T3162000",
  "vote08")])

summary(voteR)
table(voteR$R0536300)
table(voteR$R1482600)

table(voteR$R0070300)
table(voteR$R0070500)
table(voteR$R0070700)
table(voteR$R0070800)
table(voteR$R0070900)

sd(voteR$R0070300)
sd(voteR$R0070500)
sd(voteR$R0070700)
sd(voteR$R0070800)
sd(voteR$R0070900)
sd(voteR$vote08)

# Logistic Regression -----------------------------------------------------

# Estimate the logistic regression model
model <- glm(vote08 ~ R0070500 + R0070300 + R0070700 + R0070800 + R0070900, 
             family = binomial(link = 'logit'),  data = voteR)
summary(model)

# Assumptions Diagnostics -------------------------------------------------------

# Model fit: add independent variables stepwise
model1 <- glm(vote08 ~ R0070500, 
              family = binomial(link = 'logit'),  data = voteR)
AIC(model1)

model2 <- glm(vote08 ~ R0070500 + R0070300, 
              family = binomial(link = 'logit'),  data = voteR)
AIC(model2)

model3 <- glm(vote08 ~ R0070500 + R0070300 + R0070700, 
              family = binomial(link = 'logit'),  data = voteR)
AIC(model3)

model4 <- glm(vote08 ~ R0070500 + R0070300 + R0070700 + R0070800, 
              family = binomial(link = 'logit'),  data = voteR)
AIC(model4)

model <- glm(vote08 ~ R0070500 + R0070300 + R0070700 + R0070800 + R0070900, 
             family = binomial(link = 'logit'),  data = voteR)
AIC(model)

# Check and drop outliers
voteR$cd <- cooks.distance(model)
vote0 <- subset(voteR, cd < (4/4388))

model_otl <- glm(vote08 ~ R0070500 + R0070300 + R0070700 + R0070800 + R0070900, 
                data = vote0)
summary(model_otl)

# Check models before and after.
stargazer(model, model_otl,
          type = 'text', 
          align = T, 
          no.space = T)

modelnull <- glm(vote08 ~ 1, family = binomial(link = 'logit'),  
                 data = vote0)
summary(modelnull)

exp(cbind(OR = coef(model_otl), confint(model_otl)))

models <- list(intercept_only = modelnull, 
               "vote08 ~ R0070500 + R0070300 + R0070700 + R0070800 + R0070900" = model_otl)

# -2LL, AIC, and BIC values
# Use sapply to extract AIC, BIC, and -2LL
sapply(models, function(x) list(AIC = AIC(x), BIC = BIC(x), neg2LL = deviance(x)))





