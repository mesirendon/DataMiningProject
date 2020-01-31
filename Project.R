###################################################################################################
###################################################################################################
####                             US Newborns in 2016 Dataset                                   ####
###################################################################################################
###################################################################################################

# Dataset loading
rm(list = ls())
gc()
library(tcltk)
library(lattice)
setwd(tk_choose.dir())
births <- read.table("births.csv", header = TRUE, sep = ",")

###################################################################################################
# Data comprehension
## Dataset initial exploration
###################################################################################################

str(births)
summary(births)

###################################################################################################
### Frequency
###################################################################################################
births.dow <- table(births$DOB_WK)
births.dow
barchart(
  births.dow,
  xlab="Day of Week",
  ylab = "Number of births",
  col="red",
  main = "Frequency of births by day of the week",
  horizontal = FALSE,
  panel = function(...) { 
    args <- list(...)
    panel.text(args$x, args$y, args$y, pos=3, offset=1)
    panel.barchart(...)
  }
)

births.dmm <- table(births$DOB_MM)
births.dmm
barchart(
  births.dmm,
  xlab="Month of the year",
  ylab = "Number of births",
  col="blue",
  main = "Frequency of births by month",
  horizontal = FALSE,
  panel = function(...) { 
    args <- list(...)
    panel.text(args$x, args$y, args$y, pos=3, offset=1)
    panel.barchart(...)
  }
)

births.dmeth <- table(births$DMETH_REC)
births.dmeth
barchart(
  births.dmeth,
  xlab="Birth method",
  ylab = "Number of births",
  col="blue",
  main = "Frequency of births by method",
  horizontal = FALSE,
  panel = function(...) { 
    args <- list(...)
    panel.text(args$x, args$y, args$y, pos=3, offset=1)
    panel.barchart(...)
  }
)

births.bfacil <- table(births$BFACIL)
births.bfacil
barchart(
  births.bfacil,
  ylab="Birth place",
  xlab = "Number of births",
  col="blue",
  main = "Frequency of births by place",
)

births.mager <- table(births$MAGER)
births.mager
barchart(
  births.mager,
  xlab="Mother age",
  ylab = "Number of births",
  col="blue",
  main = "Frequency of births by mother's age",
  horizontal = FALSE
)

births.meduc <- table(births$MEDUC)
births.meduc
barchart(
  births.meduc,
  xlab = "Number of births",
  ylab = "Mother's education level",
  main = "Frequency of births by mother's education level"
)

births.precare5 <- table(births$PRECARE5)
births.precare5
barchart(
  births.precare5,
  xlab = "Number of births",
  ylab = "Prenatal care started at period",
  main = "Number of births by prenatal care started at certain period"
)

births.previs <- table(births$PREVIS)
births.previs
barchart(
  births.previs,
  horizontal = FALSE,
  ylab = "Number of births",
  xlab = "Number of prenatal care visits",
  main = "Number of births by number of prenatal care visits",
  col = "red"
)

births.cig0 <- table(births$CIG_0)
barchart(births.cig0, horizontal = FALSE)

births.cig1 <- table(births$CIG_1)
barchart(births.cig1, horizontal = FALSE)

births.cig2 <- table(births$CIG_2)
barchart(births.cig2, horizontal = FALSE)

births.cig3 <- table(births$CIG_3)
barchart(births.cig3, horizontal = FALSE)

backwardElimination <- function(x, sl) {
  numVars = length(x)
  for (i in c(1:numVars)){
    regressor = lm(formula = APGAR5 ~ ., data = x)
    maxVar = max(coef(summary(regressor))[c(2:numVars), "Pr(>|t|)"])
    if (maxVar > sl){
      j = which(coef(summary(regressor))[c(2:numVars), "Pr(>|t|)"] == maxVar)
      x = x[, -j]
    }
    print(numVars)
    numVars = numVars - 1
    file <- paste(i, ".txt")
    sink(file = file)
    print(summary(regressor))
    sink()
  }
  return(summary(regressor))
}

SL = 0.05
backwardElimination(births, SL)

library(caTools)
set.seed(123)
split = sample.split(births$APGAR5, SplitRatio = 0.8)
training_set = subset(births, split == TRUE)
test_set = subset(births, split == FALSE)

regressor <- lm(
  formula = APGAR5 ~ DBWT,
  data = training_set
)

library(ggplot2)
ggplot() +
  geom_point(aes(x = training_set$DBWT, y = training_set$APGAR5),
             colour = 'red') +
  geom_line(aes(x = training_set$DBWT, y = predict(regressor, newdata = training_set)),
            colour = 'blue') +
  ggtitle('APGAR5 vs DBWT (Training set)') +
  xlab('Weight') +
  ylab('APGAR Score')
