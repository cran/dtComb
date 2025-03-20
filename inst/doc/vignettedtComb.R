## ----echo=FALSE---------------------------------------------------------------
library(knitr)
opts_chunk$set(tidy = FALSE, dev = "pdf", message = FALSE, fig.align = "center", cache = FALSE)

## -----------------------------------------------------------------------------
library(dtComb)

## ----eval = TRUE, echo=TRUE---------------------------------------------------
data(laparoscopy)
head(laparoscopy)

## -----------------------------------------------------------------------------
# # train set from the laparoscopy
set.seed(2128)
inTrain <- caret::createDataPartition(laparoscopy$group, p = 3 / 4, list = FALSE)
trainData <- laparoscopy[inTrain, ]
head(trainData)

## -----------------------------------------------------------------------------
# # test set from the laparoscopy
set.seed(2128)
testData <- laparoscopy[-inTrain, -1]

## -----------------------------------------------------------------------------
markers <- trainData[, -1]
status <- factor(trainData$group, levels = c("not_needed", "needed"))

## ----fig.height=4.8, fig.width=5----------------------------------------------
set.seed(2128)

# linComb Function
fit.lin <- linComb(
  markers = markers,
  status = status,
  event = "needed",
  method = "scoring",
  resample = "cv",
  standardize = "range",
  ndigits = 2, direction = "auto",
  cutoff.method = "Youden"
)

## ----fig.height=4.8, fig.width=5----------------------------------------------
# nonlinComb Function
set.seed(2128)

fit.nonlin <- nonlinComb(
  markers = markers,
  status = status,
  event = "needed",
  method = "lassoreg",
  include.interact = "TRUE",
  resample = "boot",
  direction = "auto",
  cutoff.method = "Youden"
)

## ----fig.height=4.8, fig.width=5----------------------------------------------
# mlComb Function
set.seed(2128)

fit.ml <- mlComb(
  markers = markers,
  status = status,
  event = "needed",
  method = "knn",
  resample = "repeatedcv", nfolds = 10, nrepeats = 5,
  preProcess = c("center", "scale"),
  direction = "<", cutoff.method = "Youden"
)

## ----fig.height=4.8, fig.width=5----------------------------------------------
# mathComb Function
fit.math <- mathComb(
  markers = markers,
  status = status,
  event = "needed",
  method = "distance",
  distance = "euclidean",
  direction = "<",
  cutoff.method = "Youden"
)

## -----------------------------------------------------------------------------
predict(fit.nonlin, testData)

## -----------------------------------------------------------------------------
sessionInfo()

