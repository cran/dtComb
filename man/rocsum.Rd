% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/rocsum.R
\name{rocsum}
\alias{rocsum}
\title{Generate ROC curves and related  statistics for the given markers and
Combination score.}
\usage{
rocsum(
  markers = NULL,
  comb.score = NULL,
  status = NULL,
  event = NULL,
  direction = c("auto", "<", ">"),
  conf.level = 0.95,
  cutoff.method = c("CB", "MCT", "MinValueSp", "MinValueSe", "ValueSp", "ValueSe",
    "MinValueSpSe", "MaxSp", "MaxSe", "MaxSpSe", "MaxProdSpSe", "ROC01", "SpEqualSe",
    "Youden", "MaxEfficiency", "Minimax", "MaxDOR", "MaxKappa", "MinValueNPV",
    "MinValuePPV", "ValueNPV", "ValuePPV", "MinValueNPVPPV", "PROC01", "NPVEqualPPV",
    "MaxNPVPPV", "MaxSumNPVPPV", "MaxProdNPVPPV", "ValueDLR.Negative",
    "ValueDLR.Positive", "MinPvalue", "ObservedPrev", "MeanPrev", "PrevalenceMatching"),
  show.plot = show.plot
)
}
\arguments{
\item{markers}{a \code{numeric} data frame that includes two diagnostic tests
results}

\item{comb.score}{a matrix of \code{numeric} combination scores calculated
according to the given method}

\item{status}{a \code{factor} vector that includes the actual disease
status of the patients}

\item{event}{a \code{character} string that indicates the event in the status
to be considered as positive event}

\item{direction}{a \code{character} string determines in which direction the
comparison will be made.  “>”: if the predictor values for the control group
are higher than the values of the case group (controls > cases).
“<”: if the predictor values for the control group are lower or equal than
the values of the case group (controls < cases).}

\item{conf.level}{a \code{numeric} values determines the confidens interval
for the ROC curve(0.95, default).}

\item{cutoff.method}{a \code{character} string determines the cutoff method
for the ROC curve.}

\item{show.plot}{a \code{logical}. If TRUE, a ROC curve is plotted.
Default is FALSE.}
}
\value{
A list of \code{numeric} ROC Curves, AUC statistics and Confusion
matrices.
}
\description{
The \code{rocsum} function returns the ROC curves with
coordinates, Area Under the Curves of markers and combination score, Area Under
the Curve comparison of markers and combination score, Confusion matrices for both
markers and combination score with the cutoff values derived from the ROC Curves.
}
\author{
Serra Ilayda Yerlitas, Serra Bersan Gengec, Necla Kochan,
Gozde Erturk Zararsiz, Selcuk Korkmaz, Gokmen Zararsiz
}
