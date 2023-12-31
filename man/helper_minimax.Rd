% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/linComb.R
\name{helper_minimax}
\alias{helper_minimax}
\title{Helper function for minimax method.}
\usage{
helper_minimax(t, neg.set, pos.set, markers, status)
}
\arguments{
\item{t}{a \code{numeric} parameter that will be estimated in minimax
method for the combination score}

\item{neg.set}{a \code{numeric} data frame that contains the observation with
negative status}

\item{pos.set}{a \code{numeric} data frame that contains the observation with
positive status}

\item{markers}{a \code{numeric} data frame that contains the biomarkers}

\item{status}{a \code{factor} data frame that includes the actual disease
status of the patients}
}
\value{
A \code{numeric} Optimized value calculated with combination scores
using t
}
\description{
The \code{helper_minimax} function calculates the combination
coefficient and optimized value of given biomarkers for the minimax method.
}
\examples{
# call data
data(exampleData1)

# define the function parameters
markers <- cbind(exampleData1$ddimer, exampleData1$log_leukocyte)
status <- factor(exampleData1$group, levels = c("not_needed", "needed"))

neg.set <- markers[status == levels(status)[1], ]
pos.set <- markers[status == levels(status)[2], ]

t <- 0.5

stat <- helper_minimax(t,
  neg.set = neg.set, pos.set = pos.set,
  markers = markers, status
)

}
\author{
Serra Ilayda Yerlitas, Serra Bersan Gengec, Necla Kochan,
Gozde Erturk Zararsiz, Selcuk Korkmaz, Gokmen Zararsiz
}
