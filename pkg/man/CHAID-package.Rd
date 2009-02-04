\name{CHAID-package}
\alias{CHAID-package}
\alias{CHAID}
\docType{package}
\title{
CHi-squared Automatical Interaction Detection
}
\description{

  This package offers an implementation of CHAID, a type of decision tree
  technique for a nominal scaled dependent variable published in 1980 by Gordon
  V. Kass. CHAID stands for Chi-squared Automatic Interaction Detection and
  detects interactions between categorized variables of a data set, one of which
  is the dependent variable. The remaining variables may or may not be ordered.
  An algorithm for recursive partitioning is implemented, based on maximizing the
  significance of a chi-squared statistic for cross-tabulations between the
  dependent variable and the predictors at each partition. The data are
  partitioned into mutually exclusive, exhaustive subsets that best describe the
  dependent variable. Multiway splits are used by default.

}
\details{
\tabular{ll}{
Package: \tab CHAID\cr
License: \tab GPL-2\cr
}

The CHAID-algorithm is subdivided in five different steps. Step 1 calculates 
the cross-tabulates for each dependent variable in turn and processing step 2
and step 3. In the first instance they include the search of two variable values 
which show the least significant differing sub-table. This quest includes permutation
tests, if necessary. If the p-value exceeds the critical value, the categories 
are merged. This step 2 will be repeated. Secondly 
the most significant binary split should be found for each connected category with
more than two components (step 3). The split is carried into execution, if the significance
level is beyond a critical value. The algorithm returns to step 2. Step 4 computes
the significance of each optimally combinded independent variable. Therefore the 
implementation adjusts the p-value: I.e. using the Bonferroni multipliers according
to the characteristics of the predictor -- ordered or nominal case 
(q.v. Kass 1980: 122). The highest significant variable causes the division 
of the data set, if the critical value is passed. In step 5 the algorithm jumps 
to step 1, for each partition of the data set, which has not been investigated.
CHAID returns an object of class \code{constparty}, see package 
\code{\link[partykit]{party}}.

The package was implemented by students participating in an
advanced R programming course taught at the Department of Statistics
(University of Munich), winter term 2008/2009.

}
\author{

  The FoRt Student Project Team, with participants:

  Philip Bleninger, Catharina Brockhaus, Martina Feilke, Veronika
  Fensterer, Armin Graf, Stefanie Grunow, Elizabeth Heller, Matthias Hunger,
  Torsten Hothorn, Monika Jelizarow, Sara Kleyer, Julia Kopf, Sarah Maierhofer, Lisa Moest,
  Holger Reulen, Adrian Richter, Gunther Schauberger, Julia Schiele, Micha
  Schneider, Judith Schwitulla, Stephanie Thiemichen, Xuelin Wang, Anett Wins

  Maintainer: Torsten Hothorn <Torsten.Hothorn@R-project.org>
}
\references{
 G. V. Kass (1980). An Exploratory Technique for Investigating
 Large Quantities of Categorical Data. \emph{Applied Statistics}, \bold{29}(2), 119--127.
}     
\keyword{package}
