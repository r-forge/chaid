\name{chaid}
\alias{chaid}
\alias{chaid_control}
\title{ CHi-squared Automated Interaction Detection }
\description{
  Fits a classification tree by the CHAID algorithm.
}
\usage{
chaid(formula, data, subset, weights, na.action = na.omit, 
      control = chaid_control())
chaid_control(alpha2 = 0.05, alpha3 = -1, alpha4 = 0.05,
              minsplit = 20, minbucket = 7, minprob = 0.01,
              stump = FALSE, maxheight = -1)
}
\arguments{
  \item{formula}{an object of class \code{formula} 
                 (or one that can be coerced to
                 that class): a symbolic description of the model to be
                 fitted. Both response and all covariates 
                 are assumed to be categorical (either ordered or not).}
  \item{data}{an optional data frame containing the
          variables in the model. If not found in \code{data}, the
          variables are taken from \code{environment(formula)}, typically
          the environment from which \code{chaid} is called. }
  \item{subset}{an optional vector specifying a subset of observations to be
          used in the fitting process.}
  \item{weights}{an optional vector of weights to be used in the fitting
          process.  Should be \code{NULL} or a numeric vector.}
  \item{na.action}{a function which indicates what should happen when the data
          contain \code{NA}s. The default is \code{na.omit}. }
  \item{control}{hyper parameters of the algorithm as returned 
                 by \code{chaid_control}.}
  \item{alpha2}{Level of significance used for merging of predictor categories (step 2).}
  \item{alpha3}{If set to a positive value $< 1$, level of significance used for the the splitting
                of former merged categories of the predictor (step 3). Otherwise,
                step 3 is omitted (the default).}
  \item{alpha4}{Level of significance used for splitting of a node in the most significant
                predictor (step 5).}
  \item{minsplit}{Number of observations in splitted response at which no further split is desired.}
  \item{minbucket}{Minimum number of observations in terminal nodes.}
  \item{minprob}{Mininimum frequency of observations in terminal nodes.}
  \item{stump}{only root node splits are performed.}
  \item{maxheight}{Maximum height for the tree.}
}
\details{

The current implementation only accepts nominal or ordinal categorical predictors. When predictors
are continuous, they have to be transformed into ordinal predictors before using the following 
algorithm. 

Merging:
For each predictor variable X in turn, merge non-significant categories. Each final category of X will
result in one child node if X is used to split the node. The merging step also calculates the adjusted
p-value that is to be used in the splitting step.

1. If X has 1 category only, stop and set the adjusted p-value to be 1.

2. If X has 2 categories, go to step 8.

3. Else, find the allowable pair of categories of X (an allowable pair of categories for ordinal
predictor is two adjacent categories, and for nominal predictor is any two categories) that is least
significantly different (i.e., most similar). The most similar pair is the pair whose test statistic
gives the largest p-value with respect to the dependent variable Y. How to calculate p-value under
various situations will be described in later sections.

4. For the pair having the largest p-value, check if its p-value is larger than a user-specified
alpha-level \code{alpha2}. If it does, this pair is merged into a single compound category. Then a new
set of categories of X is formed. If it does not, then go to step 7.

5. (Optional) If the newly formed compound category consists of three or more original categories,
then find the best binary split within the compound category which p-value is the smallest.
Perform this binary split if its p-value is not larger than an alpha-level \code{alpha3}.

6. Go to step 2.

7. (Optional) Any category having too few observations (as compared with a user-specified
minimum segment size) is merged with the most similar other category as measured by the largest
of the p-values.

8. The adjusted p-value is computed for the merged categories by applying Bonferroni adjustments
that are to be discussed later.

Splitting:
The best split for each predictor is found in the merging step. The splitting step selects which
predictor to be used to best split the node. Selection is accomplished by comparing the adjusted
p-value associated with each predictor. The adjusted p-value is obtained in the merging step.
1. Select the predictor that has the smallest adjusted p-value (i.e., most significant).
2. If this adjusted p-value is less than or equal to a user-specified alpha-level \code{alpha4}, split the node
using this predictor. Else, do not split and the node is considered as a terminal node.

Stopping:
The stopping step checks if the tree growing process should be stopped according to the following
stopping rules.
a) If a node becomes pure; that is, all cases in a node have identical values of the dependent variable,
the node will not be split.
b) If all cases in a node have identical values for each predictor, the node will not be split.
c) If the current tree depth reaches the user specified maximum tree depth limit value, the tree
growing process will stop.
d) If the size of a node is less than the user-specified minimum node size value, the node will not be
split.
e) If the split of a node results in a child node whose node size is less than the user-specified
minimum child node size value, child nodes that have too few cases (as compared with this
minimum) will merge with the most similar child node as measured by the largest of the p-values.
However, if the resulting number of child nodes is 1, the node will not be split.
f) If the trees height is a positive value and equals the maxheight.
}
\value{
  An object of class \code{constparty}, see package 
  \code{\link[partykit]{party}}.
}
\references{ 
 G. V. Kass (1980). An Exploratory Technique for Investigating
 Large Quantities of Categorical Data. \emph{Applied Statistics}, 
 \bold{29}(2), 119--127.
}
\examples{

  library("CHAID")

  ### fit tree to subsample
  set.seed(290875)
  USvoteS <- USvote[sample(1:nrow(USvote), 1000),]

  ctrl <- chaid_control(minsplit = 200, minprob = 0.1)
  chaidUS <- chaid(vote3 ~ ., data = USvoteS, control = ctrl)

  print(chaidUS)
  plot(chaidUS)

}
\keyword{tree}
