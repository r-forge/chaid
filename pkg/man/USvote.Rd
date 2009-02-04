\name{USvote}
\alias{USvote}
\docType{data}
\title{ U.S. 2000 Election Data}
\description{
  Data from a post-election survey following the 
  year 2000 U.S. presidential elections.
}
\usage{data("USvote")}
\format{
  A data frame with 2000 observations on the following 6 variables.
  \describe{
    \item{\code{vote3}}{candidate voted for \code{Gore} or \code{Bush}}
    \item{\code{gender}}{gender, a factor with levels \code{male} and \code{female}}
    \item{\code{ager}}{age group, an ordered factor with levels \code{18-24} < \code{25-34} < \code{35-44} < \code{45-54} < \code{55-64} < \code{65+}}
    \item{\code{empstat}}{status of employment, a factor with levels \code{yes}, \code{no} or \code{retired}}
    \item{\code{educr}}{status of education, an ordered factor with levels \code{<HS} < \code{HS} < \code{>HS} < \code{College} < \code{Post Coll}}
    \item{\code{marstat}}{status of living situation, a factor with levels \code{married}, \code{widowed}, \code{divorced} or \code{never married}}
  }
}
\details{

  The dataset is based on data from a post-election survey on persons 
  who voted for either Bush or Gore in the 2000 U.S. election. The specific
  variables are related to the publication of Magidson and Vermunt (2005).

  Further information (and datasets) about the 2000 U.S. election and other 
  National Election Studies is available on the American National Election 
  Studies Web site (\url{http://www.electionstudies.org/}).

}
\source{
  \url{http://www.statisticalinnovations.com/products/latentgold_datasets.html#USselection2000reg}
}
\references{

  Jay Magidson and Jeroen K. Vermunt (2005). An Extension of the CHAID Tree-based 
  Segmentation Algorithm to Multiple Dependent Variables. In: 
  C. Weihs & W. Gaul (Editors), Classification: The Ubiquitous Challenge, 
  pages 176--183. Heidelberg: Springer
  \url{http://www.statisticalinnovations.com/products/8pagearticle.pdf}

}
\examples{

  library("CHAID")

  ### fit tree to subsample
  set.seed(290875)
  USvoteS <- USvote[sample(1:nrow(USvote), 2000),]
  chaidUS <- chaid(vote3 ~ gender + ager + empstat + educr + marstat, 
                   data = USvoteS)

  print(chaidUS)
  plot(chaidUS)

}
\keyword{datasets}
