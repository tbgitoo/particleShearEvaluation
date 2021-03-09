\name{linear_regression_p_bootstrap}
\alias{linear_regression_p_bootstrap}

\title{
linear_regression_p_bootstrap
}
\description{
Uses pre-generated bootstrapping matrices for evaluating relations between variables by linear regression
}
\usage{
linear_regression_p_bootstrap(x,y,n_agg=5)
}

\arguments{
  \item{x}{
Independent variable for the linear regression; this should be a vector.
}
  \item{n}{
Bootstrapping data. This is a matrix with as many rows as there are elements in \code{x}, and a possible very large number of columns where the number of columns should be multiple of \code{n_agg}, otherwise the last columns are ignored such as to achieve a number of columns which is a multiple of \code{n_agg}. 
}
\item{n_agg}{Number of columns in \code{y} that should be used together to generate a dataset to be used for the linear model (i.e. passed to \link{lm}).}
}
\details{
This function assembles datasets for linear regression, calls \link{lm} for carrying out the linear regression, and returns statistical data averaged over the different runs. This is typically useful to analyze bootstrapping data, where many data sets are drawn from an original, larger dataset. \cr\cr
Technically, the function aims at regression the data in \code{y} against the independent variable \code{x}. For this, datasets are progressively assembled from \code{x} and \code{n_agg} columns of \code{y} at a time. Internally, \code{x} is repeated \code{n_agg} times by concatenation, and associated with the \code{n_agg} columns of \code{y} assembled into a single vector by concatenation. On this dataset, linear regression (aka \link{lm}) is run.\cr\cr 
From each run of linear regression, an F statistics and degrees of freedom of the numerator (DF1) and denominator (DF2) as well as an adjusted r squared value is obtained. The function averages these over the number of linear regression runs; the average P-value is calculated using \link{pf} from these average statistic values and return as the main value.
}

\value{Single numerical P-value. In addition, attributes accessible via \link{attr} are attached:\cr
"F" for the average F-statistics\cr
"DF1" for the number of degrees of freedom in the numerator\cr
"DF2" for the number of degrees of freedom in the denominator\cr
"adj.r.squared" for the adjusted R-squared value as calculated by \link{summary.lm}\cr
"p_shapiro" averaged P-value for normality testing. The averaging here is particular: The P-value returned by \link{shapiro.test} is first converted to a Z value by \link{qnorm}, and the Z values averaged over the linear regression run. The average Z-value is converted back to an average P-value via \link{pnorm}}

\author{
Thomas Braschler
}

\examples{
# Example 1: usage
x=c(0,1,2,3)

# With a known effect
y=matrix(nrow=length(x),ncol=500,data=x)
y=y+matrix(nrow=dim(y)[1], ncol=dim(y)[2], data=rnorm(dim(y)[1]*dim(y)[2]))
linear_regression_p_bootstrap(x,y,n_agg=5)

# Random effect only
y=matrix(nrow=dim(y)[1], ncol=dim(y)[2], data=rnorm(dim(y)[1]*dim(y)[2]))
linear_regression_p_bootstrap(x,y,n_agg=5)

}

\keyword{ misc }

