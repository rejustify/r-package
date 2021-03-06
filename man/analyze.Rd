% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/analyze.R
\name{analyze}
\alias{analyze}
\title{communicates with rejustify/analyze API endpoint}
\usage{
analyze(
  df,
  shape = "vertical",
  inits = 1,
  fast = TRUE,
  sep = ",",
  learn = getOption("rejustify.learn"),
  token = getOption("rejustify.token"),
  email = getOption("rejustify.email"),
  url = getOption("rejustify.mainUrl")
)
}
\arguments{
\item{df}{The data set to be analyzed. Must be matrix-convertible. If data frame,
the dimension names will be taken as the row/column names. If matrix, the row/column
names will be ignored, and the header will be set from matrix values in line with \code{inits}
and \code{sep} specification.}

\item{shape}{It informs the API whether the data set should be read by
columns (\code{vertical}) or by rows (\code{horizontal}). The default is \code{vertical}.}

\item{inits}{It informs the API how many initial rows (or columns in
horizontal data), correspond to the header description. The default
is \code{inits=1}.}

\item{fast}{Informs the API on how big a sample draw of original data should be.
The larger the sample, the more precise but overall slower the algorithm.
Under the \code{fast = TRUE} the API samples 5% of data points, under the
\code{fast = FALSE} option it is 25\%. Default is \code{fast=TRUE}.}

\item{sep}{The header can also be described by single field values,
separated by a given character separator, for instance 'GDP, Austria, 1999'.
The option informs the API which separator should be used to split the
initial header string into corresponding dimensions. The default is \code{sep=','}.}

\item{learn}{It should be set as \code{TRUE} if the user accepts rejustify to track her/his activity
to enhance the performance of the AI algorithms (it is not enabled by default). To change this option
for all API calls run \code{setCurl(learn=TRUE)}.}

\item{token}{API token. By default read from global variables.}

\item{email}{E-mail address for the account. By default read from global variables.}

\item{url}{API url. By default read from global variables.}
}
\value{
structure of the \code{df} data set
}
\description{
The function submits the data set to the analyze API endpoint and
returns the proposed structure of the data. At the current stage
data set must be rectangular, either vertical or horizontal.

API recognizes the multi-dimension and multi-line headers. The first \code{inits} rows/columns are
collapsed using \code{sep} character. Make sure that the separator doesn't appear in the header values.
It is possible to separate dimensions in single-line headers (see examples below).

The classification algorithms are applied to the values in the rows/columns if they are not empty, and
to the headers if the rows/columns are empty. For efficiency reasons only a sample of values in each column is analyzed.
To improve the classification accuracy, you can ask the API to draw a larger sample by setting \code{fast=FALSE}.
For empty columns the API returns the proposed resources that appear to fit well in the empty spaces given the header
information and overall structure of \code{df}.

The basic properties are characterized by classes. Currently, the API distinguishes between 6 classes: \code{general},
\code{geography}, \code{unit}, \code{time}, \code{sector}, \code{number}. They describe the basic characteristics of the
values, and they are further used to propose the best transformations and matching methods for data reconciliation. Classes
are further supported by features, which determine the characteristics in greater detail, such as class \code{geography}
may be further supported by feature \code{country}.

Cleaner contains the basic set of transformations applied to each value in a dimension to retrieve machine-readable
representation. For instance, values \code{y1999}, \code{y2000}, ..., clearly correspond to years, however, they will
be processed much faster if stripped from the initial \code{y} character, such as \code{^y}. Cleaner allows basic regular expressions.

Finally, format corresponds to the format of the values, and it is particularly useful for time-series operations. Format allows
the standard date formats (see \code{?as.Date}).

The classification algorithm can be substantially improved by allowing it to recall how
it was used in the past and how well it performed. Parameter \code{learn} controls this feature, however, by default it
is disabled. The information stored by rejustify is tailored to each user individually and it can substantially
increase the usability of the API. For instance, the proposed \code{provider} for empty row/column with header 'gross domestic product'
is \code{IMF}. Selecting another provider, for instance \code{AMECO}, will teach the algorithm that for this combination
of headers and rows/columns \code{AMECO} is the preferred \code{provider}, such that the next time API is called, there will be
higher chance of \code{AMECO} to be picked by default. To enable learning option in all API calls by default, run
\code{setCurl(learn=TRUE)}.

If \code{learn=TRUE}, the information stored by rejustify include (i) the information changed by the user with respect
to assigned \code{class}, \code{feature}, \code{cleaner} and \code{format}, (ii) resources determined by \code{provider},
\code{table} and headers of \code{df}, (iii) hand-picked matching values for \code{value-selection}. The information will
be stored only upon a change of values within groups (i-iii).
}
\examples{
#API setup
setCurl()

#register token/email
register(token = "YOUR_TOKEN", email = "YOUR_EMAIL")

#sample data set
df <- data.frame(year = c("2009", "2010", "2011"),
                 country = c("Poland", "Poland", "Poland"),
                 `gross domestic product` = NA,
                 check.names = FALSE, stringsAsFactors = FALSE)
analyze(df)

#data set with one-line multi-dimension header (semi-colon separated)
df <- data.frame(country = c("Poland", "Poland", "Poland"),
                 `gross domestic product;2009` = NA,
                 `gross domestic product;2010` = NA,
                  check.names = FALSE, stringsAsFactors = FALSE)
analyze(df, sep = ";")

#data set with multi-line header
df <- cbind(c(NA, "country", "Poland", "Poland", "Poland"),
            c("gross domestic product", "2009", NA, NA, NA),
            c("gross domestic product", "2010", NA, NA, NA))
analyze(df, inits = 2)

}
