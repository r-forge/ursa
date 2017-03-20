\name{ursaProgressBar}
\alias{ursaProgressBar}
\alias{setUrsaProgressBar}
\title{
Progress bar
}
\description{
Informative progress bars with dispaying elapsed and remained time.
}
\usage{
ursaProgressBar(kind = c("tk", "txt"),
                title = basename(strsplit(commandArgs(FALSE)[4],"=")[[1]][2]),
                label = "", min = 0, max = 1, initial = min, width = NA, style = 1)

setUrsaProgressBar(pb, value, title = NULL, label = NULL)

}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{kind}{
Character. Type or progress bar. 
}
  \item{title}{
%%     ~~Describe \code{title} here~~
}
  \item{label}{
%%     ~~Describe \code{label} here~~
}
  \item{min}{
%%     ~~Describe \code{min} here~~
}
  \item{max}{
%%     ~~Describe \code{max} here~~
}
  \item{initial}{
%%     ~~Describe \code{initial} here~~
}
  \item{width}{
%%     ~~Describe \code{width} here~~
}
  \item{style}{
%%     ~~Describe \code{style} here~~
}
  \item{pb}{
%%     ~~Describe \code{pb} here~~
}
  \item{value}{
%%     ~~Describe \code{value} here~~
}
}
\details{
Wrapper to one of
\code{\link[utils:txtProgressBar]{txtProgressBar}},
%%~ \code{\link[utils:winProgressBar]{winProgressBar}},
\code{\link[tcltk:tkProgressBar]{tkProgressBar}}
. 

Method \code{close(con,...)} depends on class of progress bar.

Visualization of progress bar is updates each 0.5 seconds, it is efeective for multiple short-term iterations.
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
\code{ursaProgressBar} returns object of refererence progress bar.
}
%%~ \references{
%%~ %% ~put references to the literature/web site here ~
%%~ }
\author{
Nikita Platonov \email{platonov@sevin.ru}
}
\note{
Function name in style \emph{camelCase} for consistence with other progress bar functions in \R.
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link[utils:txtProgressBar]{txtProgressBar}}
%%~ \code{\link[utils:winProgressBar]{winProgressBar}}
\code{\link[tcltk:tkProgressBar]{tkProgressBar}}

}
\examples{
session_grid(NULL)
n1 <- 3
n2 <- 283
p <- 0.0051
#require(tcltk)
pb <- ursaProgressBar(min=0,max=n1,title="first")
for (i in seq(n1)) {
   pb2 <- ursaProgressBar(min=0,max=n2,title="second")
   for (i in seq(n2)) {
      setUrsaProgressBar(pb2)
      Sys.sleep(p)
   }
   close(pb2)
   setUrsaProgressBar(pb)
}
close(pb)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{misc}
