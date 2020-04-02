\name{face}
\alias{faceA}
\alias{faceB}
\alias{faceC}
\alias{faceD}
\title{Draw a face}
\description{Draws a face, consisting of a rectangle
for the border, circles for eyes, and a line for the mouth.}
\usage{
faceA(x, y, width, height)
faceB(x, y, width, height)
faceC(x, y, width, height)
faceD(x, y, width, height)
}
\arguments{
  \item{x, y, width, height}{
    Numeric values or unit objects specifying the location
    and size of the face.}
}
\details{
The functions \code{faceA} and \code{faceB} are
graphics functions to be used for their side effect of
producing graphical output.
The functions \code{faceC} and \code{faceD}
return a grob representing a face (and produce no output).
}
\keyword{dplot}
