% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/convert.r
\name{convert}
\alias{convert}
\alias{convert.default}
\alias{convert.list}
\alias{convert.data.frame}
\alias{convert.matrix}
\title{Convert object or its items into a requested type}
\usage{
convert(x, type)

\method{convert}{default}(x, type)

\method{convert}{list}(x, type)

\method{convert}{data.frame}(x, type)

\method{convert}{matrix}(x, type)
}
\arguments{
\item{x}{any R object}

\item{type}{R's (internal) storage type of object, such as \code{"logical"}, \code{"integer"}, \code{"double"},
\code{"complex"}, \code{"character"} or \code{"raw"}.}
}
\description{
Convert a type of an object, such as \code{vector} or \code{matrix} or items contained in an object for
\code{list} and \code{data.frame} into a requested type.
}
\details{
The \code{convert} function is a generalized \code{methods::as()} and its variants \code{as.character},
\verb{as.numeric, etc., to work for all basic R types and not just vectors. The basic types implied here are }vector\verb{, }matrix\verb{, }data.frame\verb{. Unlike }methods::as`, the object itself is not coerced
into a particular class, e.g, matrix won't become a list. Instead a numerical matrix might be
converted into a character matrix.
}
\examples{
# For vectors, convert is identical to as
foo = 1:3
identical(convert(foo, "character"), as(foo, "character"))

# For matrices, convert is similar to mode, but a pure function
foo = matrix(1:4, 2, 2)
bar = foo
mode(bar) = "character"
identical(convert(foo, "character"), bar)

# For data.frames and lists, its convert is equivalent of converting all their items.
foo = data.frame("a"=1:2, "b"=c(0.1, 0.2))
bar = convert(foo, "character")
identical(foo$a, c("1","2"))
identical(foo$b, c("0.1", "0.2"))

}
\seealso{
\code{\link[base:mode]{base::mode()}}, \code{\link[base:typeof]{base::typeof()}}, \code{\link[methods:is]{methods::is()}}, \code{\link[methods:as]{methods::as()}}
}
