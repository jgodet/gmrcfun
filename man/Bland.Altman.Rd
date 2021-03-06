% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Bland.Altman.R
\name{Bland.Altman}
\alias{Bland.Altman}
\title{Analyse de Concordance de Bland et Altman}
\usage{
Bland.Altman(x, y, alpha = 0.05, rep.mes = FALSE, sujet, ...)
}
\arguments{
\item{x, y}{Deux vecteurs de valeurs quantitatives}

\item{alpha}{le seuil de significativite a definir}

\item{rep.mes}{si TRUE, les donnees sont des donnees repetees}

\item{sujet}{Le vecteur sujet a passer en cas de donnees repetees.}
}
\description{
Analyse de Concordance de Bland et Altman
}
\details{
manque une ref pour répétition des sujets
}
\examples{
d <- data.frame( SubjectID = LETTERS[1:20], xval = rnorm(20,10,1), yval = rnorm(20,10,1) + rnorm(20,.2,1))
Bland.Altman(d$xval, d$yval)

de <- data.frame( SubjectID = as.factor(rep(LETTERS[1:5],each=4)), xval = rnorm(20,10,1), yval = rnorm(20,10,1) + rnorm(20,.2,1))
Bland.Altman(x=de$xval, y = de$yval,alpha = .1,rep.mes = TRUE, sujet = de$SubjectID)

}
