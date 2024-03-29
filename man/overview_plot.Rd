% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plots.R
\name{overview_plot}
\alias{overview_plot}
\title{Create a plot showing an overview over the provided ranking.}
\usage{
overview_plot(ranked_data, highlighted_genes = NULL, sample_proportion = 0.05)
}
\arguments{
\item{ranked_data}{The ranking to visualize.}

\item{highlighted_genes}{Genes that will be marked.}

\item{sample_proportion}{Proportion of rows to use as the shown sample.}
}
\value{
A \code{plotly} figure.
}
\description{
Create a plot showing an overview over the provided ranking.
}
