% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plots.R
\name{scores_plot}
\alias{scores_plot}
\title{Create plot showing the distribution of scores using \code{plotly}.}
\usage{
scores_plot(ranked_data, highlighted_genes = NULL, ranks = 1000)
}
\arguments{
\item{ranked_data}{Data on genes with precomputed ranks.}

\item{highlighted_genes}{Genes that will be marked.}

\item{ranks}{How may ranks the x-axis should include. If this parameter is
\code{NULL}, all ranks will be shown.}
}
\value{
A \code{plotly} figure for rendering.
}
\description{
Create plot showing the distribution of scores using \code{plotly}.
}
