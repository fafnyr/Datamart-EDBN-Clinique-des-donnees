% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/functions_EMR_to_cohort.R
\name{bio_report_to_table}
\alias{bio_report_to_table}
\title{Function bio_report_to_table()}
\usage{
bio_report_to_table(df_expand)
}
\arguments{
\item{df_expand}{document that will be modified in order to be read like a csv document (class : character)}
}
\value{
df_results : table containing data about samples collected from a patient (class : data.frame).
}
\description{
This function allows you to extract all tables containing biological data.
}
\examples{
res.lcm.exam_bio.result <- do.call("rbind", apply(res.lcm.exam_bio, 1, bio_report_to_table))
bio_report_to_table()
}
\keyword{bio}
\keyword{csv}
\keyword{datamart}
\keyword{table}
