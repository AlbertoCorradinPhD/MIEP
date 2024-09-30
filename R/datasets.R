#' coldata file
#'
#' Data frame that describes samples composing the experiment.
#'
#' \itemize{
#'   \item Name.
#'   \item condition. Condition of the sample, e.g. treatment or time points in case of time courses
#'   \item type.
#'   \item code.
#'   \item replicates. ID of the replicate
#'   \item cellLine. Cell line the sample belong to. In place of cell line, there can be the patient number
#' }
#'
#' @docType data
#' @keywords coldata
#' @name coldata
#' @format A data frame with rows corresponding to the number of samples and 6
#'   variables
NULL

#' Raw gene counts
#'
#' Data frame containing the reads' copy numbers. This data was created in
#' silico, starting from data downloaded from  GEO web site
#' (https://www.ncbi.nlm.nih.gov/geo/query/acc.cgi?acc=GSE198518). 3 in silico
#' replicates for every sample in the original data set. Original data were not
#' inserted in this in silico data set.
#'
#' \itemize{
#'   \item GeneID. Sequence's Ensemble ID
#'   \item Patient1_TREATED_rep1. Raw gene counts of patient 1, treated sample, replicates 1
#'   \item Patient1_TREATED_rep2. Raw gene counts of patient 1, treated sample, replicates 2
#'   \item Patient1_TREATED_rep3. Raw gene counts of patient 1, treated sample, replicates 3
#'   \item Patient1_CNTR_rep1. Raw gene counts of patient 1, treated sample, replicates 1
#'   \item Patient1_CNTR_rep2. Raw gene counts of patient 1, treated sample, replicates 2
#'   \item Patient1_CNTR_rep3. Raw gene counts of patient 1, treated sample, replicates 3
#'   \item Patient2_TREATED_rep1. Raw gene counts of patient 1, treated sample, replicates 1
#'   \item Patient2_TREATED_rep2. Raw gene counts of patient 1, treated sample, replicates 2
#'   \item Patient2_TREATED_rep3. Raw gene counts of patient 1, treated sample, replicates 3
#'   \item Patient2_CNTR_rep1. Raw gene counts of patient 1, treated sample, replicates 1
#'   \item Patient2_CNTR_rep2. Raw gene counts of patient 1, treated sample, replicates 2
#'   \item Patient2_CNTR_rep3. Raw gene counts of patient 1, treated sample, replicates 3
#'   \item Patient3_TREATED_rep1. Raw gene counts of patient 1, treated sample, replicates 1
#'   \item Patient3_TREATED_rep2. Raw gene counts of patient 1, treated sample, replicates 2
#'   \item Patient3_TREATED_rep3. Raw gene counts of patient 1, treated sample, replicates 3
#'   \item Patient3_CNTR_rep1. Raw gene counts of patient 1, treated sample, replicates 1
#'   \item Patient3_CNTR_rep2. Raw gene counts of patient 1, treated sample, replicates 2
#'   \item Patient3_CNTR_rep3. Raw gene counts of patient 1, treated sample, replicates 3
#' }
#'
#' @docType data
#' @keywords raw_gene_counts
#' @name raw_gene_counts
#' @format A data frame with rows corresponding to measured features and columns
#'   to samples
NULL
