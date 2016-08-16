#' Documentation for the dataset used inside parallelnewhybrid

#' SimPops_S1R1_NH
#' @format A NewHybrids format file.
#' To analyze this file using the function parallelnh_xx, save it with the
#' extension \code{.txt} to an empty folder on your hard drive, then provide
#' parallelnh_xx with the file path to the folder. To run in parallel,
#' after saving the file, copy it and give the copies unique names.
#' parallelnh_xx will attempt to analyze all files which do not contain
#' "individual.txt" within the file name, so it is essential that only
#' NewHybrids formatted files, and their associated individual files be
#' present in the folder provided to parallelnh_xx.
#' @source bla bla bla \url{http://www.blablabla.com/}
#' @keywords datasets
"SimPops_S1R1_NH"

#' SimPops_S1R1_NH_individuals
#' @format The individual file associated with SimPops_S1R1_NH. A single
#' copy of this file should be saved to the same folder in which SimPops_S1R1_NH
#' is saved. The filename must end in "individuals.txt".
#' @source bla bla bla \url{http://www.blablabla.com/}
#' @keywords datasets
"SimPops_S1R1_NH_individuals"
