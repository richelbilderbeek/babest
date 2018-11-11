#' Creates n strict clock_models
#' @param ids the alignment IDs
#' @return a list of strict_clock objects
#' @seealso The alignment IDs can be deduced from the FASTA filenames,
#'   using \code{\link{get_alignment_ids}}
#' @examples
#'   fasta_filenames <- get_beautier_paths(
#'     c("anthus_aco.fas", "anthus_nd2.fas")
#'   )
#'   clock_models <- create_strict_clock_models(
#'     ids = get_alignment_ids(fasta_filenames)
#'   )
#'
#'   create_beast2_input_file(
#'     fasta_filenames,
#'     "create_strict_clock_models.xml",
#'     clock_models = clock_models
#'   )
#'   testit::assert(file.exists("create_strict_clock_models.xml"))
#' @export
create_strict_clock_models <- function(ids) {
  n <- length(ids)
  ms <- list()
  for (i in seq(1, n)) {
    ms[[i]] <- beautier::create_strict_clock_model(
      id = ids[i],
      clock_rate_param = create_clock_rate_param(
        id = ids[i]
      )
    )
  }
  ms
}
