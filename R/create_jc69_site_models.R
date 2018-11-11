#' Creates a JC69_site_model for each ID
#' @inheritParams default_params_doc
#' @return a list of site_models
#' @seealso The alignment IDs can be deduced from the FASTA filenames,
#'   using \code{\link{get_alignment_ids}}
#' @author Richel J.C. Bilderbeek
#' @examples
#'   fasta_filenames <- get_beautier_paths(
#'     c("anthus_aco.fas", "anthus_nd2.fas")
#'   )
#'   site_models <- create_jc69_site_models(c("anthus_aco", "anthus_nd2"))
#'   create_beast2_input_file(
#'     fasta_filenames,
#'     "create_jc69_site_models.xml",
#'     site_models = site_models
#'   )
#'   testit::assert(file.exists("create_jc69_site_models.xml"))
#' @export
create_jc69_site_models <- function(ids) {
  n <- length(ids)
  ms <- list()
  for (i in seq(1, n)) {
    ms[[i]] <- beautier::create_jc69_site_model(id = ids[i])
  }
  ms
}
