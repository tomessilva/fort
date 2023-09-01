#
# fort package - .onLoad() function ----
#

.onLoad <- function(libname, pkgname) {
  # check if fort.avoid_unicode is set
  current_avoid_unicode <- getOption("fort.avoid_unicode")
  if (is.null(current_avoid_unicode)) {
    # option has *not* been set by user
    if (interactive()) {
      # if we are in an interactive session, assume we want UTF-8
      options(fort.avoid_unicode=FALSE)
    } else {
      # otherwise, keep it simple and assume 7-bit ASCII
      options(fort.avoid_unicode=TRUE)
    }
  }
}
