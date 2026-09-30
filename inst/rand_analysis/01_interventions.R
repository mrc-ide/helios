# UV-C and glycol vapor intervention definitions for the RAND scenario
# comparison, using RAND's supplied eACH ranges: glycol 11-18, UV-C 35-39.
# Midpoints are used as fixed constants for the main run matrix; see the
# uniform-sampling option noted below for a future sensitivity pass.

devtools::load_all()

uvc <- make_intervention(
  name = "uvc",
  delta_function = function() 37,
  coverage = 1
)

glycol <- make_intervention(
  name = "glycol",
  delta_function = function() 14.5,
  coverage = 1
)

# Uniform-per-run alternative (draws one eACH per simulation run from RAND's
# stated range, rather than fixing the midpoint) -- not used by default, kept
# here for the sensitivity pass:
#
# glycol_uniform <- make_intervention(
#   name = "glycol",
#   delta_function = function() stats::runif(1, min = 11, max = 18),
#   coverage = 1
# )
# uvc_uniform <- make_intervention(
#   name = "uvc",
#   delta_function = function() stats::runif(1, min = 35, max = 39),
#   coverage = 1
# )
