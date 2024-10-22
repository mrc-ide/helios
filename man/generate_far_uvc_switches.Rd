% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/interventions.R
\name{generate_far_uvc_switches}
\alias{generate_far_uvc_switches}
\title{Update model parameters with far UVC switches}
\usage{
generate_far_uvc_switches(parameters_list, variables_list)
}
\arguments{
\item{parameters_list}{A list of model parameters as generated using \code{get_parameters()}}

\item{variables_list}{A list of model variables as generated using \code{create_variables()}}
}
\description{
\code{generate_far_uvc_switches()} determines which locations will deploy far UVC given the setting type is
switched on, the setting-specific coverages, and the setting-specific coverage types. The function returns,
for each the workplace, school, leisure, and household settings, a vector of length equal to the
the number of locations within the setting type (e.g. number of schools within the school setting type)
populated with 1's and 0's, where a 1 represents the presence of far UVC and a 0 the absence of far
UVC. The function returns an updated parameter list with these vectors appended for each setting type
for which far UVC has been parameterised using the \code{set_uvc()} function.
}
\seealso{
Other intervention: 
\code{\link{generate_joint_far_uvc_switches}()},
\code{\link{generate_setting_far_uvc_switches}()},
\code{\link{set_uvc}()}
}
\concept{intervention}
