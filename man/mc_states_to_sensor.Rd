% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/states.R
\name{mc_states_to_sensor}
\alias{mc_states_to_sensor}
\title{Convert states to logical (TRUE/FALSE) sensor}
\usage{
mc_states_to_sensor(
  data,
  tag,
  to_sensor,
  source_sensor = NULL,
  inverse = FALSE
)
}
\arguments{
\item{data}{myClim object see \link{myClim-package}}

\item{tag}{The tag of states to be converted into a sensor.}

\item{to_sensor}{A vector of names for the output sensors.

\if{html}{\out{<div class="sourceCode">}}\preformatted{ If `to_sensor` is a single sensor name, the logical sensor is created 
 from the union of states across all sensors with the same tag. If `to_sensor` 
 contains multiple sensor names, the length of the vector must match the length 
 of `source_sensor`.
}\if{html}{\out{</div>}}}

\item{source_sensor}{A vector of sensors containing the states to be converted into a new sensor.
If NULL, states from all sensors are used. (default is NULL)}

\item{inverse}{A logical value. If TRUE, the sensor value is FALSE for state intervals (default is FALSE).}
}
\value{
Returns a myClim object in the same format as the input, with added sensors.
}
\description{
This function creates a logical (TRUE/FALSE) sensor from specified states.
}
\details{
The function allows you to create a TRUE/FALSE sensor based on a tag. By default,
it generates a new sensor by combining all tags specified in the \code{tag} parameter
from all available sensors at a particular logger or locality. If you specify a
\code{source_sensor}, the function converts only the tags from that specific sensor.
You can also create multiple new sensors from multiple tags by specifying more
values in \code{to_sensor} and providing exactly the same number of corresponding values
in \code{source_sensor}. For example, you can create one TRUE/FALSE sensor from states
on a temperature sensor and another from tags on a moisture sensor.

If you use parameter \code{inverse = TRUE} you get FALSE for each record where tag is assigned to and
FALSE for the records where tag is absent. By default you get TRUE for all the records
where tag is assigned.
}
\examples{
states <- data.frame(locality_id="A1E05", logger_name="Thermo_1",
                     sensor_name="Thermo_T", tag="error",
                     start=lubridate::ymd_hm("2020-10-28 9:00"),
                     end=lubridate::ymd_hm("2020-10-28 9:30"))
data <- mc_states_insert(mc_data_example_clean, states)
data <- mc_states_to_sensor(data, tag="error", to_sensor="error_sensor")
}
