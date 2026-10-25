#' Plot mean arrival delays by destination airport
#'
#' Summarizes arrival delays in the \pkg{nycflights13} flights data by
#' destination airport, joins the summaries to airport coordinates, and
#' returns a \pkg{ggplot2} map.
#'
#' @return A \code{ggplot} object.
#' @examples
#' visualize_airport_delays()
#' @importFrom ggplot2 .data
#' @export
visualize_airport_delays <- function() {
  flights <- nycflights13::flights
  airports <- nycflights13::airports

  mean_delays <- dplyr::summarise(
    dplyr::group_by(flights, .data$dest),
    mean_arr_delay = mean(.data$arr_delay, na.rm = TRUE),
    n_flights = dplyr::n(),
    .groups = "drop"
  )
  airport_delays <- dplyr::inner_join(
    mean_delays,
    airports,
    by = c("dest" = "faa")
  )
  airport_delays <- dplyr::filter(
    airport_delays,
    is.finite(.data$lon),
    is.finite(.data$lat),
    is.finite(.data$mean_arr_delay)
  )

  ggplot2::ggplot(
    airport_delays,
    ggplot2::aes(
      x = .data$lon,
      y = .data$lat,
      colour = .data$mean_arr_delay,
      size = .data$n_flights
    )
  ) +
    ggplot2::geom_point(alpha = 0.8) +
    ggplot2::scale_colour_gradient2(
      name = "Mean arrival delay\n(minutes)",
      low = "#2166AC",
      mid = "white",
      high = "#B2182B",
      midpoint = 0
    ) +
    ggplot2::scale_size_continuous(
      name = "Flights",
      range = c(1, 6),
      trans = "sqrt"
    ) +
    ggplot2::coord_quickmap() +
    ggplot2::labs(
      title = "Mean arrival delay by destination airport",
      x = "Longitude",
      y = "Latitude"
    ) +
    ggplot2::theme_minimal()
}
