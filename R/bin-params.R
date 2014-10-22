bin_params <- function(x_range, width = NULL, center = NULL, boundary = NULL,
                       closed = c("right", "left")) {
  UseMethod("bin_params")
}

#' @export
bin_params.numeric <- function(x_range, width = NULL, center = NULL,
                               boundary = NULL, closed = c("right", "left")) {
  closed <- match.arg(closed)

  if (length(x_range) == 0) {
    return(list(width = width, origin = NULL, closed = closed))
  }

  stopifnot(length(x_range) == 2)

  # Find a nice-looking value for width
  if (is.null(width)) {
    bounds <- pretty(x_range, 30)
    width <- bounds[2] - bounds[1]
    notify_guess(width, paste0("range / ", length(bounds) - 1))
  }

  if (!is.null(boundary) && !is.null(center)) {
    stop("Only one of 'boundary' and 'center' may be specified.")
  }
  if (is.null(boundary)) {
    if (is.null(center)) {
      # If neither boundary nor center given, use tile layer's algorithm.
      # This puts min and max of data in outer half of their bins.
      boundary <- width / 2
    } else {
      # If center given but not boundary, compute boundary.
      boundary <- center - width / 2
    }
  }

  origin <- find_origin(x_range, width, boundary)

  list(width = width, origin = origin, closed = closed)
}

#' @export
bin_params.Date <- function(x_range, width = NULL, center = NULL,
                            boundary = NULL, closed = c("right", "left")) {

  bin_params(
    as.numeric(x_range),
    as_numeric(width),
    as_numeric(center),
    as_numeric(boundary),
    closed
  )
}

#' @export
bin_params.POSIXct <- function(x_range, width = NULL, center = NULL,
                               boundary = NULL, closed = c("right", "left")) {
  if (length(x_range) == 0) {
    return(list(width = width, origin = NULL, closed = closed))
  }

  if (is.null(width)) {
    bounds <- pretty(x_range, 30)
    width <- bounds[2] - bounds[1]
    notify_guess(width, paste0("range / ", length(bounds)-1))
  }

  # Period object from lubridate package - need lubridate::as.difftime to find
  # the correct generic, instead of base::as.difftime.
  if (is(width, "Period")) {
    width <- as.numeric(lubridate::as.difftime(width, units = "secs"))
  } else {
    width <- as.numeric(width, units = "secs")
  }

  bin_params(
    as.numeric(x_range),
    width,
    as_numeric(center),
    as_numeric(boundary),
    closed
  )
}

# Find the left side of left-most bin
find_origin <- function(x_range, width, boundary) {
  shift <- floor((x_range[1] - boundary) / width)
  boundary + shift * width
}

