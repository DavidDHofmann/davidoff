################################################################################
#### Function to Convert Meters into Degrees
################################################################################
#' Convert meters to degrees
#'
#' Function to convert meters to degrees (only roughly though)
#' @export
#' @param x A length in meters
#' @return numeric value or vector
#' @examples
#' metersToDegrees(1)
metersToDegrees <- function(x){
  result <- 1 / 111000 * x
  return(result)
}

################################################################################
#### Function to Degrees into Meters
################################################################################
#' Convert degrees to meters
#'
#' Function to convert degrees to meters (only roughly though)
#' @export
#' @param x A length in degrees
#' @return numeric value or vector
#' @examples
#' degreesToMeters(0.01)
degreesToMeters <- function(x){
  result <- 111000 * x
  return(result)
}

################################################################################
#### Function to darken a color
################################################################################
#' Darken a color
#'
#' Function to darken a color
#' @param color A character string naming a color
#' @param factor Factor by which the color should be darkened. 1.4 by default
#' @return Hexadecimal code of the darkened color
#' @examples
#' darken("blue")
#' plot(1:2, 1:2, cex = 70, pch = 20, col = c("blue", darken("blue", 3)))
#' @export
darken <- function(color, factor = 1.4){
    col <- col2rgb(color)
    col <- col / factor
    col <- rgb(t(col), maxColorValue = 255)
    col
}

################################################################################
#### Function to Turn Steps to Spaital Lines
################################################################################
#' Turn steps to Spatial Lines
#'
#' Function to create spatial lines from steps (from amt package)
#' @export
#' @param track Track created using the amt package
#' @param crs Coordinate reference system of the track
#' @return \code{SpatialLinesDataFrame}
lineTrack <- function(track, crs){

  # Define the start and end coordinates
  begincoords <- data.frame(x = track$x1_, y = track$y1_)
  endcoords   <- data.frame(x = track$x2_, y = track$y2_)

  # Create lines from the coordinates
  l <- vector("list", nrow(begincoords))
  for (i in seq_along(l)){
      l[[i]] <- Lines(list(Line(rbind(
          begincoords[i, ]
        , endcoords[i,]
      ))), as.character(i))
  }

  # Make the lines spatial
  lines <- SpatialLines(l)

  # Assign a crs to the lines
  crs(lines) <- crs

  # Coerce the SpatialLines to a SpatialLinesDataFrame
  row.names(track) <- NULL
  lines <- SpatialLinesDataFrame(lines, data = track)

  # Return the lines
  return(lines)
}

################################################################################
#### Function to Calculate the Heading of a Step (Step Representation)
################################################################################
#' Calculate step heading
#'
#' Function to calculate heading, sometimes also called absolute turning angle
#' @export
#' @param x Track generated using the amt package
#' @return numeric value or vector
absAngle <- function(x){

  # Get the differences in x and y axes
  xx <- x$x2_ - x$x1_
  yy <- x$y2_ - x$y1_

  # Identify the sign of the difference
  b <- sign(xx)

  # Remove 0
  b[b == 0] <- 1

  # Calculate the Heading
  tempangle <- b * (yy < 0) * pi + atan(xx / yy)

  # If the heading is NA we know the animal was moving straight to north
  tempangle[is.na(tempangle)] <- 0

  # Make sure the heading is between 0 and 2pi
  tempangle[tempangle < 0] <- tempangle[tempangle < 0] + 2 * pi

  # Return the heading
  return(tempangle)
}

################################################################################
#### Function to Calculate the Heading of a Step (Point Representation)
################################################################################
#' Calculate step heading
#'
#' Function to calculate heading, sometimes also called absolute turning angle
#' @export
#' @param x Track generated using the amt package
#' @return numeric value or vector
absAngle2 <- function(x){

  # Get the differences in x and y axes
  xx <- x$x[2] - x$x[1]
  yy <- x$y[2] - x$y[1]

  # Identify the sign of the difference
  b <- sign(xx)

  # Remove 0
  b[b == 0] <- 1

  # Calculate the Heading
  tempangle <- b * (yy < 0) * pi + atan(xx / yy)

  # If the heading is NA we know the animal was moving straight to north
  tempangle[is.na(tempangle)] <- 0

  # Make sure the heading is between 0 and 2pi
  tempangle[tempangle < 0] <- tempangle[tempangle < 0] + 2 * pi

  # Return the heading
  return(tempangle)
}

################################################################################
#### Function to Resample GPS Fixes
################################################################################
#' Resample GPS fixes
#'
#' Function to resample fixes to a coarser resolution
#' @export
#' @param data Data that contains a column entitled "Timestamps"
#' @param hours Aspired time between fixes in hours
#' @param start Hour at which the resampled tracks should start
#' @param individual Column that contains individual names
#' @return \code{data.frame}
resFix <- function(data, hours, start, individual){

  # Split the dataframe by individual
  data <- split(data, as.factor(data[, individual]))

  # Loop over each list entry
  data <- lapply(data, function(x){

    # Identify the first date at which a fix was taken
    first <- range(x$Timestamp)[1] %>%

      # Update the time to the start time specified in the function
      update(., hour = start, min = 0, sec = 0)

    # Identify the last date at which a fix was taken
    last <- range(x$Timestamp)[2] %>%

      # Update the time to the end time specified in the function
      update(., hour = 24, min = 0, sec = 0)

    # Prepare a range of dates for which we would expect to find data according
    # to the specified sampling scheme
    dates <- seq(first, last, by = paste0(hours, " hours")) %>%

      # Coerce the data to a dataframe
      as.data.frame() %>%

      # Give the column a nicer name
      set_names("Timestamp")

    # Coerce the dataframes to data.tables
    setDT(dates)
    setDT(x)

    # Define the keys by which we will join
    setkey(dates, Timestamp)
    setkey(x, Timestamp)

    # Duplicate the Timestamps in the data table
    x[, Timestamp2 := Timestamp]

    # Run the join
    x <- x[dates, roll = "nearest"]

    # Remove the Timestamps that we created to resample
    x$Timestamp <- NULL

    # Coerce the data.table to a dataframe
    x <- as.data.frame(x) %>%

      # Rename the remaining timestmap column
      rename(Timestamp = Timestamp2) %>%

      # Remove any duplicates
      distinct()

    # Return the resulting dataframe
    return(x)
  })

  # Collapse the list
  data <- do.call(rbind, data)

  # Remove the rownames
  rownames(data) <- NULL

  # Return the final dataframe
  return(data)
}

#' Resample GPS fixes
#'
#' Function to resample fixes to a coarser resolution
#' @export
#' @param data Data that contains a column entitled "Timestamps"
#' @param hours Aspired time between fixes in hours
#' @param start Hour at which the resampled tracks should start
#' @return \code{data.frame}
resFix2 <- function(data, hours, start){

  # Identify the first date at which a fix was taken
  first <- range(data$Timestamp)[1] %>%

    # Update the time to the start time specified in the function
    update(., hour = start, min = 0, sec = 0)

  # Identify the last date at which a fix was taken
  last <- range(data$Timestamp)[2] %>%

    # Update the time to the end time specified in the function
    update(., hour = 24, min = 0, sec = 0)

  # Prepare a range of dates for which we would expect to find data according
  # to the specified sampling scheme
  dates <- seq(first, last, by = paste0(hours, " hours")) %>%

    # Coerce the data to a dataframe
    as.data.frame() %>%

    # Give the column a nicer name
    set_names("Timestamp")

  # For each Timestamp we now identify the closest fix
  closest <- sapply(1:nrow(dates), function(x){

    # Identify the index of the closest fix
    index <- which.min(abs(dates$Timestamp[x] - data$Timestamp))[1]

    # Check if the time difference is smaller than 30 mins
    close <- as.numeric(abs(dates$Timestamp[x] - data$Timestamp[index]), units = "hours") < hours/2

    # In case the fix is close enough, return its index
    if (close){
      return(index)
    } else {
      return(NA)
    }
  })

  # Remove NAs
  closest <- na.omit(closest)

  # Return respective fixes
  return(data[closest, ])
}

################################################################################
#### Function to Extract Covariates
################################################################################
#' Extract covariates
#'
#' Function to extract raster values and calculate their average coverage along
#' a spatial feature
#' @export
#' @param x \code{RasterLayer} or \code{RasterStack} containing covariates
#' @param y Spatial feature (\code{sp} package) below which covariates should be
#' extracted
#' @return \code{data.frame}
extrCov <- function(x, y){

  # Load required packages
  require(velox)
  require(tidyverse)

  # Crop the raster to the extent of the spatial feature
  raster <- crop(x, extent(y), snap = "out")

  # Coerce the raster to a velox raster
  raster <- velox(raster)

  # Extract the raster-values below the specified polygon
  extracted <- raster$extract(y, small = TRUE) %>%

    # Calculate the percentage cover
    lapply(., function(z) colSums(z) / nrow(z)) %>%

    # Bind the results together
    do.call(rbind, .) %>%

    # Turn them into a dataframe
    as.data.frame()

    # Define the output
    return(extracted)
}

################################################################################
#### Function to Extract Covariates (Fast)
################################################################################
#' Extract covariates quickly
#'
#' Function to extract raster values and calculate their average coverage along
#' a spatial feature. This function requires the velox package which allows a
#' much quicker value extraction.
#' @export
#' @param raster \code{velox raster} containing covariates
#' @param feature Spatial feature (\code{sp} package) below which covariates
#' should be extracted
#' @return \code{data.frame}
extrCov2 <- function(raster = NULL, feature = NULL){

  # Copy velox raster
  raster_copy <- raster$copy()

  # Crop the raster
  raster_copy$crop(feature)

  # Extract values
  extracted <- raster_copy$extract(feature, small = TRUE)

  # Calculate average values
  extracted <- vapply(extracted, colMeans, numeric(raster_copy$nbands))

  # Coerce to dataframe
  extracted <- as.data.frame(t(extracted))

  # Return the final dataframe
  return(extracted)
}

################################################################################
#### Function to Join by Nearest Date
################################################################################
#' Join dataframes by date
#'
#' Function that allows a left-join by dates
#' @export
#' @param df1 "left" data frame
#' @param df2 "right" data frame
#' @param name1 Column of df1 containing names of individuals
#' @param name2 Column of df2 containing names of individuals
#' @param date1 Column of df1 containing the timestamps
#' @param date2 Column of df1 containing the timestamps
#' @return \code{data.frame}
nearJoin <- function(df1, df2, name1, name2, date1, date2){
  out <- lapply(intersect(df1[, name1], df2[, name2]), function(x){
    d1 <- df1[df1[, name1] == x, ]
    d2 <- df2[df2[, name2] == x, ]
    d1$indices <- sapply(d1[, date1], function(d) which.min(abs(d2[, date2] - d)))
    d2$indices <- 1:nrow(d2)
    merge(d1, d2, by = c(name1 = name2, "indices"))
  })
  out <- do.call(rbind, out)
  out$indices <- NULL
  return(out)
}

################################################################################
#### Function to Create Dynamic Watermask
################################################################################
#' Create Dynamic Watermasks
#'
#' Function to createa dynamic watermask based on previous floodmaps.
#' @export
#' @param date date. Date for which a watermask should be calculated
#' @param floodmaps character. Filenames of all floodmaps that already exist
#' @param filedates vector of dates. Dates which the above floodmaps represent
#' @param years numeric. Number of years that should be considered to create a
#' watermask
#' @param Threshold numeric between 0 and 1. How often (relative frequency) a
#' pixel needs to be inundated in the past x years to be considered in the
#' watermask
#' @return \code{SpatialPolygons} of the waterask
watMask <- function(
    date      = NULL
  , floodmaps = NULL
  , filedates = NULL
  , years     = 5
  , threshold = 0.99){

  # Make naming nicer
  end_date <- date

  # Subtract 5 years, to get the first date we would include to calculate the mask
  start_date <- end_date - years(5)

  # Identify all possible dates between start and end dates for which we would
  # include maps to calculate the mask
  period <- seq(start_date, end_date, "days")

  # Keep only those filenames which are within the period of interest
  files <- files[filedates %in% period]

  # Load the files into a stack
  formask <- rast(files, bands = 1)

  # Reclassify the stack so that water becomes 1, dryland and clouds 0
  rcl <- data.frame(old = c(0, 127, 255), new = c(1, 0, 0))
  formask <- classify(formask, rcl)

  # Sum the layers
  sum <- sum(formask)

  # Identify areas where there was water 99% of the time
  areas <- sum > threshold * nlyr(formask)
  areas <- raster(areas)

  # Polygonize
  wetmask <- rasterToPolygons(areas
    , fun = function(x){x == 1}
    , dissolve = TRUE
  )

  # Apply a small negative buffer to avoid errors due to pixel size
  wetmask <- gBuffer(wetmask, width = -1/111*0.25)

  # Return the final watermask
  return(wetmask)
}

################################################################################
#### Function to Extract Coefficients from a glmmTMB result
################################################################################
#' Extract glmmTMB Coefficients
#'
#' Function to Extract Coefficients from a glmmTMB result
#' @export
#' @param x glmmTMB model
#' @param zvalue Should the value be extracted as well? Needs to be logical.
#' @param pvalue Should the pvalue be extracted as well? Needs to be logical.
#' @return \code{data.frame}
getCoeffs <- function(x, zvalue = FALSE, pvalue = FALSE){

  # Let's extract the coefficients and their SEs
  coeffs <- summary(x)$coefficients$cond %>%

    # Coerce the data to a dataframe
    as.data.frame() %>%

    # Keep the row names as own column
    mutate(Covariate = row.names(.)) %>%

    # Rename columns more nicely
    rename(
        Coefficient = Estimate
      , SE          = `Std. Error`
      , zvalue      = `z value`
      , pvalue      = `Pr(>|z|)`
    ) %>%

  # Arrange the variables nicely
  dplyr::select(Covariate, Coefficient, SE, zvalue, pvalue)

  # Select the rows that we want to keep
  if (!zvalue){
    coeffs <- dplyr::select(coeffs, -zvalue)
  }
  if (!pvalue){
    coeffs <- dplyr::select(coeffs, -pvalue)
  }

  # Return the final dataframe
  return(coeffs)
}

################################################################################
#### Function to Plot Coefficients of a Model
################################################################################
#' Plot Model Results
#'
#' Function that allows us to easily depict the coefficients of a model
#' @export
#' @param x Requires a \code{dataframe} containing the columns Covariate,
#' Coefficient, and SE
#' @param shape shape to be plotted
#' @param size size of the shape
#' @param whiskers number that indicates the size of the whisikers, i.e. by how
#' much the SE should be multiplied.
#' @param xlim limits on the x-axis
#' @param order order in which the coefficients should be plotted
#' @return Plot depicting the model results
showCoeffs <- function(x
  , shape     = 1
  , size      = 2
  , whiskers  = 1.96
  , xlim      = c(-1, 1)
  , order     = levels(x$Covariate)){

  # Load required packages
  require(ggplot2)
  require(lemon)

  # Prepare plot with Covariates on the y-axis and the corresponding
  # coefficients on the x-axis
  ggplot(data = x, aes(y = Covariate, x = Coefficient)) +

    # Add points for the estimated coefficients
    geom_point(shape = shape, size = size) +

    # Add errorbars
    geom_errorbarh(aes(
        xmin = Coefficient - whiskers * SE
      , xmax = Coefficient + whiskers * SE
      , height = 0.2)
    ) +

    # Add a vertical line that indicates no effect
    geom_vline(xintercept = 0, linetype = "dashed", color = "darkgrey") +

    # Reverse the scale of the y-axis
    scale_y_discrete(limits = rev(order)) +

    # Use a simple black and white theme
    theme_classic() +

    # Make gridlines a bit thinner
    theme(
        panel.grid.minor = element_line(size = 0.0)
      , panel.grid.major = element_line(size = 0.0)
      , panel.border     = element_blank()
      , axis.line        = element_line()
      , axis.ticks       = element_line(colour = "black")    ) +

    # Scale axis to -1, 1
    xlim(xlim) +

    # Cap the axes
    coord_capped_cart(left = "both", bottom = "both") +

    # Change label of x-axis
    labs(x = expression(beta*"-Coefficient"))
}

################################################################################
#### Function to Run GLMMTMB like Muff & Fieberg
################################################################################
#' Fit glmmTMB model
#'
#' Write a function to run a glmmTMB conditional logistic regression based on
#' Muff et al.'s paper
#' @export
#' @param formula formula of the model
#' @param data to train the model
#' @return glmmTMB conditional logistic regression model
glmm_clogit <- function(formula, data){

  # Prepare Model call but do not fit
  model <- glmmTMB(formula
    , family  = poisson()
    , data    = data
    , doFit   = FALSE
  )

  # Set the variance of the intercept artificially high
  model$parameters$theta[1] <- log(1e6)

  # Tell glmmTMB not to change the first entry of the vector of variances and
  # give all other variances another indicator to make sure they can be freely
  # estimated
  nvarparm <- length(model$parameters$theta)
  model$mapArg <- list(theta = factor(c(NA, 1:(nvarparm - 1))))

  # Fit the model
  model <- glmmTMB:::fitTMB(model)

  # Return the model
  return(model)
}

################################################################################
#### Function to Prepare Model Calls for Desired Covariates
################################################################################
#' Prepare model call
#'
#' Helper function to prepare a glmmTMB model call
#' @export
#' @param x covariates that should enter the model
#' @param slope logical. Should a random slope be fit to the model?
#' @return formula
writeForm <- function(x, slope = TRUE){

  # Identify the fixed effects structure
  fixed <- paste(x, collapse = " + ")

  if (slope){

    # Create random slope for each fixed effect
    rando <- paste("(0 + ", x, "|id)", collapse = " + ")

    # Put the random and fixed effects together
    combi <- paste(fixed, rando, sep = " + ")

  } else {

    combi <- fixed

  }

  # Prepare basic iSSF model formula
  form <- (case_ ~
    + cos_ta_
    + log_sl_
    + (1|step_id_)
    + (0 + cos_ta_|id)
    + (0 + log_sl_|id)
  )

  # Update formula with the desired covariates
  form <- update(form, paste("~ . + ", combi))

  # Return the final formula
  return(form)
}

################################################################################
#### Function to Visualize Interactions
################################################################################
#' Visualize Interactions
#'
#' There is a really nice package that automatically shows how interactions
#' influence a dependent variable. The package is called "visreg" and the
#' corresponding function is called visreg2d. However, the package does not work
#' properly with our model and we need to come up with our own code.
#' @export
#' @param xVar name of the first variable of the interaction
#' @param yVar name of the second variable of the interaction
#' @param label.color color of the axis labels
#' @param exponentiate logical. Should the predicted values be exponentiated?
#' @param norm logical. Should the predicted values be normalized?
#' @param colorPalette Color palette used for the surface plot
#' @return Surface plot
visInt <- function(model
  , xVar
  , yVar
  , label.color   = "black"
  , exponentiate  = F
  , norm          = T
  , colorPalette  = NULL
  ){

  # Get the data used to train the model
  data <- model.frame(model)

  # Find minimum and maximum of the x variable
  minX <- min(data[, xVar])
  maxX <- max(data[, xVar])

  # Find minimum and maximum of the y variable
  minY <- min(data[, yVar])
  maxY <- max(data[, yVar])

  # Prepare two vectors of equal length
  seqX <- seq(from = minX, to = maxX, length = 100)
  seqY <- seq(from = minY, to = maxY, length = 100)

  # Specify limits
  xlim <- c(range(seqX))
  ylim <- c(range(seqY))

  # Get the beta estimates from the model
  coeffs <- getCoeffs(model)

  # Write a function to predict values based on x and y
  predictVals <- function(x, y){
      x * coeffs$Coefficient[coeffs$Covariate == xVar] +
      y * coeffs$Coefficient[coeffs$Covariate == yVar] +
      x * y * coeffs$Coefficient[coeffs$Covariate == paste0(xVar, ":", yVar)]
  }

  # Apply the function to the seqX and seqY vectors
  mat <- outer(seqX, seqY, predictVals)

  # Exponentiate values if desired
  if (exponentiate){
    mat <- exp(mat)
  }

  # Normalize values if desired
  normalize <- function(x){
    (x - min(x)) / (max(x) - min(x))
  }
  if (norm){
    mat <- normalize(mat)
    seqX <- normalize(seqX)
    seqY <- normalize(seqY)
    xlim <- normalize(xlim)
    ylim <- normalize(ylim)
  }

  # Prepare colors
  n <- length(pretty(c(range(mat)), 20)) - 1
  cols <- colorPalette(n)

  # Prepare a contour plot
  filled.contour(
    , x = seqX
    , y = seqY
    , z = mat
    , xlim = xlim
    , ylim = ylim
    , zlim = c(range(mat))
    , xlab  = xVar
    , ylab  = yVar
    , main  = paste0(xVar, ":", yVar)
    , col   = cols
    , col.lab   = label.color
    , col.main  = label.color
    , plot.axes = {
      axis(1, col = label.color, col.ticks = label.color, col.axis = label.color)
      axis(2, col = label.color, col.ticks = label.color, col.axis = label.color)
    }
  )
}

#' Visualize Interactions
#'
#' There is a really nice package that automatically shows how interactions
#' influence a dependent variable. The package is called "visreg" and the
#' corresponding function is called visreg2d. However, the package does not work
#' properly with our model and we need to come up with our own code.
#' @export
#' @param xVar name of the first variable of the interaction
#' @param yVar name of the second variable of the interaction
#' @param label.color color of the axis labels
#' @param exponentiate logical. Should the predicted values be exponentiated?
#' @param norm logical. Should the predicted values be normalized?
#' @return \code{RasterLayer}
visInt2 <- function(model
  , xVar
  , yVar
  , exponentiate  = F
  , norm          = T
  ){

  # Get the data used to train the model
  data <- model.frame(model)

  # Find minimum and maximum of the x variable
  minX <- min(data[, xVar])
  maxX <- max(data[, xVar])

  # Find minimum and maximum of the y variable
  minY <- min(data[, yVar])
  maxY <- max(data[, yVar])

  # Prepare two vectors of equal length
  seqX <- seq(from = minX, to = maxX, length = 100)
  seqY <- seq(from = minY, to = maxY, length = 100)

  # Specify limits
  xlim <- c(range(seqX))
  ylim <- c(range(seqY))

  # Get the beta estimates from the model
  coeffs <- getCoeffs(model)

  # Write a function to predict values based on x and y
  predictVals <- function(x, y){
      x * coeffs$Coefficient[coeffs$Covariate == xVar] +
      y * coeffs$Coefficient[coeffs$Covariate == yVar] +
      x * y * coeffs$Coefficient[coeffs$Covariate == paste0(xVar, ":", yVar)]
  }

  # Apply the function to the seqX and seqY vectors
  mat <- outer(seqX, seqY, predictVals)

  # Exponentiate values if desired
  if (exponentiate){
    mat <- exp(mat)
  }

  # Normalize values if desired
  normalize <- function(x){
    (x - min(x)) / (max(x) - min(x))
  }
  if (norm){
    mat <- normalize(mat)
    seqX <- normalize(seqX)
    seqY <- normalize(seqY)
    xlim <- normalize(xlim)
    ylim <- normalize(ylim)
  }

  # Get the data used to train the model
  data <- model.frame(model)

  # Write a function to rotate the matrix
  rotate <- function(x) apply(t(x), 2, rev)

  # Rotate the matrix
  mat <- rotate(mat)

  # Turn matrix into raster and return it
  return(raster(mat))
}

################################################################################
#### Function to calculate Distances on a Raster Efficiently
################################################################################
#' Calculate Raster Distance
#'
#' Function to calculate the distance of a raster cell to the nearest cell
#' containing a certain value
#' @export
#' @param x \code{RasterLayer} on which distances should be calculated
#' @param value value to which the distance should be calculated
#' @return \code{RasterLayer}
distanceTo <- function(x, value = 1){

  # This package requires some packages
  require(spatstat)
  require(maptools)

  # Convert raster to ppp object
  ppp <- x %>%
    rasterToPoints(., fun = function(x){x == value}, spatial = TRUE) %>%
    spTransform(., CRS("+init=epsg:32734")) %>%
    as(., "ppp")

  # Create empty raster onto which the distances are calculated
  distance <- raster(x)
  values(distance) <- distance %>%
    as(., "SpatialPoints") %>%
    spTransform(., CRS("+init=epsg:32734")) %>%
    as(., "ppp") %>%
    nncross(., ppp) %>%
    .[["dist"]]

  # Return the distance raster
  return(distance)
}

################################################################################
#### Function to Split Data into Training and Testing
################################################################################
#' Split data into training and testing sets
#'
#' Function to split dataframe into training and testing data
#' @export
#' @param x \code{dataframe} to be split
#' @param ratio number between 0 and 1 indicating the ratio into which the data
#' should be split. I.e. if the ratio is set to 0.75, 75% of the data will end
#' up as training data
#' @return Named list containing a training dataframes and a testing dataframe
splitDat <- function(x, ratio = 0.75){
  n <- nrow(x)
  split <- rep(FALSE, n)
  split[sample(n, ratio * n)] <- TRUE
  split <- list(Training = x[split, ], Testing = x[!split, ])
  return(split)
}

################################################################################
#### Function to Split RasterStack into List
################################################################################
#' Split RasterStack into List
#'
#' Function to split a \code{RasterStack} into a list
#' @export
#' @param r \code{RasterStack} to be split
#' @param n number of splits
#' @return List of \code{RasterStack}
splitStack <- function(r, n = 2){

  # Check if n > length(s)
  if (n > nlayers(r)){
    n <- nlayers(r)
  }

  # Create a variable that assigns groups
  groups <- sort(rep(1:n, length.out = nlayers(r)))

  # Put geometries into the respective list entry
  listed <- lapply(1:n, function(x){
    r[[which(groups == x)]]
  })

  # Return the list
  return(listed)
}

################################################################################
#### Function to Split Shapefile into List
################################################################################
#' Split Shapefile into List
#'
#' Function to split an \code{sp} object into a list
#' @export
#' @param s shape to be split
#' @return List of spatial objects from the \code{sp} package
splitShape <- function(s, n = 2){

  # Check if n > length(s)
  if (n > length(s)){
    n <- length(s)
  }

  # Create a variable that assigns groups
  groups <- sort(rep(1:n, length.out = length(s)))

  # Put geometries into the respective list entry
  listed <- lapply(1:n, function(x){
    s[which(groups == x), ]
  })

  # Return the list
  return(listed)
}

################################################################################
#### Function to Read csvs on Multiple Cores
################################################################################
#' Read CSVs on Multiple Cores
#'
#' Function to quickly read a couple of csvs using multiple cores
#' @export
#' @param x path of the csvs to be loaded
#' @return \code{tibble}
mcread_csv <- function(x){
  mclapply(x, function(y){
    suppress_messages(read_csv(y))
  }, mc.cores = detectCores() - 1) %>% do.call(rbind, .)
}

################################################################################
#### Function to Reclassify RasterStack on Multiple Cores
################################################################################
#' Reclassify Rasterstack on Multiple Cores
#'
#' Function to quickly reclassify rasterstacks on multiple cores
#' @export
#' @param r \code{RasterStack} to be reclassified
#' @param rcl reclassification matrix
#' @return \code{RasterStack}
mcreclassify <- function(r, rcl, ...){

  # Function requires the parallel package
  require(parallel)

  # Identify number of cores and subtract 1
  n <- detectCores() - 1

  # Split stack into a list
  r_list <- splitStack(r, n = n)

  # Reclassify on multiple cores
  r_list <- mclapply(r_list, function(x){reclassify(x, rcl)}, mc.cores = n)

  # Return the stack
  return(stack(r_list))
}

################################################################################
#### Function to Crop RasterStack on Multiple Cores
################################################################################
#' Crop Rasterstack on Multiple Cores
#'
#' Crop a rasterstack quickly using multiple cores
#' @export
#' @param r \code{RasterStack} to be reclassified
#' @param e extent to which the \code{RasterStack} should be cropped
#' @return \code{RasterStack}
mccrop <- function(r, e, ...){

  # Function requires the parallel package
  require(parallel)

  # Identify number of cores and subtract 1
  n <- detectCores() - 1

  # Split stack into a list
  r_list <- splitStack(r, n = n)

  # crop on multiple cores
  r_list <- mclapply(r_list, function(x){crop(x, e)}, mc.cores = n)

  # Return the stack
  return(stack(r_list))
}

################################################################################
#### Function to Find Index of Lagged Closest Date
################################################################################
#' Find the Index of A Lagged Closest Date
#'
#' Function to identify the index of a lagged closest date
#' @export
#' @param a vector of dates 1
#' @param b vector of dates 2
#' @param lag number indicating the lag
#' @param unit Unit of the lag
#' @return index of the matched date
closest <- function(a, b, lag, unit = "days"){
  differences <- a - b
  if (unit == "months"){
    min <- min(differences[differences >= months(lag)])
  } else {
    min <- min(differences[differences >= days(lag)])
  }
  index <- which(differences == min)
  return(index)
}

# ################################################################################
# #### Function to Combine Covariates
# ################################################################################
# # Write a function that combines covariates # Notation: x = Floodmaps, s =
# # static covars, d1 = dynamic covars 1, d2 = dynamic covars 2
# combCovars <- function(x, s, d1, d2){
#
#   # Combine floodmap with static covariates
#   comb <- stack(x, s)
#
#   # Identify date of x layer
#   date_x <- names(x) %>%
#     substr(start = 12, stop = 21) %>%
#     paste0(., ".15") %>%
#     as.Date(format = "%Y.%m.%d")
#
#   # Rename the layer
#   names(comb)[[1]] <- "Inundated"
#
#   # Extract dates from d1 stack
#   dates_d1 <- names(d1) %>%
#     substr(start = 6, stop = 13) %>%
#     paste0(., ".15") %>%
#     as.Date(format = "%Y.%m.%d")
#
#   # Identify index of date in d1 that is closest to the date of x
#   index <- closest(date_x, dates_d1, lag = 0)
#
#   # Combine data with d1 layer that is closest in date
#   d1_layer <- d1[[index]]
#   names(d1_layer) <- "LocalRain"
#   comb <- stack(comb, d1_layer)
#
#   # Coerce to dataframe
#   comb <- as.data.frame(comb, xy = T)
#
#   # Assign date as new column
#   comb$Date <- date_x
#
#   # Combine with data about rain in catchment area
#   comb$CatchmentRains_0Months <-
#     d2[closest(date_x, d2$Date, lag = 0, unit = "months"), 2][[1]]
#   comb$CatchmentRains_1Months <-
#     d2[closest(date_x, d2$Date, lag = 1, unit = "months"), 2][[1]]
#   comb$CatchmentRains_2Months <-
#     d2[closest(date_x, d2$Date, lag = 2, unit = "months"), 2][[1]]
#   comb$CatchmentRains_3Months <-
#     d2[closest(date_x, d2$Date, lag = 3, unit = "months"), 2][[1]]
#   comb$CatchmentRains_4Months <-
#     d2[closest(date_x, d2$Date, lag = 4, unit = "months"), 2][[1]]
#   comb$CatchmentRains_5Months <-
#     d2[closest(date_x, d2$Date, lag = 5, unit = "months"), 2][[1]]
#   comb$CatchmentRains_6Months <-
#     d2[closest(date_x, d2$Date, lag = 6, unit = "months"), 2][[1]]
#   comb$CatchmentRains_7Months <-
#     d2[closest(date_x, d2$Date, lag = 7, unit = "months"), 2][[1]]
#   comb$CatchmentRains_8Months <-
#     d2[closest(date_x, d2$Date, lag = 8, unit = "months"), 2][[1]]
#   comb$CatchmentRains_9Months <-
#     d2[closest(date_x, d2$Date, lag = 9, unit = "months"), 2][[1]]
#
#   # Rename columns nicely
#   comb <- rename(comb,
#       ID            = id
#     , Elevation     = elev
#     , Slope         = slope
#     , Aspect        = aspect
#     , Distributary  = distributaries
#     , NoInundated   = no_inundated
#     , Rank          = rank
#   )
#
#   # Return the dataframe
#   return(comb)
#
# }

################################################################################
#### Function to Create Centroids that lie WITHIN Polygons
################################################################################
#' Create centroid within a polygon
#'
#' Function to create centroid within a polygon
#' @export
#' @param pol \code{SpatialPolygons} or \code{SpatialPolygonsDataFrame} in which
#' the centroids should be identified
#' @return \code{SpatialPointsDataFrame}
gCentroidWithin <- function(pol){

  # Load required packages
  require(rgeos)

  # Identify the number of polygons
  pol$.tmpID <- 1:length(pol)

  # Calculate centroids
  initialCents <- gCentroid(pol, byid = T)

  # Put data from the polygons to the centroids
  centsDF <- SpatialPointsDataFrame(initialCents, pol@data)

  # Indicate that these are true centroids
  centsDF$isCentroid <- TRUE

  # Check if the centroids are within the polygons
  centsInOwnPoly <- sapply(1:length(pol), function(x){
    gIntersects(pol[x,], centsDF[x, ])
  })

  # In case all centroids are within the polygons. We're done
  if(all(centsInOwnPoly) == TRUE){
        return(centsDF)
  } else {

    # We substitue outside centroids with points inside the polygon
    newPoints <- SpatialPointsDataFrame(
        gPointOnSurface(
            pol[!centsInOwnPoly, ]
          , byid = T
        )
      , pol@data[!centsInOwnPoly,]
    )

    # Indicate that these are not true centroids
    newPoints$isCentroid <- FALSE

    # Replace outside entrouds
    centsDF <- rbind(centsDF[centsInOwnPoly, ], newPoints)

    # Order points according to their polygon counterparts
    centsDF <- centsDF[order(centsDF$.tmpID), ]

    # Remove temporary ID column
    centsDF@data <- centsDF@data[, - which(names(centsDF@data) == ".tmpID")]

    # Return points
    return(centsDF)
  }
}

################################################################################
#### Function to Rasterize Shapefiles Using Velox
################################################################################
#' Rasterize Shapes Using Velox
#'
#' Function to rasterize using velox
#' @export
#' @param l \code{SpatialPoints}, \code{SpatialLines}, or \code{SpatialPolygons}
#' to be rasterized
#' @param r \code{RasterLayer} onto which the objects should be rasterized
#' @return \code{RasterLayer}
rasterizeVelox <- function(l, r){

  # Load required package
  require(velox)

  # Prepare a raster to which we will add all rasterized lines
  summed <- raster(r)
  summed <- setValues(r, 0)

  # Loop through each line and rasterize it
  for (i in 1:length(l)){
    line <- l[i, ]
    line$value <- 1
    vx <- velox(r)
    vx$rasterize(line, field = "value", background = 0, small = T)
    vx <- vx$as.RasterLayer()
    summed <- calc(stack(summed, vx), sum)
    cat(i, "out of", length(l), "done...\n")
  }
  return(summed)
}

################################################################################
#### Function to Rasterize Using Terra
################################################################################
#' Rasterize Shapes Using terra
#'
#' Function to rasterize using terra
#' @export
#' @param l \code{SpatialPoints}, \code{SpatialLines}, or \code{SpatialPolygons}
#' to be rasterized
#' @param r \code{RasterLayer} onto which the objects should be rasterized
#' @return \code{RasterLayer}
rasterizeTerra <- function(l, r){

  # Load required packages
  require(terra)

  # Prepare a raster to which we will add all rasterized lines
  summed <- rast(r)
  values(summed) <- 0

  # Loop through each line and rasterize it
  for (i in 1:length(l)){
    line <- l[i, ]
    rasterized <- terra::rasterize(line, r, field = 1, background = 0)
    summed <- sum(c(summed, rasterized))
    cat(i, "out of", length(l), "done...\n")
  }
  return(summed)
}

################################################################################
#### Function to Extend Raster and Fill Observed Values
################################################################################
#' Extend Raster and Fill with Values
#'
#' Function to extend a \code{RasterLayer} an fill the extended area with values
#' sampled from the original \code{RasterLayer}
#' @export
#' @param x \code{RasterLayer}
#' @param y extent to which the raster should be extended
#' @return \code{RasterLayer}
extendRaster <- function(x, y){

  # Create mask of NA values
  na_mask <- is.na(x)

  # Extract values from raster
  vals <- values(x)

  # Remove na's
  vals <- na.omit(vals)

  # Extend raster to new extent
  r <- extend(x, y)

  # Also extend the mask
  na_mask <- extend(na_mask, y, value = 0)

  # Identify NAs in the new extent
  indices <- which(is.na(values(r)))

  # Replace them with values sampled from old raster
  values(r)[indices] <- vals[runif(length(indices), min = 1, max = length(vals))]

  # Make sure original NA's are put back on the map
  r <- mask(r, na_mask, maskvalue = 1, updatevalue = NA)

  # Return the new raster
  return(r)
}

################################################################################
#### Function to Create Segments (Lines) Between Spatial Points
################################################################################
#' Create Segments Between Spatial Points
#'
#' Function to create SpatialLines between \code{SpatialPoints}. In contrast to
#' \code{spLines} from the \code{raster} package this function returns each line
#' segment as individual line.
#' @export
#' @param x \code{SpatialPoints} or \code{SpatialPointsDataFrame} containing
#' points between which spatial lines should be generated
#' @return \code{SpatialLinesDataFrame}
createSegments <- function(x){

  # Identify its coordinates
  multil <- coordinates(x) %>%

  # Convert to data frame
  as.data.frame() %>%

  # Rename columns
  setNames(c("x", "y")) %>%

  # Make sure from-to coordinates are in one row
  mutate(x_to = lead(x)) %>%
  mutate(y_to = lead(y)) %>%

  # Remove na rows
  na.omit() %>%

  # Split each row into a list entry
  split(seq(nrow(.))) %>%

  # Coerce each row to a separate line
  lapply(function(z){
    l <- spLines(rbind(
        SpatialPoints(data.frame(x = z$x, y = z$y))
      , SpatialPoints(data.frame(y = z$x_to, x = z$y_to))
    ))
    l <- SpatialLinesDataFrame(l, data = z, match.ID = F)
  return(l)
  }) %>% do.call(rbind, .)

  # Return this new multiline object
  return(multil)
}

################################################################################
#### Function to cut line after intersection with polygon
################################################################################
#' Cut Lines after Intersection
#'
#' Function to cut a multiline after the intersection with a desired shape
#' @export
#' @param line \code{SpatialLines} or \code{SpatialLinesDataFrame} that needs to
#' be cut
#' @param polygon \code{SpatialPolygons} or \code{SpatialPolygonsDataFrame}
#' which is used to cut the line
#' @param precision precision at which the lines should be cut. The value needs
#' to be given in map units and depends on the crs
#' @return \code{SpatialLinesDataFrame}
cutLineAfter <- function(line, polygon, precision = 1){

  # Load required packages
  require(rgeos)

  # Backup data
  dat <- line@data

  # Extract reference crs
  ref_crs <- crs(polygon)

  # Split line into its segments
  line <- createSegments(line)
  crs(line) <- ref_crs

  # Identify the line segment that intersects with the polygon
  index <- min(which(gIntersects(polygon, line, byid = T)))

  # Extract the respective line
  line_int <- line[index, ]

  # Extract its coordinates
  coordis <- coordinates(line_int)[[1]][[1]]

  # Identify the number of points we need to distribute on the line
  n <- gLength(line_int) / precision

  # Make sure it is not smaller than one
  n <- max(n, 1)

  # Distribute regularly spaced points on this line
  line_split <- coordinates(spsample(line[index, ], type = "regular", n = n))

  # Add the original coordinates back in
  line_split <- rbind(coordis[1, ], line_split, coordis[2, ])

  # Create line segments between these points
  line_split <- createSegments(line_split)
  crs(line_split) <- ref_crs

  # Check which segment is the first the intersects with the buffer
  index2 <- min(which(gIntersects(polygon, line_split, byid = T)))

  # Remove segments after this index
  line_split <- line_split[1:index2, ]

  # Crop the rest
  line_split <- gDifference(line_split, polygon)

  # Coerce lines to spatial lines
  line <- as(line, "SpatialLines")
  line_split <- as(line_split, "SpatialLines")

  # Make split line a single line
  line_split <- gLineMerge(line_split)

  # Put things together
  line <- rbind(
      line[1:(index - 1), ]
    , line_split
  )

  # Simplify to single line
  line <- gLineMerge(line)

  # Put data back
  line <- SpatialLinesDataFrame(line, data = dat, match.ID = F)

  # Return the new line
  return(line)
}

################################################################################
#### Function to Cut Line at Closest Distance to a Point
################################################################################
#' Cut Line at Closest Distance to a Point
#'
#' Function to cut a spatial line at its closest distance to a spatial point
#' @export
#' @param line \code{SpatialLines} or \code{SpatialLinesDataFrame} that needs to
#' be cut
#' @param point \code{SpatialPoints} or \code{SpatialPointsDataFrame} which is
#' used as reference to which distances are calculated
#' @param polygon \code{SpatialPolygons} or \code{SpatialPolygonsDataFrame}
#' containing polygons within which the lines need to lie before they are cut
#' @param precision precision at which the lines should be cut. The value needs
#' to be given in map units and depends on the crs
#' @return \code{SpatialLinesDataFrame}
cutLineClose <- function(line, point, polygon, precision = 1){

  # Load required packages
  require(rgeos)

  # Backup data
  dat <- line@data

  # Extract reference crs
  ref_crs <- crs(point)

  # Split line into its seperate segments
  line <- createSegments(line)
  crs(line) <- ref_crs

  # Check distance of each line to the point
  distances <- as.vector(as.matrix(gDistance(point, line, byid = T)))

  # Also check which lines are within the polygon
  within <- as.vector(as.matrix(gIntersects(polygon, line, byid = T)))

  # Put all into a dataframe
  temp <- data.frame(
      ID        = 1:length(distances)
    , Distance  = distances
    , Within    = within
  )

  # Identify index of segment that lies within the polygon and closest to the
  # point
  temp <- temp[temp$Within, ]
  index <- temp$ID[temp$Distance == min(temp$Distance)][1]

  # Extract the respective segment
  line_close <- line[index, ]

  # Extract first and last coordinates of the segment
  coordis <- coordinates(line_close)[[1]][[1]]

  # Identify the number of points we need to distribute on the segment
  n <- gLength(line_close) / precision

  # Make sure it is not smaller than one
  n <- max(n, 1)

  # Distribute regularly spaced points on this segment
  crs(line_close) <- NA
  line_split <- coordinates(spsample(line_close, type = "regular", n = n))

  # Add the original coordinates back in
  line_split <- as.data.frame(rbind(coordis[1, ], line_split, coordis[2, ]))

  # Make data spatial
  coordinates(line_split) <- c("x", "y")
  crs(line_split) <- ref_crs

  # Identify point that is closest to source point
  min_index <- which.min(gDistance(point, line_split, byid = T))[1]

  # Get red of points that follow
  line_split <- line_split[1:min_index, ]

  # Create line segment between first and last remaining point
  line_split <- createSegments(
    rbind(
        line_split[1, ]
      , line_split[length(line_split), ]
    )
  )
  crs(line_split) <- ref_crs

  # Put things together
  line <- rbind(
      line[1:(index - 1), ]
    , line_split
  )

  # Simplify to single line
  line <- gLineMerge(line)

  # Put data back
  line <- SpatialLinesDataFrame(line, data = dat, match.ID = F)

  # Return the new line
  return(line)
}

################################################################################
#### Function to Split Line
################################################################################
#' Split Line into Equal Segments
#'
#' Function to segment a line into equal parts. There are two options. Either
#' the line is split into equal segments of equal length or the line is split
#' based on points that are regularly distributed on the line.
#' @export
#' @param line \code{SpatialLines} or \code{SpatialLinesDataFrame} that needs to
#' be cut
#' @param regular a logical value indicating if the line should be cut based on
#' a regular interval. \code{T} by default. If this is set to \code{F}, the line
#' is cut into lengths of equal lengths.
#' @return
splitLine <- function(line, regular = T, distance, k = 2){

  # Load required package
  require(spatstat)

  # Option 1: The line is split regularly
  if (regular){

    # Extract the lines coordinates
    coords <- coordinates(line)[[1]][[1]]

    # Coerce the line to a psp object
    lineppp <- suppressWarnings(as.psp(line))

    # Distribute points that are equally spaced on line
    linepoints <- pointsOnLines(lineppp, eps = distance)

    # Put their coordinates into a dataframe
    coords_new <- data.frame(x = linepoints$x, y = linepoints$y)

  # Option 2: The line is split into segments of equal length
  } else {

    # Identify the number of points we need to set on the line
    t <- 1/k * c(1:(k - 1))

    # Get the original coordinates of the line
    coords <- as.data.frame(coordinates(line)[[1]][[1]])

    # Calculate new coordinates so that the line is split into k segments
    x_new <- coords$x[1] * (1-t) + coords$x[2] * (t)
    y_new <- coords$y[1] * (1-t) + coords$y[2] * (t)
    coords_new <- data.frame(x = x_new, y = y_new)
  }

  # Create a new line connecting all coordinates
  coords_all <- rbind(coords[1, ], coords_new, coords[2, ])
  line <- spLines(SpatialPoints(coords_all))

  # Make sure each segment is stored seperately
  line <- createSegments(line)

  # Return the final lines
  return(line)
}

################################################################################
#### Function to Get Mode of a Vector
################################################################################
#' Identify Mode
#'
#' Function to identify the mode value of a vector
#' @export
#' @param v vector of numeric values for which the mode should be calculated
#' @return numeric value
getMode <- function(v){
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

################################################################################
#### Function to Derive the Visitation History of a Trajectory
################################################################################
#' Retrieve the Visitation History of a Track
#'
#' Function to get the visitation history
#' @export
#' @param raster \code{RasterLayer} containing categories which are used for the
#' visitation history
#' @param line \code{SpatialLines} or \code{SpatialLinesDataFrame} which depicts
#' a trajectory and is used to identify the visitation history.
#' @return \code{data.frame}
visitHist <- function(line, raster){

  # To identify the history of visited places we can extract raster values below
  # each trajectory (corresponding to the places IDs). However, values need to
  # be extracted in the same order in which the line passess them. This only
  # works with raster::extract, which is rather slow. To keep computations low,
  # we therefore first perform a check whether a trajectory ever leaves the
  # source area and enters (non-NA) foreign areas. If this is not the case, we
  # don't need to bother about extracting values in the right order. This check
  # can easily be done using the very quick velox$extract function. If the
  # trajectory indeed leaves the source area, we extract values along the line.
  require(velox)

  # Crop raster to line
  raster <- crop(raster, line, snap = "out")

  # Identify first relocation
  first <- as.data.frame(coordinates(line)[[1]][[1]])
  first <- SpatialPoints(first[1, ])

  # Extract raster value below first relocation
  first_value <- raster::extract(raster, first)

  # Coerce raster to velox raster
  raster_velox <- velox(raster)

  # Extract values below the entire trajectory
  all_values <- raster_velox$extract(line, small = T) %>%
    .[[1]] %>%
    as.vector() %>%
    na.omit()

  # Check if the trajectory ever enters another area
  ever_other <- sum(all_values != first_value) != 0

  # In case the trajectory never enters another ID, we're done...
  if (!ever_other){

    # ... and simply return a null value
    return(NULL)

  # Otherwise, we need to retrieve the visitation history using raster::extract
  } else {

    # Extract values along the trajectory (in the order in which the trajectory
    # crosses the areas)
    extracted <- raster::extract(raster, line, small = T, along = T)
    extracted <- unname(na.omit(extracted[[1]]))

    # Prepare visitation history from extracted values
    visits <- extracted %>%
      data.frame(from = lag(.), to = .) %>%
      na.omit() %>%
      subset(from != to)

    # Return the visitation history
    return(visits)
  }
}

################################################################################
#### Function to Create Transparent Colors
################################################################################
#' Create Transparent Colors
#'
#' Function to create transparent color
#' @export
#' @param color The color that you want to make transparent
#' @param percent Value between 0 and 100 indicating how transparent the color
#' should be
#' @return Hexadecimal color code
colTrans <- function(color, percent = 50){

  # RGB of color
  rgb <- col2rgb(color)

  # Create new color with transparency
  col_new <- rgb(
      rgb[1]
    , rgb[2]
    , rgb[3]
    , max = 255
    , alpha = (100 - percent) * 255 / 100
  )

  # Save the color
  return(col_new)
}

################################################################################
#### Function to Identify Bursts from Steps
################################################################################
#' Identify Bursts from Steps
#'
#' Function to identify bursts from steps as prepared from the \code{amt}
#' package
#' @export
#' @param x Dataframe resulting from the \code{amt} package
#' @return Burst IDs
stepBursts <- function(x){

  # For each row we check if the timestamp at the end of the step aligns with
  # the timestamp at the end of the previous step
  same <- sapply(2:nrow(x), function(y){
    same <- x$t1_[y] == x$t2_[y-1]
  })

  # For the first step we never know
  same <- c(FALSE, same)

  # Turn vector around
  diff <- !same

  # Put a burst ID to each cluster of steps
  bursts <- cumsum(diff)

  # Return the bursts
  return(bursts)
}

################################################################################
#### Function to Identify Distance Traveled Within XX Fixes
################################################################################
#' Calculate Distance Traveled
#'
#' Function to identify distance traveled in previous fixes
#' @export
#' @param x \code{data.frame} containing steps as prepared in the \code{amt}
#' package
#' @param fixes Numeric value indicaing how many fixes should be considered to
#' calculate the distance travelled
#' @return
distanceTraveled <- function(x, fixes = 6){
  distance <- rep(NA, nrow(x))
  if (nrow(x) > fixes){
    distance[(fixes + 1):nrow(x)] <- sapply((fixes + 1):nrow(x), function(y){
      sum(x$sl_[(y-fixes):(y - 1)])
    })
    return(distance)
  }
}

################################################################################
#### Function to Identify the Season in Botswana
################################################################################
#' Identify the Season in Botswana
#'
#' Function to identify the season in botswana
#' @export
#' @param x POSIXct
#' @return Season
getSeason <- function(x) {
    DS <- as.Date("2020-04-01", format = "%Y-%m-%d") # Start Dry
    WS <- as.Date("2020-11-01", format = "%Y-%m-%d") # Start Wet

    # Convert dates from any year to 2020 dates
    d <- as.Date(strftime(x, format = "2020-%m-%d"))

    # Identify season
    season <- ifelse(d >= DS & d < WS, "Dry", "Wet")

    # Return the season
    return(season)
}

################################################################################
#### Function to Create Random Points
################################################################################
#' Create Random Points
#'
#' Function that allows to either select source points in the center of the
#' catchment area or to randomize source points in the catchment area.
#' @export
#' @param points \code{SpatialPointsDataFrame} of the source points
#' @param areas \code{SpatialPolygonsDataFrame} of the source areas
#' @param n numeric value indicating how many source points should be sampled
#' @param randomize logical indicating if source points should be randomized
#' @return \code{SpatialPointsDataFrame}
createPoints <- function(points = NULL, areas = NULL, n = 1, randomize = F){
  if (!randomize){

    # Duplicate source points to n_disp
    points <- points[sort(rep(1:length(points), n)), ]

  } else {

    # Sample points within the catchment areas
    points <- lapply(1:nrow(areas), function(x){
      points_sampled <- spsample(areas[x, ], n = n, type = "random")
      points_sampled$ID <- rep(areas$ID[x], length(points_sampled))
      return(points_sampled)
    }) %>% do.call(rbind, .)
  }

  # Return the points
  return(points)
}

################################################################################
#### Function to Prepare Movement Model for Dispersal Simulation
################################################################################
#' Prepare Movement Model for Dispersal Simulation
#'
#' Function to prepare movement model for dispersal simulation
#' @export
#' @param model Movement model
#' @return \code{list}
prepareModel <- function(model = NULL){

  # Extract parameter estimates from model
  coeffs <- fixef(model)$cond

  # Obtain original model formula and coerce it to a terms object
  form <- model
  form <- formula(form)
  form <- terms(form)

  # Identify and remove unnecessary terms
  newform <- c("Intercept", "step_id_", "0 +")
  newform <- paste(newform, collapse = "|")
  newform <- grep(newform, attr(form, "term.labels"))
  newform <- drop.terms(form, newform, keep.response = F)
  newform <- formula(newform)

  # Return the coefficients and the formula
  return(list(
      coefficients  = coeffs
    , formula     = newform
  ))
}

###############################################################################
### Function to Force Absolute Turning Angle Between 0 and 2 Pi
###############################################################################
#' Normalize Turning Angles
#'
#' Function to normalize turning angles to a value between 0 and 2*pi
#' @export
#' @param x Turning angle
#' @return numeric value
normalizeAngle <- function(x){
  x[x > 2 * pi] <- x[x > 2 * pi] - 2 * pi
  x[x < 0] <- x[x < 0] + 2 * pi
  return(x)
}

###############################################################################
### Function to calculate new absolute turning angle (REPLACED IN C++)
###############################################################################
#' Calculate New Absolute Turning Angle
#'
#' Function to calculate new absolute turning angle
#' @export
#' @param absta Absolute turning angle
#' @param ta Relative turning angle
#' @return numeric value
getAbsNew <- function(absta = NULL, ta = NULL){

  # Calculate new absolute turning angle
  absta_new <- absta + ta

  # We need to make sure that the absolute turning angle ranges from 0 to 2 *
  # pi
  absta_new[absta_new > 2 * pi] <- absta_new[absta_new > 2 * pi] - 2 * pi
  absta_new[absta_new < 0] <- absta_new[absta_new < 0] + 2 * pi

  # Return it
  return(absta_new)
}

###############################################################################
### Function to Calculate New Endpoints (REPLACED IN C++)
###############################################################################
#' Calculate New Endpoints
#'
#' Function to calculate new endpoints
#' @export
#' @param xy Coordinates from where the simulated individual comes from
#' @param sl Step length in meters
#' @param absta Absolute turning angle
#' @return \code{data.frame}
calcEndpoints <- function(xy, sl, absta){

  # Initiate new matrix into which we store the new endpoints
  xy_new <- matrix(, nrow = n_rsteps, ncol = 2)

  # Make sure that the steps cover at least 1m
  sl_new <- sl
  sl_new[sl_new < 1] <- 1

  # Convert step lengths to degree
  sl_new <- sl_new / 111000

  # Calculate new endpoints
  xy_new[, 1] <- xy[, 1] + sin(absta) * sl_new
  xy_new[, 2] <- xy[, 2] + cos(absta) * sl_new

  # Return the results
  return(xy_new)
}

################################################################################
#### Function to Check if Points are within or outside an extent
################################################################################
#' Check if Coordinates are within Extent
#'
#' Function to Check if Points are within or outside an extent
#' @export
#' @param \code{data.frame} or \code{matrix} containing columns representing x
#' and y coordinates for which you want to know if they lie within an extent or
#' not.
#' @param \code{list} containing xmin, xmax, ymin, and ymax
#' @return Logical (\code{TRUE} = within extent, \code{FALSE} = outside extent)
pointsInside <- function(xy = NULL, extent = NULL){

  # Check if x value is within boundaries
  xy[, 1] > extent$xmin & xy[, 1] < extent$xmax &

    # Check if y value is within boundaries
    xy[, 2] > extent$ymin & xy[, 2] < extent$ymax
}

################################################################################
#### Function to Predict Selection Score from a glmmTMB Model
################################################################################
#' Predict Step Selection Scores
#'
#' Function to predict the step selection scores using a glmmTMB model
#' @export
#' @param coefficients named \code{list} containing the coefficients for each
#' covariate
#' @param formula object of class \code{formula}. The same formula that was used
#' to fit the glmmTMB model
#' @return \code{numeric vector} or \code{numeric value}
predictScore <- function(coefficients = NULL, formula = NULL, data = data){

  # Prepare model matrix from data
  modeldat <- model.matrix(formula, data = data)

  # Multiply the matrix with estimated selection coefficients to calculate
  # predicted selection scores
  scores <- exp(modeldat %*% coefficients)

  # Return the predicted scores
  return(scores)
}

################################################################################
#### Function to Simulate Dispersal
################################################################################
#' Simulate Dispersal
#'
#' Function to simulate dispersal based on a step selection model that was
#' fitted in the glmmTMB framework
#' @export
#' @param source \code{SpatialPointsDataFrame} of the source point from which
#' the disperser should be initiated
#' @param covars \code{RasterStack} of the spatial covariates
#' @param model iSSF model that was prepared using \code{prepareModel}
#' @param sl_dist gamma distribution based on which step lengths should be
#' sampled
#' @param sl_max numeric value indicating the maximal step length (in meters)
#' that is biologically sensible
#' @param date POSIXct of the start date
#' @param n_steps number of steps that should be simulated in total
#' @param n_rsteps number of random steps proposed at each relocation
#' @param stop logical indicating if the simulation should stop when a random
#' step leaves the study extent. \code{FALSE} by default, indicating that any
#' random step leaving the study extent will be omitted
#' @return
disperse <- function(
    source              = NULL    # Start Coordinates
  , covars              = NULL    # Spatial Covariates, prepared with our funct.
  , model               = NULL    # iSSF Model, prepared with our funct.
  , sl_dist             = NULL    # Step Length Distribution
  , sl_max              = Inf     # What is the largest possible step?
  , date                = as.POSIXct("2015-06-15 03:00:00", tz = "UTC")
  , n_steps             = 10      # Number of steps simulated
  , n_rsteps            = 25      # Number of random steps proposed
  , stop                = F){     # Should the simulation stop at a boundary?

  # Create a new dataframe indicating the first location. Note that we draw
  # random turning angles to start off
  track <- data.frame(
      x           = coordinates(source)[, 1]
    , y           = coordinates(source)[, 2]
    , absta_      = runif(1, min = 0, max = 2 * pi)
    , ta_         = runif(1, min = -pi, max = pi)
    , sl_         = NA
    , Timestamp   = date
    , BoundaryHit = FALSE
  )

  # Simulate random steps
  for (i in 1:n_steps){

    # Draw random turning angles
    ta_new <- runif(n_rsteps
      , min = -pi
      , max = +pi
    )

    # Draw random step lengths
    sl_new <- rgamma(n_rsteps
      , shape = sl_dist$params$shape
      , scale = sl_dist$params$scale
    )

    # In case the sl_ should be capped, do so
    if (sl_max != Inf){
      sl_new <- pmin(sl_new, sl_max)
    }

    # Identify origin of track
    begincoords <- track[i, c("x", "y")]

    # Calculate new absolute turning angles
    absta_new <- getAbsNewC(absta = track$absta_[i], ta = ta_new)

    # Calculate new endpoints
    endpoints_new <- calcEndpointsC(
        xy    = as.matrix(track[i, c("x", "y")])
      , absta = absta_new
      , sl    = sl_new
    )

    # Check which endpoints leave the study extent
    inside <- pointsInside(xy = endpoints_new, extent = covars$extent)

    # In case some steps are not inside the study area and we want the loop to
    # break
    if (sum(!inside) > 0 & stop){

      # Break the loop
      break

    # In case some steps are not inside the study area and we DONT want the loop
    # to break
    } else if (sum(!inside) > 0 & !stop){

      # Keep only steps inside the study area
      endpoints_new <- endpoints_new[inside, ]
      absta_new     <- absta_new[inside]
      ta_new        <- ta_new[inside]
      sl_new        <- sl_new[inside]

    }

    # Create spatial lines from origin to new coordinates
    l <- vector("list", nrow(endpoints_new))
    for (j in seq_along(l)){
        l[[j]] <- Lines(
          list(
            Line(
              rbind(
                  begincoords[1, ]
                , endpoints_new[j,]
              )
            )
          ), as.character(j)
        )
    }

    # Coerce to spatial lines
    steps <- SpatialLines(l)

    # Extract covariates along each step
    extracted <- extrCov2(covars$covars, steps)

    # Put some nice column names
    names(extracted) <- covars$covar_names

    # Put everything into a dataframe
    rand <- data.frame(
        x           = endpoints_new[, 1]
      , y           = endpoints_new[, 2]
      , absta_      = absta_new
      , ta_         = ta_new
      , sl_         = sl_new
      , BoundaryHit = sum(!inside)
    )

    # Add extracted data to each step
    rand <- cbind(rand, extracted)

    # Check if the timestamp corresponds to low or high activity
    Activity <- strftime(date, tz = "UTC", format = "%H:%M:%S")
    Activity <- factor(ifelse(Activity %in% c("03:00:00", "15:00:00")
        , yes = "MainActivity"
        , no  = "LowActivity"
      ), levels = c("LowActivity", "MainActivity"))

    # Update date
    date <- date + hours(4)

    # Note that we assume that no fix exists at 11:00. In this case we add
    # another 4 hours
    if(strftime(date, tz = "UTC", format = "%H:%M:%S") == "11:00:00"){
      date <- date + hours(4)
    }

    # Add the new date to the dataframe
    rand$Timestamp <- date
    rand$Activity <- Activity

    # Calculate selection scores
    SelectionScore <- as.numeric(predictScore(
        coefficients  = model$coefficients
      , formula       = model$formula
      , data          = rand
    ))

    # Coerce selection scores to probabilities
    Probs <- SelectionScore / sum(SelectionScore)

    # Sample a step according to the above predicted probabilities
    rand <- rand[sample(1:nrow(rand), size = 1, prob = Probs), ]

    # Add the step to our track
    track <- rbind(
        track[, c("x", "y", "absta_", "ta_", "sl_", "Timestamp", "BoundaryHit")]
      , rand[, c("x", "y", "absta_", "ta_", "sl_", "Timestamp", "BoundaryHit")]
    )
  }
  return(track)
}

################################################################################
#### Function to Prepare Raster Layers for Dispersal Simulation
################################################################################
#' Prepare Covariates for Simulation
#'
#' Function to prepare covariate layers for dispersal simulation
#' @export
#' @param layers \code{RasterStack} of covariates
#' @return \code{list}
prepareCovars <- function(layers){

  # Extract layer names
  names <- names(layers)

  # Extract layer extent
  extent <- extent(layers)
  extent <- list(
      xmin = extent@xmin
    , xmax = extent@xmax
    , ymin = extent@ymin
    , ymax = extent@ymax
  )

  # Convert layers to velox raster
  layers <- velox(layers)

  # Return a named list
  return(list(
      covars      = layers
    , covar_names = names
    , extent      = extent
  ))
}

################################################################################
#### Function to Normalize a map
################################################################################
#' Normalize a Raster
#'
#' Function to normalize a map
#' @export
#' @param x \code{RasterLayer} or \code{RasterStack} that you want to normalize
#' to a range between 0 and 1
#' @return \code{RasterLayer} or \code{RasterStack}
normalizeMap <- function(x){
  (x - minValue(x))/(maxValue(x) - minValue(x))
}

################################################################################
#### Function to Coerce Simulated Trajectories to Proper Lines
################################################################################
#' Coerce Simulated Trajectories to SpatialLines
#'
#' Function to coerce simulated coordinates to lines after a desired number of
#' steps
#' @export
#' @param simulations \code{data.frame} resulting from the function
#' \code{disperse}
#' @param steps Number of steps to be considered. Set to 68 by default meaning
#' that the first 68 steps are considered only
#' @param sampling Which tracks should be coerced? One of "Static" or "Random"
#' @param mc.cores Number of cores used for the coercion. Set to
#' \code{detectCores() - 1} by default.
#' @return \code{SpatialLinesDataFrame}
sims2tracks <- function(
    simulations = NULL
  , steps       = 68
  , sampling    = c("Static", "Random")
  , mc.cores    = detectCores() - 1
  ){

    # Subset data as desired
    sims <- subset(simulations
      , StepNumber <= steps
      & PointSampling %in% sampling
    )

    # Nest the data so that each trajectory (unique ID) receives its own row
    sims <- sims %>% group_by(ID) %>% nest()

    # Move from point to step representation (create lines from points)
    sims_traj <- mclapply(1:nrow(sims)
      , mc.cores = mc.cores
      , function(x){
        p <- SpatialPoints(sims$data[[x]][, c("x", "y")])
        l <- spLines(p)
        l$StartPoint <- sims$data[[x]]$StartPoint[1]
        return(l)
    }) %>% do.call(rbind, .)

    # Assign original IDs back to lines
    sims_traj$ID <- sims$ID

    # Assign proper crs
    crs(sims_traj) <- CRS("+init=epsg:4326")

    # Return the tracks
    return(sims_traj)
}

################################################################################
#### Function to Scale a Covariate Using a Scaling Table
################################################################################
#' Scale Covariates
#'
#' Function to scale covariates using a scaling table
#' @export
#' @param covars Either a \code{data.frame} or a \code{RasterStack} containing
#' continuous covariates.
#' @param scaling \code{data.frame} containing a "Center" column and a "Scale"
#' column. The \code{data.frame} must contain rownames to indicating the
#' covariates to which the scaling parameters belong to.
#' @return \code{data.frame} or \code{RasterStack}
scaleCovars <- function(covars, scaling){

  # Scale covariates with the parameters from the scaling table
  scaled <- sapply(1:ncol(covars), function(x){
    scale(covars[, x]
      , center  = scaling[names(covars)[x], ]$Center
      , scale   = scaling[names(covars)[x], ]$Scale
    )
  })

  # Coerce to dataframe
  scaled <- as.data.frame(scaled)

  # Assign names back
  names(scaled) <- names(covars)

  # Return scaled covars
  return(scaled)
}

################################################################################
#### LEGACY
################################################################################
# ################################################################################
# #### Function to Simulate Dispersal (OLD)
# ################################################################################
# # Function to simulate dispersal based on a step selection model that was fitted
# # in the glmmTMB framework
# disperse <- function(
#     source              = NULL    # Start Coordinates
#   , covars              = NULL    # Spatial Covariates, prepared with our funct.
#   , model               = NULL    # iSSF Model, prepared with our funct.
#   , sl_dist             = NULL    # Step Length Distribution
#   , max_sl              = Inf     # What is the largest possible step?
#   , date                = as.POSIXct("2015-06-15 03:00:00", tz = "UTC")
#   , n_steps             = 10      # Number of steps simulated
#   , n_rsteps            = 25      # Number of random steps proposed
#   , stop                = F){     # Should the simulation stop at a boundary?
#
#   # Create a new dataframe indicating the first location. Note that we draw
#   # random turning angles to start off
#   track <- data.frame(
#       x           = coordinates(source)[, 1]
#     , y           = coordinates(source)[, 2]
#     , absta_      = runif(1, min = 0, max = 2 * pi)
#     , ta_         = runif(1, min = -pi, max = pi)
#     , sl_         = NA
#     , Timestamp   = date
#     , BoundaryHit = FALSE
#   )
#
#   # Simulate random steps
#   for (i in 1:n_steps){
#
#     # Draw random turning angles
#     ta_new <- runif(n_rsteps
#       , min = -pi
#       , max = +pi
#     )
#
#     # Draw random step lengths
#     sl_new <- rgamma(n_rsteps
#       , shape = sl_dist$params$shape
#       , scale = sl_dist$params$scale
#     )
#
#     # In case the sl_ should be capped, do so
#     if (max_sl != Inf){
#       sl_new <- min(sl_new, max_sl)
#     }
#
#     # Identify origin of track
#     begincoords <- track[i, c("x", "y")]
#
#     # Calculate new absolute turning angles
#     absta_new <- getAbsNewC(absta = track$absta_[i], ta = ta_new)
#
#     # Calculate new endpoints
#     endpoints_new <- calcEndpointsC(
#         xy    = as.matrix(track[i, c("x", "y")])
#       , absta = absta_new
#       , sl    = sl_new
#     )
#
#     # Check which endpoints leave the study extent
#     inside <- pointsInside(xy = endpoints_new, extent = covars$extent)
#
#     # In case some steps are not inside the study area and we want the loop to
#     # break
#     if (sum(!inside) > 0 & stop){
#
#       # Break the loop
#       break
#
#     # In case some steps are not inside the study area and we DONT want the loop
#     # to break
#     } else if (sum(!inside) > 0 & !stop){
#
#       # Keep only steps inside the study area
#       endpoints_new <- endpoints_new[inside, ]
#       absta_new     <- absta_new[inside]
#       ta_new        <- ta_new[inside]
#       sl_new        <- sl_new[inside]
#
#     }
#
#     # Create spatial lines from origin to new coordinates
#     l <- vector("list", nrow(endpoints_new))
#     for (j in seq_along(l)){
#         l[[j]] <- Lines(
#           list(
#             Line(
#               rbind(
#                   begincoords[1, ]
#                 , endpoints_new[j,]
#               )
#             )
#           ), as.character(j)
#         )
#     }
#
#     # Coerce to spatial lines
#     steps <- SpatialLines(l)
#
#     # Extract covariates along each step
#     extracted <- extrCov2(covars$covars, steps)
#
#     # Put some nice column names
#     names(extracted) <- covars$covar_names
#
#     # Put everything into a dataframe
#     rand <- data.frame(
#         x           = endpoints_new[, 1]
#       , y           = endpoints_new[, 2]
#       , absta_      = absta_new
#       , ta_         = ta_new
#       , sl_         = sl_new
#       , BoundaryHit = sum(!inside)
#     )
#
#     # Add extracted data to each step
#     rand <- cbind(rand, extracted)
#
#     # Check if the timestamp corresponds to low or high activity
#     Activity <- strftime(date, tz = "UTC", format = "%H:%M:%S")
#     Activity <- factor(ifelse(Activity %in% c("03:00:00", "15:00:00")
#         , yes = "MainActivity"
#         , no  = "LowActivity"
#       ), levels = c("LowActivity", "MainActivity"))
#
#     # Update date
#     date <- date + hours(4)
#
#     # Note that we assume that no fix exists at 11:00. In this case we add
#     # another 4 hours
#     if(strftime(date, tz = "UTC", format = "%H:%M:%S") == "11:00:00"){
#       date <- date + hours(4)
#     }
#
#     # Add the new date to the dataframe
#     rand$Timestamp <- date
#     rand$Activity <- Activity
#
#     # Calculate selection scores
#     SelectionScore <- as.numeric(predictScore(
#         coefficients  = model$coefficients
#       , formula       = model$formula
#       , data          = rand
#     ))
#
#     # Keep only the step with the highest selection score
#     rand <- rand[SelectionScore == max(SelectionScore), ]
#
#     # In case several steps get the same score, choose one randomly
#     rand <- rand[sample(nrow(rand), 1), ]
#
#     # Add the step to our track
#     track <- rbind(
#         track[, c("x", "y", "absta_", "ta_", "sl_", "Timestamp", "BoundaryHit")]
#       , rand[, c("x", "y", "absta_", "ta_", "sl_", "Timestamp", "BoundaryHit")]
#     )
#   }
#   return(track)
# }
#
# ################################################################################
# #### Function To Simulate Dispersal (OLDEST)
# ################################################################################
# # Function to simulate dispersal based on a step selection model that was fitted
# # in the glmmTMB framework
# disperseOLD <- function(
#     source              = NULL    # Start point
#   , covars              = NULL    # Spatial Covariates
#   , model               = NULL    # iSSF Model
#   , sl_dist             = NULL    # Step Length Distribution
#   # , AttractionPoint     = NULL    # Point of attraction (Spatial Point)
#   # , AttractionStrength  = NULL    # Strength of attraction
#   , date                = as.POSIXct("2015-06-15 03:00:00", tz = "UTC")
#   , n_steps             = 10      # Number of steps simulated
#   , n_rsteps            = 25      # Number of random steps proposed
#   , stop                = TRUE){  # Should the simulation stop at a boundary?
#
#   # Load required packages
#   require(tidyverse)
#   require(lubridate)
#   require(sp)
#   require(glmmTMB)
#
#   # Create a new dataframe indicating the first location. Note that we draw
#   # random turning angles to start off
#   track <- data.frame(
#       x           = coordinates(source)[, 1]
#     , y           = coordinates(source)[, 2]
#     , absta_      = runif(1, min = 0, max = 2 * pi)
#     , ta_         = runif(1, min = -pi, max = pi)
#     , sl_         = NA
#     , Timestamp   = date
#     , BoundaryHit = FALSE
#   )
#
#   # Simulate random steps
#   for (i in 1:n_steps){
#
#     # Prepare an empty list into which we can store the random steps
#     rand <- list()
#
#     # Draw random turning angles
#     ta_new <- runif(n_rsteps
#       , min = -pi
#       , max = +pi
#     )
#
#     # Draw random step lengths
#     sl_new <- rgamma(n_rsteps
#       , shape = sl_dist$params$shape
#       , scale = sl_dist$params$scale
#     )
#
#     # Make sure that the steps cover at least 1m
#     sl_new[sl_new < 1] <- 1
#
#     # Convert step lengths to degree
#     sl_new <- sl_new / 111000
#
#     # Put the step lengths and turning angles into a new dataframe
#     rand <- data.frame(
#         absta_  = track$absta_[i] + ta_new
#       , ta_     = ta_new
#       , sl_     = sl_new
#     )
#
#     # We need to make sure that the absolute turning angle ranges from 0 to 2 *
#     # pi
#     rand$absta_[rand$absta_ > 2 * pi] <-
#       rand$absta_[rand$absta_ > 2 * pi] - 2 * pi
#     rand$absta_[rand$absta_ < 0] <-
#       rand$absta_[rand$absta_ < 0] + 2 * pi
#
#     # Calculate new endpoints
#     rand <- rand %>% mutate(.
#         , x = track$x[i] + sin(absta_) * sl_
#         , y = track$y[i] + cos(absta_) * sl_
#       )
#
#     # Calculate step lengths in meters again
#     rand$sl_ <- rand$sl_ * 111000
#
#     # Convert each step to a spatial line
#     begincoords <- track[i, c("x", "y")]
#     endcoords   <- rand[, c("x", "y")]
#
#     # Now create a seperate line for each step
#     l <- vector("list", nrow(endcoords))
#     for (j in seq_along(l)) {
#         l[[j]] <- Lines(
#           list(
#             Line(
#               rbind(
#                   begincoords[1, ]
#                 , endcoords[j,]
#               )
#             )
#           ), as.character(j)
#         )
#     }
#
#     # Coerce the lines to spatial lines
#     steps <- SpatialLines(l)
#     crs(steps) <- CRS("+init=epsg:4326")
#
#     # Depending on whether the simulation should stop at boundaries...
#     if (stop){
#
#       # ... we break the loop when a step leaves the study areas
#       extent  <- extent(covars) %>% as(., "SpatialPolygons")
#       inside  <- suppressWarnings(
#         gContainsProperly(extent, steps, byid = TRUE) %>% as.vector()
#       )
#       hit     <- sum(!inside)
#       if (hit > 0) break
#
#     # If the simulation should not stop when a boundary is hit...
#     } else {
#
#       # ... we remove any step leaving the boundaries
#       extent  <- extent(covars) %>% as(., "SpatialPolygons")
#       keep    <- suppressWarnings(
#         gContainsProperly(extent, steps, byid = TRUE) %>% as.vector()
#       )
#       hit     <- sum(!keep)
#       steps   <- steps[keep, ]
#       rand    <- rand[keep, ]
#
#     }
#
#     # Add the data about turning angles etc. to each spatial line
#     steps <- SpatialLinesDataFrame(steps, data = rand, match.ID = F)
#
#     # Extract covariates along each step
#     extracted <- extrCov(covars, steps)
#
#     # Put some nice column names
#     names(extracted) <- names(covars)
#
#     # Add extracted data to each step
#     rand <- cbind(rand, extracted)
#
#     # # In case a point of attraction is provided, identify each steps' distance
#     # # to the point of attraction
#     # if (!is.null(AttractionPoint)){
#     #   rand$DistanceToAttraction <- suppressWarnings(gDistance(
#     #       spTransform(AttractionPoint, CRS("+init=epsg:32734"))
#     #     , spTransform(steps, CRS("+init=epsg:32734"))
#     #     , byid = T
#     #   ))
#     # }
#
#     # Check if the timestamp corresponds to low or high activity
#     rand$Activity <- date %>% strftime(tz = "UTC", format = "%H:%M:%S")
#     rand$Activity <- factor(ifelse(rand$Activity %in% c("03:00:00", "15:00:00")
#         , yes = "MainActivity"
#         , no  = "LowActivity"
#       ), levels = c("LowActivity", "MainActivity"))
#
#     # Update date
#     date <- date + hours(4)
#
#     # Note that we assume that no fix exists at 11:00. In this case we add
#     # another 4 hours
#     if(strftime(date, tz = "UTC", format = "%H:%M:%S") == "11:00:00"){
#       date <- date + hours(4)
#     }
#
#     # Add the new date to the dataframe
#     rand$Timestamp <- date
#
#     # Calculate selection scores
#     rand$SelectionScore <- predictScoreOLD(model = model, data = rand)
#
#     # Keep only the step with the highest selection score
#     rand <- subset(rand, SelectionScore == max(SelectionScore))
#
#     # In case several steps get the same score, choose one randomly
#     rand <- rand[sample(nrow(rand), 1), ]
#
#     # Also indicate how many steps hit a boundary
#     rand$BoundaryHit <- hit
#
#     # Add the step to our track
#     track <- rbind(
#         track[, c("x", "y", "absta_", "ta_", "sl_", "Timestamp", "BoundaryHit")]
#       , rand[, c("x", "y", "absta_", "ta_", "sl_", "Timestamp", "BoundaryHit")]
#     )
#   }
#   return(track)
# }
#
# ################################################################################
# #### Function to Predict Selection Score from a glmmTMB Model (OLD)
# ################################################################################
# # Function to predict the selection score from a glmmTMB model
# predictScoreOLD <- function(model = NULL, data = NULL){
#
#   # Extract parameter estimates from model
#   coeffs <- fixef(model)$cond
#
#   # Obtain original model formula and coerce it to a terms object
#   form <- model %>%
#     formula() %>%
#     terms()
#
#   # Identify and remove unnecessary terms
#   newform <- c("Intercept", "step_id_", "0 +") %>%
#     paste(., collapse = "|") %>%
#     grep(., attr(form, "term.labels")) %>%
#     drop.terms(form, ., keep.response = F) %>%
#     formula()
#
#   # Coerce data to model matrix
#   modeldat <- model.matrix(newform, data = data)
#
#   # Multiply the matrix with estimated selection coefficients to calculate
#   # predicted selection scores
#   scores <- exp(modeldat %*% coeffs)
#
#   # Return the predicted scores
#   return(scores)
# }
#
#
# ################################################################################
# #### Function to Scale a Covariate Using a Scaling Table
# ################################################################################
# # Function to scale a covariate using a scaling table
# scaleCovar <- function(covar, table){
#   scale(covar
#     , center  = table[names(layer), ]$Center
#     , scale   = table[names(layer), ]$Scale
#   )
# }
#
