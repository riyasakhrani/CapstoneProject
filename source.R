##Put code here 
#
crime <- read.csv("./datasets/crimedata.years.csv")
pop <- read.csv("./datasets/pop.csv")
pop <- na.omit(pop)
survey <- read_csv("./datasets/survey.csv")
census_data = read_rds("./datasets/census_data.rds")
drive <- read.csv("./datasets/car_data.csv")
gender_survey = read.csv("./datasets/gender_survey.csv")

# Because theres no easy prebuilt way to flip  colours in leaftlet legend. From: https://stackoverflow.com/questions/40276569/reverse-order-in-r-leaflet-continuous-legend
addLegend_decreasing <- function (map, position = c("topright", "bottomright", "bottomleft","topleft"),
                                  pal, values, na.label = "NA", bins = 7, colors, 
                                  opacity = 0.5, labels = NULL, labFormat = labelFormat(), 
                                  title = NULL, className = "info legend", layerId = NULL, 
                                  group = NULL, data = getMapData(map), decreasing = FALSE) {
  
  position <- match.arg(position)
  type <- "unknown"
  na.color <- NULL
  extra <- NULL
  if (!missing(pal)) {
    if (!missing(colors)) 
      stop("You must provide either 'pal' or 'colors' (not both)")
    if (missing(title) && inherits(values, "formula")) 
      title <- deparse(values[[2]])
    values <- evalFormula(values, data)
    type <- attr(pal, "colorType", exact = TRUE)
    args <- attr(pal, "colorArgs", exact = TRUE)
    na.color <- args$na.color
    if (!is.null(na.color) && col2rgb(na.color, alpha = TRUE)[[4]] == 
        0) {
      na.color <- NULL
    }
    if (type != "numeric" && !missing(bins)) 
      warning("'bins' is ignored because the palette type is not numeric")
    if (type == "numeric") {
      cuts <- if (length(bins) == 1) 
        pretty(values, bins)
      else bins   
      if (length(bins) > 2) 
        if (!all(abs(diff(bins, differences = 2)) <= 
                 sqrt(.Machine$double.eps))) 
          stop("The vector of breaks 'bins' must be equally spaced")
      n <- length(cuts)
      r <- range(values, na.rm = TRUE)
      cuts <- cuts[cuts >= r[1] & cuts <= r[2]]
      n <- length(cuts)
      p <- (cuts - r[1])/(r[2] - r[1])
      extra <- list(p_1 = p[1], p_n = p[n])
      p <- c("", paste0(100 * p, "%"), "")
      if (decreasing == TRUE){
        colors <- pal(rev(c(r[1], cuts, r[2])))
        labels <- rev(labFormat(type = "numeric", cuts))
      }else{
        colors <- pal(c(r[1], cuts, r[2]))
        labels <- rev(labFormat(type = "numeric", cuts))
      }
      colors <- paste(colors, p, sep = " ", collapse = ", ")
    }
    else if (type == "bin") {
      cuts <- args$bins
      n <- length(cuts)
      mids <- (cuts[-1] + cuts[-n])/2
      if (decreasing == TRUE){
        colors <- pal(rev(mids))
        labels <- rev(labFormat(type = "bin", cuts))
      }else{
        colors <- pal(mids)
        labels <- labFormat(type = "bin", cuts)
      }
    }
    else if (type == "quantile") {
      p <- args$probs
      n <- length(p)
      cuts <- quantile(values, probs = p, na.rm = TRUE)
      mids <- quantile(values, probs = (p[-1] + p[-n])/2, na.rm = TRUE)
      if (decreasing == TRUE){
        colors <- pal(rev(mids))
        labels <- rev(labFormat(type = "quantile", cuts, p))
      }else{
        colors <- pal(mids)
        labels <- labFormat(type = "quantile", cuts, p)
      }
    }
    else if (type == "factor") {
      v <- sort(unique(na.omit(values)))
      colors <- pal(v)
      labels <- labFormat(type = "factor", v)
      if (decreasing == TRUE){
        colors <- pal(rev(v))
        labels <- rev(labFormat(type = "factor", v))
      }else{
        colors <- pal(v)
        labels <- labFormat(type = "factor", v)
      }
    }
    else stop("Palette function not supported")
    if (!any(is.na(values))) 
      na.color <- NULL
  }
  else {
    if (length(colors) != length(labels)) 
      stop("'colors' and 'labels' must be of the same length")
  }
  legend <- list(colors = I(unname(colors)), labels = I(unname(labels)), 
                 na_color = na.color, na_label = na.label, opacity = opacity, 
                 position = position, type = type, title = title, extra = extra, 
                 layerId = layerId, className = className, group = group)
  invokeMethod(map, data, "addLegend", legend)
}


# Merge census data and drive into new df
census_data_d <- merge(census_data, drive)

# Select the required columns
clean_cen <- census_data %>%
  select(GeoUID, `Region Name`, geometry, y21_transit, y21_walk, `21_bike`, walk_score, bike_score)

# Round up the walk and bike scores
clean_cen$walk_score <- round(clean_cen$walk_score, digits = 0)

clean_cen$bike_score <- round(clean_cen$bike_score, digits = 0)

# Linear Regression of Walking Population and Walk Score
mod_walk <- lm(y21_walk ~ walk_score, data = census_data)

##fix the coordinate system
utm_points = cbind("x" = crime$X, "y" = crime$Y)
utm_spacial_vector = vect(utm_points, crs="+proj=utm +zone=10 +datum=WGS84  +units=m")
latlon_spacial_vector = project(utm_spacial_vector, "+proj=longlat +datum=WGS84")
subset_idx = seq_len(nrow(crime))
crime_latlon = geom(latlon_spacial_vector)[subset_idx, c("x", "y")]
crime = cbind(crime, crime_latlon)

##select just traffic related incidents 
crash <- crime %>%
  select(TYPE, YEAR, MONTH, NEIGHBOURHOOD, x, y) %>%
  filter(TYPE %in% c("Vehicle Collision or Pedestrian Struck (with Injury)", "Vehicle Collision or Pedestrian Struck (with Fatality)"))

##get a count 
CollisionCount <- crash %>%
  group_by(YEAR) %>%
  count(TYPE)

CollisionCount <- left_join(CollisionCount, pop, by = c("YEAR" = "Year"))

CollisionCount <- CollisionCount %>%
  mutate(per_capita = n / Population * 100000)

clean_cen$y21_total_commute = census_data$y21_total_commute

clean_cen <- clean_cen %>%
  mutate(walk_prop = (y21_walk / y21_total_commute) * 100)
