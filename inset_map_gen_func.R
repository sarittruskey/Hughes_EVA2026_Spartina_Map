# =============================================================================
# inset_map_gen_func.R
# Generalized function for plotting inset maps showing study site locations
# 
# Author: Sarit Truskey
# Last updated: 2026-02-19
#
# Description:
#   Contains inset_map_gen_func(), a generalized function for plotting zoomed-in inset maps. 
#   This function takes all data and aesthetic inputs as arguments rather than 
#   relying on objects in the global environment, making it reusable for any 
#   region for which you have shapefiles and any set of sites you have coordinates for!
#
# Dependencies:
#   ggplot2, sf, ggspatial
#   (these should already be loaded if running map_Hughes_EVA2026.Rmd or you can make sure to 
#   copy the library chunk to a new script)
#
# Usage:
#   To make this function "usable", source this file at the top of your script or Rmd via:
#   source("inset_map_gen_func.R")
#   Then you can call inset_map_gen_func() with your data and aesthetics in your script.
#   See example usage in map_Hughes_EVA2026.Rmd.
# =============================================================================

inset_map_gen_func <- function(
    long_range, # variable for longitude range (map will be cropped to these longitudes)
    lat_range, # variable for latitude range (map will be cropped to these latitudes)
    pts_sf, # the sf object you coverted your set locations data frame into just above with st_as_sf()
    outline_poly, # polygon shapefile to fill land
    outline_arc, # arc shapefile to use for coast outline
    shape_col, # name of column in pts_sf that determines shape (expects a string, "site_shape" not site_shape)
    shape_vals, # expects a vector of assigned shape values
    fill_col, # name of column in pts_sf that determines fill color
    fill_vals, # expects a vector of assigned fill values
    fill_breaks = names(fill_vals), # you can give the order you want your fill colors to appear in the legend here; defaults to order of fill_vals if not provided
    fill_name, # name to use for fill legend as string, e.g. "Site"
    scale_bar_loc = "tl", # this is the area in the plot where the scale bar will be added; if not changed, will default to topleft via "tl"
    scale_text_cex = 1.5, # this is the text size for the scale bar, defaults to 1.5 unless changed in call
    point_size = 5, # this is the default size of the points used for subsites, unless changed in call
    point_alpha = 0.6, # this is the default alpha value for points
    border_linewidth = 2 # this is the weight of the border around the plot area
) {
  # body of function for plotting; will be similar to base map but adding a few extra things
  ggplot() +
    geom_sf(
      data = outline_poly, # polygon shapefile passed as argument
      fill = "grey85",
      color = NA
    ) +
    geom_sf(
      data = outline_arc, # arc shapefile with outlines passed as argument
      fill = NA,
      color = "black",
      linewidth = 0.2
    ) +
    geom_sf(
      data = pts_sf, # this is the sf object with the subsite datapoints
      aes(shape = .data[[shape_col]], # shape of point determined by the shape_col field you provide in calling function; Note: .data[[shape_col]] is a ggplot2 pattern for referencing a column by name when that name is stored as a string variable. When a column name is passed as an argument like shape_col = "site_shape", you need to direct ggplot to where to find it (i.e., .data[["site_shape"]], where .data is what you provided the argument `data =` earlier)
          fill = .data[[fill_col]]), # fill color of point determined by the fill_col field you provide in calling function
      size = point_size, # point_size variable in function call will change size of points or default to 5
      stroke = 0.9,
      color = "black",
      alpha = point_alpha # point_alpha variable in function call will change alpha for points or default to 0.6
    ) +
    coord_sf(
      xlim = long_range, # long_range variable in function call will manually set x range (longitude) for map
      ylim = lat_range,  # lat_range variable in function call will manually set y range (latitude) for map
      expand = FALSE
    ) +
    scale_shape_manual( # set shape values for points by site name (site_shape field)
      values = shape_vals, # shape values passed as argument
      guide = "none" # take out legend for shape
    ) +
    scale_fill_manual( # set fill values for points by site name (site_shape field values, in this instance)
      values = fill_vals,       # fill colors passed as argument
      breaks = fill_breaks,     # legend order passed as argument
      name   = fill_name
    ) +
    guides(
      fill = guide_legend(# customize fill legend
        override.aes = list(
          shape = shape_vals[fill_breaks], # match shapes to legend order
          color = "black",
          size  = 6 # making points a bit bigger in legend
        )
      )
    ) +
    annotation_scale( #add spatial scale bar
      location = scale_bar_loc, # where you want the scale bar to be placed; defaults to "tl" topleft but can be changed in calling function
      text_cex = scale_text_cex # size of text in scale bar; defaults to 1.5 unless another value provided
    ) +
    theme(
      panel.background = element_rect(fill = "white"),
      panel.grid = element_line(color = "transparent"),
      axis.title = element_blank(), #removes axes title; if you want to keep axes labels, remove or comment out this line
      axis.text = element_blank(), # removes text for axes; if you want to keep axes text, remove or comment out this line
      axis.ticks = element_blank(), # removes ticks for axes; if you want to keep axes ticks, remove or comment out this line
      text = element_text(size = 24, color = "black"), # default size for text in plot
      panel.border = element_rect(color = "black", fill = NA, linewidth = border_linewidth) # adds a rectangle border around plotted area
    )
}
