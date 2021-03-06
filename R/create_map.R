#' Create spacial heat map
#'
#' @description Create a spacial heat map using layered on top of an Open Street Map. The heat map is generated using spacial boundaries such as ZCTAs for the US or
#'
#' @param data Dataset to use for plot. Must be a data.frame with two mandatory columns: zipcode and bins.
#' @param shape_boundaries Dataset containing the spacial representation of geographic areas(ZIP Code Tabulation Area). Dataset is retrieved as followed: shape_zcta<-load_shape_us_zcta(shape_us_zcta_dir, shape_us_zcta_filename).
#' @param shape_roads Dataset containing the shapes of the principal roads.
#' @param boundaries_coords Dataset with the ZCTA code name and the associated (Lat,Long) coordinates.
#' @param show_boundaries_label Display the boundaries labels on the map(ex: zip codes).
#' @param show_boundaries_label_dist Distance within which only one out of two labels will be shown.
#' @param map_center Coordinates(Latitude/Longitude) of the map center to be drawn. Must be a data.frame with two mandatory columns: lat and long.
#' @param dist_center Distance defining the map bounding box. The height(distance top-bot) of the map is equal to 2 times dist_center.
#' @param locations Dataset containing a list of locations to be plotted on the map. Each location is defined by Lat/Long Coordinates and a label.
#' Must be a data.frame with three mandatory columns: name, lat, and long.
#' @param legend_title Metric name that is being mapped.
#' @param zoom Map zoom level used by openstreetmap. If null, it is determined automatically. map zoom, an integer from 0 (whole world) to 19 (building).
#' Recommended values: If dist_center = 25miles, zoom=11. If dist_center = 50miles, zoom=10
#' see http://wiki.openstreetmap.org/wiki/Zoom_levels
#' @param type the tile server from which to get the map, or the url pattern. type = c("osm", "osm-bw","maptoolkit-topo",
#' "waze", "bing", "stamen-toner", "stamen-terrain","stamen-watercolor", "osm-german", "osm-wanderreitkarte", "mapbox",
#' "esri","esri-topo", "nps", "apple-iphoto", "skobbler", "hillshade", "opencyclemap","osm-transport", "osm-public-transport", "osm-bbike", "osm-bbike-german")
#' @param color_boundaries Color of the boundaries(shape_boundaries). RGB('#xxxxxx). http://colorbrewer2.org or http://sape.inf.usi.ch/quick-reference/ggplot2/colour. Set to NULL to not show.
#' @param color_bins List of colors used in the color scale bar. if N bins are used, then N colors should be defined. As default 6 bins so 6 colors are used.
#' @param color_roads Color of the roads(shape_roads).
#'
#' @return a ggplot object
#' @export
#'
#'
#' @examples
#' #USA ZCTA
#' Download shapefile(.shp) from http://www2.census.gov/geo/tiger/GENZ2015/shp/cb_2015_us_zcta510_500k.zip
#' unzip cb_2015_us_zcta510_500k.zip in C:/SHAPEFILES/ZCTA
#' shape_dir = "C:/SHAPEFILES/ZCTA"
#' shape_filename = "cb_2015_us_zcta510_500k"
#' identifier = "ZCTA5CE10"
#' shape_boundaries<-shapefile_to_df(shape_dir, shape_filename,identifier)
#'
#' #USA Primary Roads
#' Download shapefile(.shp) from http://www2.census.gov/geo/tiger/GENZ2015/shp/cb_2015_us_zcta510_500k.zip
#' unzip tl_2016_us_primaryroads.zip in C:/SHAPEFILES/us_primaryroads
#' shape_dir = "C:/SHAPEFILES/us_primaryroads"
#' shape_filename = "tl_2016_us_primaryroads"
#' identifier = "FULLNAME"
#' shape_roads<-shapefile_to_df(shape_dir, shape_filename,identifier)
#'
#' locations = data.frame(name=c("Children's Mercy Park"), long = c(-94.823686), lat = c(39.121296) )
#' map_center = data.frame(long = c(-94.823686), lat = c(39.121296))
#' boundaries_coords<-boundaries_coords.shape(shape_boundaries, map_center)
#' boundaries_coords100<-boundaries_coords[boundaries_coords$dist_to_center<100,]
#' data = data.frame(id = boundaries_coords100$id, value = sample(100, size = nrow(boundaries_coords100), replace = TRUE))
#' data$bin <- create_bins(data$value,nbins=6)
#'
#' dist_center<-25
#' legend_title<-'random value'
#' zoom<-10
#' type = 'osm'
#' color_bins<-c("#ececec","#fcc5c0","#fa9fb5","#f768a1","#c51b8a","#7a0177")
#' p<-create_map(data=data, shape_boundaries=shape_boundaries, shape_roads=NULL,boundaries_coords=boundaries_coords,  show_boundaries_label=TRUE, map_center,  dist_center, locations, legend_title, zoom, type, color_boundaries = 'grey100', color_bins = color_bins, color_roads = 'chartreuse')
#' p

#' @importFrom geosphere destPoint mercator
#' @importFrom dplyr inner_join
#' @importFrom OpenStreetMap openmap openproj autoplot.OpenStreetMap
#' @import ggplot2
#'
create_map<- function(data, shape_boundaries, shape_roads,boundaries_coords, show_boundaries_label=TRUE, show_boundaries_label_dist=1.5,map_center,  dist_center, locations, legend_title, zoom=10, type = 'osm', color_boundaries = 'black',color_bins = c("#ececec","#fcc5c0","#fa9fb5","#f768a1","#c51b8a","#7a0177"), color_roads = 'steelblue4'){

  if(!"id" %in% colnames(data)) print('ERROR: data doesnt have a column id')
  if(!"bin" %in% colnames(data)) print('ERROR: data doesnt have a column bin')
  if(!"id" %in% colnames(shape_boundaries)) print('ERROR: shape_boundaries doesnt have a column id')
  if(!"id" %in% colnames(boundaries_coords)) print('ERROR: boundaries_coords doesnt have a column id')


  # define the canvas and bounding box
  print('define the canvas and bounding box')
  dist_lat = dist_center/0.000621371
  dist_long = dist_center*(1.5)/0.000621371
  dist_canvas = dist_long + 50 #(dist_center+50)/0.000621371

  bb_lat_top=destPoint(p = map_center, b = 0,  d = dist_lat)[2]
  bb_lat_bot=destPoint(p = map_center, b = 0,  d = -dist_lat)[2]
  bb_long_right=destPoint(p = map_center, b = 90,  d = dist_long)[1]
  bb_long_left=destPoint(p = map_center, b = 90,  d = -dist_long)[1]

  c_lat_top=destPoint(p = map_center, b = 0,  d = dist_canvas)[2]
  c_lat_bot=destPoint(p = map_center, b = 0,  d = -dist_canvas)[2]
  c_long_right=destPoint(p = map_center, b = 90,  d = dist_canvas)[1]
  c_long_left=destPoint(p = map_center, b = 90,  d = -dist_canvas)[1]

  bb_mer_lat_top = mercator(c(bb_long_left,bb_lat_top))[2]
  bb_mer_lat_bot = mercator(c(bb_long_left,bb_lat_bot))[2]
  bb_mer_long_right = mercator(c(bb_long_right,bb_lat_top))[1]
  bb_mer_long_left = mercator(c(bb_long_left,bb_lat_top))[1]

  c_mer_lat_top = mercator(c(c_long_left,c_lat_top))[2]
  c_mer_lat_bot = mercator(c(c_long_left,c_lat_bot))[2]
  c_mer_long_right = mercator(c(c_long_right,c_lat_top))[1]
  c_mer_long_left = mercator(c(c_long_left,c_lat_top))[1]

  # apply canvas filters to boundary shape data
  data_boundaries <- inner_join(data, boundaries_coords,c("id"))
  if(nrow(data_boundaries)==0) print('ERROR: No match found between data and boundaries_coords. Check the column id values')
  data_boundaries <-data_boundaries[data_boundaries$dist_to_center<dist_canvas,]
  data_boundaries$lat<-NULL
  data_boundaries$long<-NULL
  data_boundaries <- inner_join(data_boundaries, shape_boundaries,c("id"))
  if(nrow(data_boundaries)==0) print('ERROR: No match found between data and shape_boundaries Check the column id values')

  # get the open street map and transform the map from the lat-lont projection to the mercator projection
  print('open source map')
  mp <- openmap(c(bb_lat_top,bb_long_left), c(bb_lat_bot,bb_long_right), type = type, zoom = zoom)
  map_longlat <- openproj(mp,projection = osm())

  # draw the map using ggplot2
  p <-autoplot(map_longlat)

  #draw the boundaries polygons
  print('draw the boundaries')
  if(!is.null(color_boundaries))
    p = p+ geom_polygon(data = data_boundaries, aes_string(x = 'long', y = 'lat', group = 'group', fill = 'bin'),color=color_boundaries,size=0.05, alpha=0.7)
  else
    p = p+ geom_polygon(data = data_boundaries, aes_string(x = 'long', y = 'lat', group = 'group', fill = 'bin'),size=0.05, alpha=0.7)

  if(!is.null(shape_roads))
  {
    print('draw the roads')
    shape_roads.cv = shape_roads[shape_roads$long>=c_mer_long_left & shape_roads$long<=c_mer_long_right & shape_roads$lat>=c_mer_lat_bot & shape_roads$lat<=c_mer_lat_top,]
    # draw the roads shapes
    p = p+ geom_path(data=shape_roads.cv,size=0.5, aes(x=long,y=lat,group=group),color=color_roads)
  }

  #http://sape.inf.usi.ch/quick-reference/ggplot2/colour
  #color_bins = c("#ececec","#fcc5c0","#fa9fb5","#f768a1","#c51b8a","#7a0177")
  p = p+ scale_fill_manual(values = color_bins)
  p = p+ guides(colour = guide_legend(order = 1, title = NULL,keyheight = 0.6,keywidth = 0.6,
                                      label.theme = element_text(size = 8, face = "bold", hjust=1, angle = 0)),
                fill = guide_legend(order = 2,title = legend_title, reverse = TRUE, keyheight = 0.6,keywidth = 0.6,
                                    title.theme = element_text(size = 8,face = "bold", angle = 0),
                                    label.theme = element_text(size = 8,hjust=1, angle = 0)))
  if(nrow(locations)>0){

    print('draw the locations')

    mer = apply(locations,1,function(x){
      lat=as.numeric(x['lat']);
      long=as.numeric(x['long']);
      mer<-mercator(c(long,lat))
    })
    locations$mer_lat = mer[2,]
    locations$mer_long = mer[1,]
    p = p+ geom_point(data = locations, aes(x = mer_long, y = mer_lat, color = name), alpha = 1, fill = "#FFFF00", pch = 21, size = 3,stroke = 1)
  }
  # apply the bounding box
  print('apply the bounding box')
  p = p+ coord_fixed(xlim = c(bb_mer_long_left, bb_mer_long_right),  ylim = c(bb_mer_lat_bot, bb_mer_lat_top))

  # define the plot theme
  p = p+ labs(x=NULL,y=NULL)
  p = p+ theme(axis.text=element_blank(),axis.ticks=element_blank(),
               legend.key = element_rect(colour = "transparent", fill = "transparent"),
               plot.margin=unit(c(0,0,0,0),"pt"),
               panel.background = element_rect(fill = "transparent",colour = NA),
               plot.background = element_rect(fill = "transparent",colour = NA),
               legend.background = element_rect(fill = "transparent",colour = NA))


  if(show_boundaries_label == TRUE){
    print('draw the boundaries labels')

    boundaries.cv = boundaries_coords[boundaries_coords$mer_long>=c_mer_long_left & boundaries_coords$mer_long<=c_mer_long_right & boundaries_coords$mer_lat>=c_mer_lat_bot & boundaries_coords$mer_lat<=c_mer_lat_top,]
    # draw the boundaries labels: boundaries_labels
    boundaries.labels = boundaries_coords_filter(boundaries.cv, show_boundaries_label_dist)
    # draw the boundaries labels: geom_text
    p = p+ geom_text(data=boundaries.labels, aes(mer_long, mer_lat, label = id), size=2)
  }
  return (p)
  #path<-paste(directory, mapname, ".png", sep = "")
  #ggsave(path, plot = p, device = "png", width = 10, height = 6.5, bg = "transparent")
}

