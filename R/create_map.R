#' Add together two numbers.
#'
#' @param data Dataset to use for plot. Must be a data.frame with two mandatory columns: zipcode and bins.
#' @param shape_boundaries Dataset containing the spacial representation of geographic areas(ZIP Code Tabulation Area). Dataset is retrieved as followed: shape_zcta<-load_shape_us_zcta(shape_us_zcta_dir, shape_us_zcta_filename).
#' @param shape_roads Dataset containing the shapes of the principal roads.
#' @param boundaries_coords Dataset with the ZCTA code name and the associated (Lat,Long) coordinates.
#' @param map_center Coordinates(Latitude/Longitude) of the map center to be drawn. Must be a data.frame with two mandatory columns: lat and long.
#' @param dist_center Distance defining the map bounding box. The height(distance top-bot) of the map is equal to 2 times dist_center.
#' @param locations Dataset containing a list of locations to be plotted on the map. Each location is defined by Lat/Long Coordinates and a label.
#' Must be a data.frame with three mandatory columns: name, lat, and long.
#' @param legend_title Metric name that is being mapped.
#' @param zoom Map zoom level used by openstreetmap. If null, it is determined automatically. map zoom, an integer from 0 (whole world) to 19 (building).
#' Recommended values: If dist_center = 25miles, zoom=11. If dist_center = 50miles, zoom=10
#' see http://wiki.openstreetmap.org/wiki/Zoom_levels
#' @param color_bins List of colors used in the color scale bar. if N bins are used, then N colors should be defined. As default 6 bins so 6 colors are used.
#' @param show_boundaries_label Display the boundaries labels on the map(ex: zip codes).
#' @return a heat map ggplot.
#' @examples
#' @export
create_map<- function(data, shape_boundaries, shape_roads,boundaries_coords, map_center,  dist_center, locations, legend_title, zoom=NULL, color_bins = c("#ececec","#fcc5c0","#fa9fb5","#f768a1","#c51b8a","#7a0177"), show_boundaries_label=TRUE){

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

  print('apply canvas filters to boundary shape data')

  data_boundaries <- inner_join(data, boundaries_coords,c("zipcode" = "code"))
  data_boundaries <-data_boundaries[data_boundaries$dist_to_center<dist_canvas,]
  data_boundaries$lat<-NULL
  data_boundaries$long<-NULL
  data_boundaries <- inner_join(data_boundaries, shape_boundaries,c("zipcode" = "id"))



  if(shape_roads)
  data_roads = shape_roads[shape_roads$long>=c_mer_long_left & shape_roads$long<=c_mer_long_right & shape_roads$lat>=c_mer_lat_bot & shape_roads$lat<=c_mer_lat_top,]

  #zoom<-10
  print('get the open street map and transform the map from the lat-lont projection to the mercator projection')

  mp <- openmap(c(bb_lat_top,bb_long_left), c(bb_lat_bot,bb_long_right), type="maptoolkit-topo", zoom = zoom)
  map_longlat <- openproj(mp,projection = osm())

  print('draw the map using ggplot2')

  p <-autoplot(map_longlat)

  print('draw the boundaries polygons')

  p = p+ geom_polygon(data = data_boundaries, aes_string(x = 'long', y = 'lat', group = 'group', fill = 'bin'),color='black',size=0.05, alpha=0.7)


  if(shape_roads)
  {
    print('apply canvas filters to ROADS shape data')
    data_roads = shape_roads[shape_roads$long>=c_mer_long_left & shape_roads$long<=c_mer_long_right & shape_roads$lat>=c_mer_lat_bot & shape_roads$lat<=c_mer_lat_top,]
    print('draw the roads shapes')
    p = p+ geom_path(data=data_roads,size=0.5, aes(x=long,y=lat,group=group),color="steelblue4")
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
    rm(mer)
    p = p+ geom_point(data = locations, aes(x = mer_long, y = mer_lat, color = name), alpha = 1, fill = "white", pch = 21, size = 3,stroke = 1)
  }

  print('apply the bounding box')

  p = p+ coord_fixed(xlim = c(bb_mer_long_left, bb_mer_long_right),  ylim = c(bb_mer_lat_bot, bb_mer_lat_top))

  print('define the plot theme')

  p = p+ labs(x=NULL,y=NULL)
  p = p+ theme(axis.text=element_blank(),axis.ticks=element_blank(),
               legend.key = element_rect(colour = "transparent", fill = "transparent"),
               plot.margin=unit(c(0,0,0,0),"pt"),
               panel.background = element_rect(fill = "transparent",colour = NA),
               plot.background = element_rect(fill = "transparent",colour = NA),
               legend.background = element_rect(fill = "transparent",colour = NA))

  if(show_boundaries_label){

    print('draw the boundaries labels')

    c_boundaries = boundaries_coords[boundaries_coords$mer_long>=c_mer_long_left & boundaries_coords$mer_long<=c_mer_long_right & boundaries_coords$mer_lat>=c_mer_lat_bot & zcboundaries_coordsta$mer_lat<=c_mer_lat_top,]
    boundaries_names = us_zcta_names(c_boundaries)
    p = p+ geom_text(data=boundaries_names, aes(mer_long, mer_lat, label = code), size=2)
  }
  print('done')
  return (p)
  #path<-paste(directory, mapname, ".png", sep = "")
  #ggsave(path, plot = p, device = "png", width = 10, height = 6.5, bg = "transparent")
}

