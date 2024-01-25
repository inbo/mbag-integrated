calc_parcel_size <- function(layer) {
  require(dplyr)
  require(sf)

  parcels_grouped <- layer %>%
    filter(!is.na(.data$GWSNAM_H)) %>%

    # Join neighbouring polygons by main crop
    group_by(.data$GWSNAM_H, .data$GWSGRPH_LB) %>%
    summarise() %>%
    st_cast("MULTIPOLYGON") %>%
    st_cast("POLYGON") %>%
    ungroup()

  out_sf <- parcels_grouped %>%
    # Give new IDs
    tibble::rownames_to_column(var = "ID") %>%

    # Calculate area
    mutate(area_m2 = st_area(.data$geometry),
           area_m2 = units::drop_units(.data$area_m2),
           area_ha = .data$area_m2 / 10000)

  return(out_sf)
}
