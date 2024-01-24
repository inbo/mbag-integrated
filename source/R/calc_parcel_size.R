calc_parcel_size <- function(layer) {
  require(dplyr)
  require(sf)

  parcels_grouped <- layer %>%
    filter(!is.na(GWSNAM_H)) %>%

    # Join neighbouring polygons by main crop
    group_by(GWSNAM_H, GWSGRPH_LB) %>%
    summarise() %>%
    st_cast("MULTIPOLYGON") %>%
    st_cast("POLYGON") %>%
    ungroup()

  out_sf <- parcels_grouped %>%
    # Give new IDs
    rownames_to_column(var = "ID") %>%

    # Calculate area
    mutate(area_m2 = st_area(geometry),
           area_m2 = units::drop_units(area_m2),
           area_ha = area_m2 / 10000)

  return(out_sf)
}
