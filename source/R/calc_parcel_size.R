calc_parcel_size <- function(layer) {
  require(dplyr)
  require(sf)

  out_df_year <- layer %>%
    filter(!is.na(GWSNAM_H)) %>%

    # Join neighbouring polygons by main crop
    group_by(GWSNAM_H) %>%
    summarise() %>%
    st_cast("MULTIPOLYGON") %>%
    st_cast("POLYGON") %>%
    ungroup() %>%

    # Calculate median perceel area by point
    mutate(area = st_area(geometry)) %>%
    st_drop_geometry() %>%
    group_by(REF_ID) %>%
    summarise(perceel_median_area = units::drop_units(median(area)),
              perceel_iqr_area = IQR(area),
              perceel_cv_area = perceel_iqr_area / perceel_median_area,
              .groups = "drop")

  return(do.call(rbind.data.frame, out_list))
}
