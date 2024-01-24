calc_parcel_size <- function(year) {
  number_string <- sub(".*([0-9]{2})$", "\\1", year)

  # Load parcel file of year
  lbg_binding <- st_read(file.path(
      "data", "landbouwgebruikspercelen", "Shapefile",
      paste0("Lbgbrprc", number_string, ".shp"))) %>%
    st_transform(crs = 31370)

  if (year %in% 2022:2023) {
    lbg_binding <- lbg_binding %>%
      rename(LBLHFDTLT = GWSNAM_H,
             perc_id = REF_ID)
  } else {
    lbg_binding <- lbg_binding %>%
      rename(perc_id = OIDN)
  }

  # Intersection
  intersect <- st_intersection(punten_df_year %>% st_buffer(300), lbg_binding)

  out_df_year <- lbg_binding %>%
    filter(perc_id %in% intersect$perc_id) %>%       # Filter percelen
    full_join(st_drop_geometry(intersect)) %>%
    filter(!is.na(LBLHFDTLT)) %>%

    # Join neighbouring polygons by main crop
    group_by(pointid, LBLHFDTLT) %>%
    summarise() %>%
    st_cast("MULTIPOLYGON") %>%
    st_cast("POLYGON") %>%
    ungroup() %>%

    # Calculate median perceel area by point
    mutate(area = st_area(geometry)) %>%
    st_drop_geometry() %>%
    group_by(pointid) %>%
    summarise(perceel_median_area = units::drop_units(median(area)),
              perceel_iqr_area = IQR(area),
              perceel_cv_area = perceel_iqr_area / perceel_median_area,
              .groups = "drop")

  return(do.call(rbind.data.frame, out_list))
}
