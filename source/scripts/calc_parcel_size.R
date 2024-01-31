calc_parcel_size <- function(layer) {
  stopifnot(requireNamespace("tidyverse", quietly = TRUE))
  stopifnot(requireNamespace("sf", quietly = TRUE))
  stopifnot(requireNamespace("units", quietly = TRUE))
  require("tidyverse")
  require("sf")
  require("units")

  parcels_grouped <- layer |>
    filter(!is.na("GWSNAM_H")) |>

    # Join neighbouring polygons by main crop
    dplyr::group_by("GWSNAM_H", "GWSGRPH_LB") |>
    dplyr::summarise() |>
    sf::st_cast("MULTIPOLYGON") |>
    sf::st_cast("POLYGON") |>
    dplyr::ungroup()

  out_sf <- parcels_grouped |>
    # Give new IDs
    tibble::rownames_to_column(var = "ID") |>

    # Calculate area
    dplyr::mutate(
      area_m2 = sf::st_area("geometry"),
      area_m2 = units::drop_units("area_m2"),
      area_ha = "area_m2" / 10000)

  return(out_sf)
}
