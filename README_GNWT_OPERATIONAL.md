# hydroclim: `gnwt-operational` branch

This branch is a **snapshot of the hydroclim code GNWT (ECC, NT Hydrology) runs operationally**, saved in October 2026. It is **not meant to be merged into `main`**, which has moved on separately.

GNWT workflows that need this branch rather than `main`:
- Transboundary reporting (https://github.com/EmmaGRiley/NWTHydroclimate/tree/main/Transboundary). It needs `hydro_calc_dayofyear_cumulative.R` and `hydro_plot_dayofyear_cumulative.R`, which are only on this branch.
- `Climate_Functions.R` / *R coding session – instructions.docx*

## Download and set up
1. On GitHub, switch the branch dropdown from **main** to **gnwt-operational**, then **Code → Download ZIP**.
2. Unzip and rename the folder to exactly `hydroclim`. Put it at `C:/Users/<you>/Documents/R_Scripts/Packages/hydroclim/`.
3. Install HYDAT: `install.packages("tidyhydat"); tidyhydat::download_hydat()`. Re-run `download_hydat()` once a year.

`hydro_map_basins.R` reads a CartoDB key from `.Renviron` (`CARTO_API_KEY`). It's only needed for that map.
