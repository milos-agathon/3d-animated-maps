install.packages(
    "pacman"
)

pacman::p_load(
    rsi, terra, sf, rayshader, abind,
    glue, av, lubridate
) # add lubridate

# 2. AREA OF INTEREST
#--------------------

# 38.8758359,-104.9231662

colorado <- sf::st_point(
    rev(c(
        38.8758359, -104.9231662
    ))
)

colorado_sf <- sf::st_sfc(
    colorado,
    crs = "EPSG:4326"
)

aoi <- sf::st_buffer(
    sf::st_transform(
        colorado_sf,
        crs = "EPSG:8857"
    ), 4000 # 4 kilometers
)

# 3. DOWNLOAD DATA
#-----------------

year <- 2024

start_dates <- seq.Date(
    from = as.Date(
        glue(
            "{year}-01-01"
        )
    ),
    to = as.Date(
        glue(
            "{year}-05-31"
        )
    ),
    by = "10 day"
)

end_dates <- start_dates + 10

main_dir <- getwd()

downloaded_weeks <- vapply(
    seq_along(start_dates),
    function(week) {
        start_date <- as.character(
            start_dates[week]
        )
        end_date <- as.character(
            end_dates[week]
        )

        file_date <- as.Date(start_date)
        output_filename <- file.path(
            main_dir, glue(
                "{year}-{sprintf('%02d', month(file_date))}-{sprintf('%02d', day(file_date))}.tif"
            )
        )
        tryCatch(
            {
                rsi::get_sentinel2_imagery(
                    aoi = aoi,
                    start_date = start_date,
                    end_date = end_date,
                    output_filename = output_filename,
                    mask_function = sentinel2_mask_function # no brackets
                )
                message(
                    glue(
                        "Downloaded data for period {week}({start_date} to {end_date})"
                    )
                )
                output_filename
            },
            error = function(e) {
                message(
                    glue(
                        "Failed to download data for period {week}({start_date} to {end_date}): {e$message}"
                    )
                )
                NA
            }
        )
    }, character(1)
)

rast <- terra::rast("2024-05-10.tif") #downloaded_weeks[[16]])

terra::plotRGB(
    rast,
    r = 4, g = 3, b = 2,
    stretch = "lin",
    smooth = TRUE
)

# 4. DEM DATA
#-----------------

dem <- rsi::get_dem(
    aoi = aoi,
    pixel_x_size = 10,
    pixel_y_size = 10,
    output_filename = "dem.tif"
)

dem_rast <- terra::rast(dem)

terra::plot(dem_rast)

elmat <- rayshader::raster_to_matrix(dem_rast)

# 5. RENDER 3D SNAPSHOTS
#-----------------------

# Helper functions

normalize_channel <- function(channel){ #channel
    (channel - min(channel, na.rm = TRUE)) / (max(channel, na.rm = TRUE) - min(channel, na.rm = TRUE))
}

adjust_colors <- function(
    r, g, b, gamma_values, desaturation_factor) {
    r <- r^gamma_values[1]
    g <- g^gamma_values[2]
    b <- b^gamma_values[3] #b

    gray <- (r + g + b) / 3
    r <- gray * (1 - desaturation_factor) + r * desaturation_factor
    g <- gray * (1 - desaturation_factor) + g * desaturation_factor
    b <- gray * (1 - desaturation_factor) + b * desaturation_factor

    list(r, g, b)
}

factor <- 0.4
theta <- -15
phi <- 30
zoom <- 0.88

for(i in seq_along(downloaded_weeks)) {
    file <- downloaded_weeks[i]
    fout <- file.path(
        main_dir, sprintf(
            "frame_%04d.png", i
        )
    )
    print(
        paste(
            "Processed: ", basename(file)
        )
    )

    img <- terra::rast(file)[[2:4]]
    r <- normalize_channel(terra::values(img[[3]]))
    g <- normalize_channel(terra::values(img[[2]]))
    b <- normalize_channel(terra::values(img[[1]])) #1

    adjusted <- adjust_colors(
        r, g, b,
        gamma_values = c(.6, .5, .8),
        desaturation_factor = factor
    )

    r <- adjusted[[1]]
    g <- adjusted[[2]]
    b <- adjusted[[3]]

    col <- abind::abind(
        t(
            matrix(
                r, nrow = terra::nrow(
                    img
                )
            )
        ),
        t(
            matrix(
                g, nrow = terra::nrow(
                    img
                )
            )
        ),
        t(
            matrix(
                b, nrow = terra::nrow(
                    img
                )
            )
        ), along = 3
    )

    elmat |>
        rayshader::height_shade(
            texture = colorRampPalette(
                "white"
            )(256)
        ) |>
        rayshader::add_overlay(
            col, alphalayer = 1
        ) |>
        rayshader::plot_3d(
            elmat,
            solid = FALSE,
            shadow = FALSE,
            zscale = 10,
            theta = theta,
            phi = phi,
            zoom = zoom, 
            window = c(800, 800)
        )

        rayshader::render_snapshot(
            fout,
            clear = TRUE,
            title_text = gsub(
                ".tif", "", basename(file)
            ),
            title_color = "black",
            title_font = "Helvetica",
            gravity = "NorthEast",
            offset = c(20, 50)
        )
}

# 6. MAKE VIDEO
#--------------

video_file <- file.path(
    main_dir, "sentinel2-colorado.mp4"
)

av::av_encode_video(
    dir(main_dir, full.names = TRUE, 
    pattern = "frame"),
    output = video_file,
    framerate = 2
)
