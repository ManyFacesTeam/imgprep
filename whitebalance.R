library(magick)

# Get list of all PNG files in the current working directory
files <- list.files("manyfaces/small", pattern = "\\.jpg$", full.names = TRUE)
file <- files[[10]]
for (file in files) {
  image <- image_read(file)  # Read image

  # Convert image to raw RGB array
  img_data_rgb <- image_data(image, "rgb")
  #img_data <- img_data_rgb |> apply(2:3, as.integer)
  img_data <- as.integer(img_data_rgb) |> aperm(c(3,2,1))

  # Extract a 25x25 patch from the top-right corner
  width <- dim(img_data)[2]
  patch <- img_data[, (width - 24):width, 1:25]

  # Compute mean R, G, B values
  meanRGB <- round(apply(patch, 1, mean))
  white <- round(mean(meanRGB))  # Estimate 'white' value
  # white <- 200

  # Compute white balance multipliers
  whiteMult <- white / meanRGB

  # Apply white balance correction
  img_data_wb <- img_data
  img_data_wb[1,,] <- img_data[1,,] * whiteMult[1]  # Adjust R channel
  img_data_wb[2,,] <- img_data[2,,] * whiteMult[2]  # Adjust G channel
  img_data_wb[3,,] <- img_data[3,,] * whiteMult[3]  # Adjust B channel

  # Ensure values stay within range
  img_data_wb_hex <- pmin(pmax(img_data_wb, 0), 255) |>
    round() |> apply(2:3, as.raw)

  # Convert back to image and save
  image_wb <- image_read(img_data_wb_hex)
  image_write(image_wb, sub("small", "wb", file))
}
