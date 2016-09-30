#' Calculate the voxels for all pulses in a spectrogram
#' @param pulses a batPulse object
#' @importFrom assertthat assert_that
#' @importFrom methods validObject new
#' @importFrom dplyr %>% mutate_ select_ rowwise do_ mutate
#' @importFrom tidyr unnest_
#' @export
voxel_pulse <- function(pulses){
  resolution <- 1 / c(0.5, 0.5)
  assert_that(inherits(pulses, "batPulse"))
  validObject(pulses)

  if (length(pulses@Spectrogram) != 1) {
    stop("voxel_pulse() handles only one spectrogram at a time.")
  }

  if (nrow(pulses@PulseMetadata) == 0) {
    return(pulses)
  }

  spectrogram.raster <- spectrogram_raster(
    pulses@Spectrogram[[1]]
  )
  voxel <- pulses %>%
    calculate_box() %>%
    rowwise() %>%
    do_(
      Voxel = ~interpolate_voxel(
        spectrogram.raster = spectrogram.raster,
        this.box = .
      )
    ) %>%
    unnest_(unnest_cols = "Voxel") %>%
    mutate_(Fingerprint = ~factor(Fingerprint)) %>%
    as.data.frame()
  pulses@PulseVoxel <- voxel
  return(pulses)
}

#' Calculate the voxel for an individual pulse
#' @param spectrogram.raster a spectrogram raster
#' @param this.box the bounding box of the pulse
#' @importFrom dplyr %>% mutate_ count_
#' @importFrom raster extent crop raster resample clump extract Which
#' @importMethodsFrom raster as.data.frame
#' @export
interpolate_voxel <- function(spectrogram.raster, this.box){
  resolution <- c(0.5, 0.5)
  dB.range <- this.box$MaxAmplitude - this.box$DeltaAmplitude

  spectrogram <- this.box[c("Xmin", "Xmax", "Ymin", "Ymax")] %>%
    unlist() %>%
    '+'(c(-2, 2, -2, 2)) %>%
    pmin(
      rep(
        c(extent(spectrogram.raster)@xmax, extent(spectrogram.raster)@ymax),
        each = 2
      )
    ) %>%
    pmax(
      rep(
        c(extent(spectrogram.raster)@xmin, extent(spectrogram.raster)@ymin),
        each = 2
      )
    ) %>%
    extent() %>%
    crop(x = spectrogram.raster)
  detail.raster <- raster(
    xmn = this.box$BXmin,
    xmx = this.box$BXmax,
    ymn = this.box$BYmin,
    ymx = this.box$BYmax,
    resolution = resolution / 10
  ) %>%
    resample(x = spectrogram)
  candidate.zone <- clump(
    detail.raster >= dB.range,
    directions = 4
  )
  relevant <- this.box[c("Xpeak", "Ypeak")] %>%
    unlist() %>%
    matrix(nrow = 1) %>%
    extract(x = candidate.zone) %>%
    '=='(candidate.zone) %>%
    Which(cells = TRUE)

  detail.raster %>%
    as.data.frame(xy = TRUE) %>%
    "["(relevant, ) %>%
    mutate_(
      x = ~floor((x - this.box$BXmin) / resolution[1]),
      y = ~floor((y - this.box$BYmin) / resolution[2]),
      dB = ~dB - dB.range,
      dB = ~floor(dB * this.box$S / this.box$DeltaAmplitude)
    ) %>%
    count_(vars = c("x", "y", "dB")) %>%
    mutate_(Fingerprint = ~this.box$Fingerprint)
}
