#' Calculate the fast Fourier transformation for each pulse
#' @export
#' @importFrom assertthat assert_that is.count
#' @param pulses A list of pulses. E.g. the output of \code{\link{find_pulses}}
#' @param n.fourier The number of required Fourier components in each direction.
#' @examples
#' wav <- read_wav(
#'   system.file("demo_wav/leislers.wav", package = "rhinolophus")
#' )
#' spectrogram <- wav2spectrogram(wav)
#' pulses <- find_pulses(spectrogram = spectrogram)
#' pulse.fft <- fft_pulse(pulses = pulses, n.fourier = 10)
fft_pulse <- function(pulses, n.fourier = 30){
  assert_that(inherits(pulses, what = "batPulse"))
  assert_that(is.count(n.fourier))

  resolution <- 1 / c(0.5, 0.5)

  if (length(pulses@Spectrogram) != 1) {
    stop("fft_pulse() handles only one spectrogram at a time.")
  }

  if (nrow(pulses@PulseMetadata) == 0) {
    return(pulses)
  }

  box <- pulses@PulseMetadata %>%
    mutate_(
      S = ~(Xmax - Xmin) * resolution[1],
      S = ~pmin(next_power_2(S), 256),
      SY = ~ ceiling(Ymax * resolution[2] / S) - floor(Ymin * resolution[2] / S)
    )
  while (any(box$SY > 1)) {
    box <- box %>%
      mutate_(
        S = ~ifelse(SY == 1, S, S * 2),
        SY = ~ ceiling(Ymax * resolution[2] / S) - floor(Ymin * resolution[2] / S)
      )
  }
  box <- box %>%
    mutate_(
      BXmin = ~Xpeak - ((Xpeak - Xmin) / (Xmax - Xmin)) * S / resolution[1],
      BXmax = ~BXmin + S / resolution[1],
      BYmin = ~floor(Ymin * resolution[2] / S) * S / resolution[2],
      BYmax = ~ceiling(Ymax * resolution[2] / S) * S / resolution[2]
    ) %>%
    select_(~-SY)

  spectrogram.raster <- spectrogram_raster(
    pulses@Spectrogram[[1]]
  )
  fft.pulse <- box %>%
    rowwise() %>%
    do_(
      Fft = ~interpolate_fft(
        spectrogram.raster = spectrogram.raster,
        this.box = .,
        n.fourier = n.fourier
      )
    ) %>%
    as.list() %>%
    '[['(1)
  names(fft.pulse) <- box$Fingerprint
  pulses@PulseFourier <- fft.pulse
  return(pulses)
}

#' Calculate the fast Fourier transformation of an indivual pulse
#' @importFrom dplyr %>%
#' @importFrom raster extent crop raster resample clump extract Which mask
#' @importMethodsFrom raster as.matrix
#' @importFrom stats fft
#' @export
interpolate_fft <- function(spectrogram.raster, this.box, n.fourier){
  resolution <- c(0.5, 0.5)
  dB.range <- this.box$MaxAmplitude - this.box$DeltaAmplitude

  spectrogram <- this.box[c("Xmin", "Xmax", "Ymin", "Ymax")] %>%
    unlist() %>%
    extent() %>%
    crop(x = spectrogram.raster)
  detail.raster <- raster(
    xmn = this.box$BXmin,
    xmx = this.box$BXmax,
    ymn = this.box$BYmin,
    ymx = this.box$BYmax,
    resolution = resolution
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
    '!='(candidate.zone) %>%
    Which(cells = TRUE)
  candidate.zone[relevant] <- NA
  pulse.fft <- mask(
    detail.raster,
    candidate.zone,
    updatevalue = dB.range,
    updateNA = TRUE
  ) %>%
    '-'(dB.range) %>%
    '/'(this.box$DeltaAmplitude) %>%
    as.matrix() %>%
    fft()
  select.dim <- pmin(n.fourier, dim(pulse.fft))
  pulse.fft[
    seq_len(select.dim[1]),
    seq_len(select.dim[2])
  ]
}
