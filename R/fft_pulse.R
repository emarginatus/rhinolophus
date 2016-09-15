#' Calculate the fast fourier transformation for each pulse
#' @export
#' @importFrom sp bbox
#' @importFrom assertthat assert_that is.count has_name
#' @importFrom stats fft
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

  pulse.fft <- lapply(
    pulses@PulseMetadata$Fingerprint,
    function(fingerprint){
      pulse <- pulses@PulseMetadata %>%
        filter_(~Fingerprint == fingerprint) %>%
        select_(~Xmin, ~Xmax, ~Ymin, ~Ymax, ~Spectrogram)
      spectrogram <- pulses@Spectrogram[[pulse$Spectrogram]]
      select.t <- pulse$Xmin:pulse$Xmax
      select.f <- pulse$Ymin:pulse$Ymax
      selection <- spectrogram$S[select.f, select.t, drop = FALSE]

      pulse.fft <- fft(selection)
      select.dim <- pmin(n.fourier, dim(pulse.fft))
      pulse.fft[
        seq_len(select.dim[1]),
        seq_len(select.dim[2])
      ]
    }
  )
  names(pulse.fft) <- pulses@PulseMetadata$Fingerprint
  pulses@PulseFourier <- pulse.fft

  return(pulses)
}
