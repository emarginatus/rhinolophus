#' Find the location of possible pulses in a spectrogram
#' @export
#' @importFrom raster raster Which rowColFromCell zonal clump subs
#' @importFrom assertthat assert_that is.number
#' @importFrom dplyr %>% filter_ rename_ mutate_ summarize_ select_ inner_join rowwise
#' @importFrom digest digest
#' @param spectrogram The spectrogram
#' @param min.contour The minimum amplitude of the pulse
#' @param min.peak Take only contour into account with a maximum amplitude of at least min.peak
#' @examples
#'  wav <- read_wav(
#'    system.file("demo_wav/leislers.wav", package = "rhinolophus")
#'  )
#'  spectrogram <- wav2spectrogram(wav)
#'  find_pulses(spectrogram)
find_pulses <- function(spectrogram, min.contour = 10, min.peak = 20){
  assert_that(is.number(min.contour))
  assert_that(is.number(min.peak))
  assert_that(min.peak > min.contour)
  assert_that(inherits(spectrogram, "batSpectrogram"))


  sha1 <- function(x){
    digest(x, algo = "sha1")
  }

  pulses <- do.call(
    rbind,
    lapply(
      names(spectrogram@Spectrogram),
      function(fingerprint){
        spec <- spectrogram@Spectrogram[[fingerprint]]
        spectrogram.raster <- raster(
          spec$S[rev(seq_len(nrow(spec$S))), ],
          xmn = min(spec$t),
          xmx = max(spec$t),
          ymn = min(spec$f),
          ymx = max(spec$f)
        )
        names(spectrogram.raster) <- "dB"

        minimum.contour <- clump(spectrogram.raster >= min.contour, directions = 4)
        local.max <- zonal(spectrogram.raster, minimum.contour, fun = max)
        recode <- local.max %>%
          as.data.frame() %>%
          filter_(~ value >= min.peak) %>%
          rename_(AmplitudeMax = ~value) %>%
          mutate_(Pulse = ~seq_along(zone))
        selected.pulses <- subs(minimum.contour, recode, by = "zone", which = "Pulse")

        pulses <- do.call(
          rbind,
          lapply(
            recode$Pulse,
            function(i){
              xy <- rowColFromCell(
                selected.pulses,
                Which(selected.pulses == i, cells = TRUE)
              )
              xy %>%
                as.data.frame() %>%
                summarize_(
                  Pulse = i,
                  Xmin = ~min(col),
                  Xmax = ~max(col),
                  Ymin = ~1L + nrow(spec$S) - max(row),
                  Ymax = ~1L + nrow(spec$S) - min(row),
                  Ratio = ~n() / (diff(range(col)) * diff(range(row))),
                  AmplitudeMin = min.contour
                )
            }
          )
        )
        recode %>%
          inner_join(x = pulses, by = "Pulse") %>%
          mutate_(
            Spectrogram = ~factor(fingerprint),
            DeltaTime = diff(head(spec$t, 2)),
            DeltaFrequency = diff(head(spec$f, 2))
          ) %>%
          filter_(~is.finite(Ratio)) %>%
          rowwise() %>%
          mutate_(Fingerprint = ~sha1(c(Spectrogram, Xmin, Xmax, Ymin, Ymax))) %>%
          select_(~-zone, ~-Pulse) %>%
          as.data.frame()
      }
    )
  )

  new(
    Class = "batPulse",
    Metadata = spectrogram@Metadata,
    SpectrogramMetadata = spectrogram@SpectrogramMetadata,
    Spectrogram = spectrogram@Spectrogram,
    PulseMetadata = pulses
  )
}
