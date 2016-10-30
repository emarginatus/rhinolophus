#' Find the location of possible pulses in a spectrogram
#'
#' @export
#' @importFrom raster clump zonal Which xyFromCell crop focal extent
#' @importFrom assertthat assert_that is.number
#' @importFrom dplyr %>% filter_ rename_ mutate_ summarize_ select_ inner_join rowwise
#' @importFrom digest sha1
#' @importFrom methods new
#' @param spectrogram The spectrogram
#' @param min.peak Take only pulses into account with a maximum amplitude of at least min.peak
#' @param min.amplitude The minimum amplitude of the pulse
#' @param delta.amplitude The difference in amplitude between the peak of the pulse and the lowest value in the pulse.
#' @examples
#'  wav <- read_wav(
#'    system.file("demo_wav/leislers.wav", package = "rhinolophus")
#'  )
#'  spectrogram <- wav2spectrogram(wav)
#'  find_pulses(spectrogram)
find_pulses <- function(
  spectrogram,
  min.peak = 40,
  min.amplitude = 10,
  delta.amplitude = 10
){
  assert_that(is.number(min.peak))
  assert_that(is.number(min.amplitude))
  assert_that(is.number(delta.amplitude))
  assert_that(delta.amplitude > 0)
  assert_that(min.peak >= min.amplitude + delta.amplitude)
  assert_that(inherits(spectrogram, "batSpectrogram"))

  pulses <- lapply(
    names(spectrogram@Spectrogram),
    function(fingerprint){
      spectrogram.raster <- spectrogram_raster(
        spectrogram@Spectrogram[[fingerprint]]
      )
      candidate.zone <- clump(
        spectrogram.raster > min.amplitude,
        directions = 4
      )
      local.max <- zonal(spectrogram.raster, candidate.zone, fun = max)
      relevant <- local.max[
        local.max[, 2] >= max(min.peak, min.amplitude + min(delta.amplitude)),
        "zone"
      ]
      if (length(relevant) == 0) {
        return(
          data.frame(
            Xmin = numeric(0),
            Xmax = numeric(0),
            Ymin = numeric(0),
            Ymax = numeric(0),
            Xpeak = numeric(0),
            Ypeak = numeric(0),
            MaxAmplitude = numeric(0),
            DeltaAmplitude = numeric(0),
            Fingerprint = character(0),
            Spectrogram = factor(character(0)),
            stringsAsFactors = FALSE
          )
        )
      }
      lapply(
        relevant,
        function(zone){
          pulse <- data.frame(
            Xmin = numeric(0),
            Xmax = numeric(0),
            Ymin = numeric(0),
            Ymax = numeric(0),
            Xpeak = numeric(0),
            Ypeak = numeric(0),
            MaxAmplitude = numeric(0),
            DeltaAmplitude = numeric(0)
          )
          this.zone <- candidate.zone == zone
          this.zonecell <- Which(this.zone, cells = TRUE)
          xy <- xyFromCell(candidate.zone, this.zonecell)
          this.spec <- crop(spectrogram.raster, extent(xy))
          this.zone <- crop(this.zone, extent(xy))
          this.spec[is.na(this.zone) | this.zone == 0] <- -Inf
          n.focus <- pmin(2 * ceiling(dim(this.spec)[1:2] / 2) - 1, 7)
          this.max <- focal(
            this.spec,
            matrix(1, nrow = n.focus[1], ncol = n.focus[2]),
            max
          )
          candidate <- Which(
            this.spec == this.max & is.finite(this.spec),
            cells = TRUE
          )
          db <- this.spec[candidate]
          relevant.peak <- which(db >= min.peak)
          if (length(relevant.peak) == 0) {
            return(pulse)
          }
          relevant.peak <- relevant.peak[
            order(db[relevant.peak], decreasing = TRUE)
          ]
          candidate <- candidate[relevant.peak]
          db <- db[relevant.peak]

          to.low <- which(db < (min.amplitude + delta.amplitude))
          if (length(to.low) > 0) {
            db <- db[-to.low]
            candidate <- candidate[-to.low]
          }
          if (length(candidate) == 0) {
            return(pulse)
          }
          working <- this.spec
          i <- 1
          while (i <= length(candidate)) {
            minimum.contour <- clump(
              working >= (db[i] - delta.amplitude),
              directions = 4
            )
            local.zone <- minimum.contour[candidate[i]]
            selection <- Which(
              minimum.contour == local.zone,
              cells = TRUE
            )
            same.zone <- i +
              which(minimum.contour[candidate[-seq_len(i)]] == local.zone)
            if (length(same.zone) > 0) {
              candidate <- candidate[-same.zone]
              db <- db[-same.zone]
            }
            if (
              any(
                minimum.contour[candidate[seq_len(i - 1)]] == local.zone,
                na.rm = TRUE
              )
            ) {
              candidate <- candidate[-i]
              db <- db[-i]
              next
            }
            working[selection] <- db[i] - delta.amplitude - 1
            peak <- xyFromCell(this.spec, candidate[i])
            pulse <- xyFromCell(this.spec, selection) %>%
              as.data.frame() %>%
              summarize_(
                Xmin = ~min(x),
                Xmax = ~max(x),
                Ymin = ~min(y),
                Ymax = ~max(y)
              ) %>%
              mutate_(
                Xpeak = peak[, "x"],
                Ypeak = peak[, "y"],
                MaxAmplitude = db[i],
                DeltaAmplitude = ~delta.amplitude
              ) %>%
              bind_rows(pulse)
            i <- i + 1
          }
          return(pulse)
        }
      ) %>%
        bind_rows() %>%
        mutate_(
          Spectrogram = ~factor(fingerprint)
        ) %>%
        rowwise() %>%
        mutate_(
          Fingerprint = ~sha1(
            list(
              Spectrogram = fingerprint,
              Xpeak = Xpeak,
              Ypeak = Ypeak,
              DeltaAmplitude = DeltaAmplitude
            )
          )
        ) %>%
        as.data.frame()
    }
  ) %>%
    bind_rows()

  new(
    Class = "batPulse",
    Metadata = spectrogram@Metadata,
    SpectrogramMetadata = spectrogram@SpectrogramMetadata,
    Spectrogram = spectrogram@Spectrogram,
    PulseMetadata = pulses
  )
}
