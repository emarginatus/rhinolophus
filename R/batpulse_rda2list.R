#' Convert batPulse objects stored in files to a list
#' @param files A vector of filenames
#' @export
#' @importFrom dplyr %>% mutate_ inner_join
batpulse_rda2list <- function(files){
  import.env <- new.env()
  pulse.list <- lapply(
    files,
    function(x){
      load(x, envir = import.env)
      get("pulses.fft", import.env)
    }
  )
  metadata <- do.call(
    rbind,
    lapply(pulse.list, function(x){
      x@Metadata
    })
  )
  pulse.metadata <- do.call(
    rbind,
    lapply(
      pulse.list,
      function(x){
        x@PulseMetadata
      }
    )
  )
  spectrogram.metadata <- do.call(
    rbind,
    lapply(
      pulse.list,
      function(x){
        x@SpectrogramMetadata
      }
    )
  )
  pulse.fourier <- do.call(
    c,
    lapply(
      pulse.list,
      function(x){
        x@PulseFourier
      }
    )
  )
  spectrogram <- do.call(
    c,
    lapply(
      pulse.list,
      function(x){
        x@Spectrogram
      }
    )
  )
  pulse <- pulse.metadata %>%
    mutate_(Spectrogram = ~levels(Spectrogram)[Spectrogram]) %>%
    inner_join(
      spectrogram.metadata %>%
        mutate_(File = ~levels(File)[File]),
      by = c("Spectrogram" = "Fingerprint")
    ) %>%
    inner_join(metadata, by = c("File" = "Fingerprint"))
  return(
    list(
      Pulse = pulse,
      Fourier = pulse.fourier,
      Spectrogram = spectrogram
    )
  )
}
