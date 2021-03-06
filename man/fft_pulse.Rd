% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fft_pulse.R
\name{fft_pulse}
\alias{fft_pulse}
\title{Calculate the fast fourier transformation for each pulse}
\usage{
fft_pulse(pulses, spectrogram, n.fourier = 30)
}
\arguments{
\item{pulses}{A list of pulses. E.g. the output of \code{\link{find_pulses}}}

\item{spectrogram}{The spectrogram. E.g. the output of \code{\link{wav2spectrogram}}}

\item{n.fourier}{The number of required Fourier components in each direction.}
}
\description{
Calculate the fast fourier transformation for each pulse
}
\examples{
wav <- read_wav(
  system.file("demo_wav/leislers.wav", package = "rhinolophus")
)
spectrogram <- wav2spectrogram(wav)
pulses <- find_pulses(spectrogram = spectrogram)
pulse.fft <- fft_pulse(pulses = pulses, spectrogram = spectrogram)
}
