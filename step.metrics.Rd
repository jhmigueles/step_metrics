\name{step.metrics}
\alias{step.metrics}
\title{
function to calculate step-based metrics from actigraph csv files aggregated at 1 min epochs
}
\description{
This function calculates steps per day, peak cadences and bands and time spent at use-specified
moderate and vigorous PA threshold based on steps/min.
}
\usage{
step.metrics(datadir, outputdir="./",
             timestamp_colname = "timestamp", 
             steps_colname = "steps",
             th.MOD=100, th.VIG=130)
}
\arguments{
  \item{datadir}{
  Directory where the accelerometer files are stored
  }
  \item{outputdir}{
  Directory where the output needs to be stored. Note that this function will
  attempt to create folders in this directory and uses those folder to organise
  output
  }
  \item{f0}{
   File index to start with (default = 1). Index refers to the filenames sorted
   in increasing order
  }
  \item{timestamp_colname}{
   Column name where the timestamps are stored (default = "timestamp")
  }
  \item{steps_colname}{
   Column name where the steps per min are stored (default = "steps")
  }
  \item{th.MOD}{
  Threshold for moderate PA (in steps/min)
  }
  \item{th.VIG}{
  Threshold for vigorous PA (in steps/min)
  }
}
\details{
 Threshold for moderate and vigorous PA are 100 and 130 steps/min, respectively.
}
\value{
 The function stores two databases in outputdir. The daysummary database contains
 one file per participant with the data at daily level. The personsummary dataset
 contains the daily averages for the recording period
}
\examples{
\dontrun{
datafile = "C:/myfolder/mydata"
outputdir = "C:/myresults"
step.metrics(datadir, outputdir,
             timestamp_colname = "timestamp", 
             steps_colname = "steps",
             th.MOD=100, th.VIG=130)
}
}
\author{
 Jairo H. Migueles <jairo.hidalgo.migueles@gmail.com>
}
\references{
\itemize{
\item Migueles JH, Cadenas-sanchez C, Aguiar EJ, Molina-garcia P. Step-Based Metrics and 
Overall Physical Activity in Children With Overweight or Obesity : Cross-Sectional Study. 
JMIR mHealth uHealth. 2020;8(4). doi:10.2196/14841
\item Tudor-Locke C, Schuna JM, Han H, et al. Cadence (steps/min) and intensity during ambulation 
in 6-20 year olds: the CADENCE-kids study. Int J Behav Nutr Phys Act. 2018;15(1):20. 
doi:10.1186/s12966-018-0651-y
\item Tudor-locke C, Aguiar EJ, Han H, et al. Walking cadence (steps/min) and intensity in 
21–40 year olds: CADENCE-adults. Int J Behav Nutr Phys Act. 2019 Jan 17;16(1):8. doi: 10.1186/s12966-019-0769-6
}
}
