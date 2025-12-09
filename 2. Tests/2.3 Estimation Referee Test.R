# estimation_referee test
#
# testing that the referees in referee_estimate matches referees in soccer_data, output should be TRUE

all(referee_estimate$refNum %in% unique(soccer_data$refNum))
