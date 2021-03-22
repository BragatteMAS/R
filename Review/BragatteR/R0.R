# Rt
##Effective Reproduction Number Estimation from Data Series
# @bragatte 202103201600 refactor [REF](https://github.com/ec-jrc/COVID-19/tree/master/programs/ReprNumber)

# install pack
install.packages("R0")
# loads library
library(R0)
# epidemic curve can be input as a list of dates
epid = c("2012-01-01", "2012-01-02", "2012-01-02", "2012-01-03")
# or as incidence counts
epid.count = c(1, 2, 4, 8)
# create generation time : gamma distribution, with mean 2.6 time units and standard deviation 1 time unit
GT.flu <- generation.time("gamma", c(2.6, 1))
# loads example dataset> data(Germany.1918)
res.R <- estimate.R(Germany.1918,
                     GT = GT.flu,
                     methods = c("EG", "ML", "SB", "TD"))
# applies methods EG, ML, SB, TD to the dataset
plot(res.R)
# diplays results
plotfit(res.R)
# displays fit to the epidemic curve
# sensitivity analysis according to choice of time window for exponential growth
sensitivity.analysis(
    Germany.1918,
    GT.flu,
    begin = 1:15,
    end = 16:30,
    est.method = "EG",
    sa.type = "time"
)
# sensitivity analysis according to generation time
sensitivity.analysis(
    Germany.1918,
    GT.type = "gamma",
    GT.mean = seq(1, 5, 1),
    GT.sd.range = 1,
    begin = 1,
    end = 27,
    est.method = "EG",
    sa.type = "GT"
)