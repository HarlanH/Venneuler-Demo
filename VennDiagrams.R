# Visualizing two sources of data about how DC2 Meetup members overlap

options(stringsAsFactors=FALSE)

library(venneuler) # Written by Wilkinson! Wrapper around Java code.
library(plyr) # Hadley Wickham's amazing tool for split-apply-combine

# simple example, if you've pre-computed the overlap
sample_dat <- c(A=0.3, B=0.3, C=1.1, "A&B"=0.1, "A&C"=0.2, "B&C"=0.1 ,"A&B&C"=0.1)
sample_diagram <- venneuler(sample_dat)
print(sample_diagram) # building these when N>2 is a constrained optimization problem!
plot(sample_diagram)

# survey says...
dat_surv <- read.csv('SurveyMeetupAttendees.csv')
names(dat_surv) <- c('id', 'ActionDesign', 'BDDC', 'BDDistrict', 'DBDC', 'DSDC',
                'DCACM','LeanStartup','Python','DCTech','eDiscovery','FedBigData',
                'GeoDC','Hadoop', 'Intel', 'OpenAnalytics','RUDC','SAS','SMA',
                'WSS','WINFORMS') # replace long names; could also use plyr::rename()
View(dat_surv)

# have unordered string values -- turn into ordered factors!
levs <- c('Never heard of this', 'Am not a member', 'Am a member but rarely attend',
          'Am a member and sometimes attend', 'Am a member and usually attend')
for (i in names(dat_surv)[-1]) { # for loops are slow -- who cares! (ignore ID column)
  dat_surv[,i] <- factor(dat_surv[,i], levels=levs, ordered=TRUE)
}
print(summary(dat_surv))

# remove respondents whose only responses are NAs
to_remove <- daply(dat_surv,
                   .(id), # row-wise; could also do aaply(dat, 1, ...)
                   function(rr) all(is.na(rr[,2:ncol(rr)])) # ignoring ID, all NAs?
                   )
print(sum(to_remove))
dat_surv <- dat_surv[!to_remove,]

# build a diagram using survey data

is_member <- function(x) x %in% c('Am a member but rarely attend',
                                  'Am a member and sometimes attend', 
                                  'Am a member and usually attend') 
              # could also do as.numeric(x) >= 3, but less readable

mus <- c('DSDC', 'DBDC', 'RUDC') # only want these 3 columns

# plyr::colwise() takes a function that works on a column and turns it into a function
# that works on all columns of a dataframe. Handy!
dc2_surv <- colwise(function(cc) is_member(cc))(dat_surv[,mus])

# back to readable names w/ plyr::rename()
dc2_surv <- rename(dc2_surv, c('DSDC'='Data Science', 'DBDC'='Data Business','RUDC'='R Users'))

# bam
plot(venneuler(dc2_surv))

# load data that Marck pulled from the Meetup API

# # offline pre-processing step
# load("dc2-2013-01-24.RData")
# dc2df <- llply(dc2df, function(x) subset(x$members, select=c(id, lat, lon, member.last.visited, status)))
# save(dc2df, file="dc2-2013-01-24-simple.Rdata")

load("dc2-2013-01-24-simple.Rdata") # -> dc2df
str(dc2df) # got a list of DFs

# there are a few ways of taking these lists of IDs and turning them into a DF for
# the venneuler function. This one generates a set of true/false columns, as before.

# could do things like filter on status, recent activity, etc, but not currently.

# get the unique IDs from all three DFs
all_ids <- unique(unlist(llply(dc2df, function(x) x$id)))

# foreach meetup, find out which of the IDs are in _this_ one.
# the t() forces the right shape of the DF.
dc2_api <- as.data.frame(t(laply(dc2df, function(x) all_ids %in% x$id)))
# lost the column names; put 'em back
names(dc2_api) <- names(dc2df)
# then use plyr::rename() again to make pretty labels
dc2_api <- rename(dc2_api, c('r.users.dc'='R Users', 'data.science.dc'='Data Science',
                             'data.business.dc'='Data Business'))
# and make consistent with the first df
dc2_api <- dc2_api[,names(dc2_surv)] # reorder

plot(venneuler(dc2_api))
