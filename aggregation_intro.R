# Aggregations in R:

#   * there are many functions that can do aggregation for you, but we are only going to cover __ddply()__ in __plyr__ package

# * creating simple aggregated summary:
#   * note: 
# 1. __(group) by__ variable must have at least one input
# 2. you __must__ specify what type of aggregation you want to perform, choose one from: summarize, transform
# * [the link to the package dodument](http://cran.r-project.org/web/packages/plyr/plyr.pdf)
sample_data <- read.csv("./sample_health_facilities.csv")

library(plyr)
my_summary <- ddply(sample_data, .(state, lga), transform, 
                    counts = length(lga_id),
                    total_num_nurse = sum(num_nurses_fulltime, na.rm=T),
                    avg_c_section = mean(c_section_yn == T,na.rm=T))
head(my_summary)


# * look at the output and compare the difference, the only change here is replacing summarize with transform
my_summary <- ddply(sample_data, .(state, lga), summarise, 
                    counts = length(lga_id),
                    total_num_nurse = sum(num_nurses_fulltime, na.rm=T),
                    avg_c_section = mean(c_section_yn == T,na.rm=T))
head(my_summary)


# ddply could take by variable in string format which is very handy when you want to use it in a function
my_summary <- ddply(sample_data, c("state", "lga"), summarise, 
                    counts = length(lga_id),
                    total_num_nurse = sum(num_nurses_fulltime, na.rm=T),
                    avg_c_section = mean(c_section_yn == T,na.rm=T))
head(my_summary)



# define your own function in ddply
# the syntax is pretty much the same as defining functions in R, except 
# it is CRITICAL to add data.frame() function so that it returns a data.frame
# for each small chunk of data splitted by ddply()

my_summary <- ddply(sample_data, .(state), function(df){
                                            data.frame(
                                              unique_lga_number = nrow(df),
                                              avg_c_section = mean(df$c_section_yn == T,na.rm=T),
                                              avg_c_section_true = length(which(df$c_section_yn))
                                            )
                                            })

head(my_summary)
# idata.frame
# If you have HUGE amount of data for ddply to aggregate and you find it annoying to wait a long time before seeing the result
# idata.frame is the solution to this, but it comes with the cost of slightly complicated code.


# An immutable data frame works like an ordinary data frame, except that when you subset
# it, it returns a reference to the original data frame, not a a copy. 
# This makes subsetting substantially faster and has a big impact when you are working
# with large datasets with many groups."

isample <- idata.frame(sample_data)
my_summary <- ddply(isample, .(state), function(df){
                                          data.frame(
                                            unique_lga_number = nrow(df),
                                            avg_c_section = mean(df$c_section_yn == T,na.rm=T),
                                            avg_c_section_true = length(which(df$c_section_yn))
                                          )
                                        })
head(my_summary)

# check the time, the difference with grow bigger with BIG dataset
system.time(replicate(100, ddply(isample, .(state), summarise, mean(num_nurses_fulltime))))

system.time(replicate(100, ddply(sample_data, .(state), summarise, mean(num_nurses_fulltime))))

# Draw backs of idata.frame: sometimes certain functions doesn't work with idata.frame
my_summary <- ddply(isample, .(state), summarise, length(which(c_section_yn)))

my_summary <- ddply(sample_data, .(state), summarise, length(which(c_section_yn)))

## Question: # How would you calculate the proportion of of c_section_yn==TRUE versus total non-NA records in each state?

# since the length(which()) doesn't work, what might be the candidate function go get counts of instances of TRUE and FALSE
# hint from day1: we learned table(), nrow()
# use table() to get frequency count of TRUE and FALSE values
# check if 'TRUE' is contained in the table() output, and assign 'TRUE' counts to numerator. If not return 0
# lastly getting the length of the c_section_yn column, do remember to use na.omit() to skim off the NA values.
my_summary <- ddply(isample, .(state), function(df){ 
                    my_count <- table(df$c_section_yn)
                    data.frame(
                    num_true = if('TRUE' %in% names(my_count)){
                                my_count['TRUE']
                                }else{
                                    0
                                },
                    total_non_na = length(na.omit(df$c_section_yn)),
                    )})
head(my_summary)

