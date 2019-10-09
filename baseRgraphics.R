##############################################################################################################################################################
# R code written by Joset A. Etzel (jetzel@wustl.edu, mvpa.blogspot.com) for R-Ladies STL, October 2019
# https://wustl.box.com/v/EtzelBaseRGraphicsSlides  and    https://wustl.box.com/v/EtzelBaseRGraphicsCode
# adapted from "D:\maile\svnFiles\plein\mvpa_meanderings\boxplotGraphDemo.R" & https://mvpa.blogspot.com/2016/02/r-demo-specifying-side-by-side-boxplots.html
# This code may be adapted for personal use, provided the source is cited.
#
# The code demonstrates various base R graphics.
##############################################################################################################################################################
# make a "toy" dataset: how long dogs of two breeds play with four different types of toys.

rm(list=ls());  # clear R's workspace - WARNING if you like to keep things in the workspace
options(warnPartialMatchDollar=TRUE);    # safety option: warns if column names specified with $ don't match exactly

breed.ids <- c("labrador", "dachshund");  # two dog breeds
toy.ids <- c("frisbee", "rope", "bone", "plush");   # each dog has a measurement for each of the four toy types
num.dogs <- 30;   # generate 30 datapoints for each type of dog on each toy

set.seed(4002);  # to reproduce the random numbers
data.tbl <- data.frame(array(NA, c((length(breed.ids)*num.dogs)*length(toy.ids), 4)));  # blank table we'll fill up with the fake dataset; long-format
colnames(data.tbl) <- c("dog.id", "breed.id", "toy.id", "play.min");
ctr <- 1;   # row-counter for filling up data.tbl
# make the fake dataset (fill up data.tbl)
for (tid in 1:length(toy.ids)) { 
  for (bid in 1:length(breed.ids)) {  
    for (did in 1:num.dogs) {   # tid <- 1; bid <- 1; did <- 1;
      data.tbl$dog.id[ctr] <- did;
      data.tbl$breed.id[ctr] <- breed.ids[bid];
      data.tbl$toy.id[ctr] <- toy.ids[tid];
      
      # add a breed-based bias to the generated data so will have some trends in the plots
      if (breed.ids[bid] == "labrador") {
        if (toy.ids[tid] == "frisbee") { data.tbl$play.min[ctr] <- runif(1, min=20, max=60); }  # how long (in minutes) toy played with
        if (toy.ids[tid] == "rope") { data.tbl$play.min[ctr] <- runif(1, min=10, max=50); }  
        if (toy.ids[tid] == "bone") { data.tbl$play.min[ctr] <- runif(1, min=0, max=60); }  
        if (toy.ids[tid] == "plush") { data.tbl$play.min[ctr] <- runif(1, min=0, max=40); }  
      }
      if (breed.ids[bid] == "dachshund") {
        if (toy.ids[tid] == "frisbee") { data.tbl$play.min[ctr] <- runif(1, min=0, max=10); }  # how long (in minutes) toy played with
        if (toy.ids[tid] == "rope") { data.tbl$play.min[ctr] <- runif(1, min=0, max=20); }  
        if (toy.ids[tid] == "bone") { data.tbl$play.min[ctr] <- runif(1, min=0, max=60); }  
        if (toy.ids[tid] == "plush") { data.tbl$play.min[ctr] <- runif(1, min=0, max=60); }  
      }
      
      ctr <- ctr + 1;   # done with making this row's data, so increment counter
    }
  }
}

# add a few missings, for realism
data.tbl$play.min[which(data.tbl$dog.id == 3 & data.tbl$breed.id == "labrador" & data.tbl$toy.id == "bone")] <- NA;
data.tbl$play.min[which(data.tbl$dog.id == 4 & data.tbl$breed.id == "labrador" & data.tbl$toy.id == "plush")] <- NA;
data.tbl$play.min[which(data.tbl$dog.id == 5 & data.tbl$breed.id == "dachshund" & data.tbl$toy.id == "bone")] <- NA;
data.tbl$play.min[which(data.tbl$dog.id == 6 & data.tbl$breed.id == "dachshund" & data.tbl$toy.id == "frisbee")] <- NA;


# str(data.tbl);
# 'data.frame':	240 obs. of  4 variables:
#   $ dog.id  : int  1 2 3 4 5 6 7 8 9 10 ...
#   $ breed.id: chr  "labrador" "labrador" "labrador" "labrador" ...
#   $ toy.id  : chr  "frisbee" "frisbee" "frisbee" "frisbee" ...
#   $ play.min: num  31.4 25.2 45.5 55.9 48 ...
# note the column types.

# to save to disk: write.csv(data.tbl, "d:/temp/baseRdemo.csv");

##############################################################################################################################################################
# dot plot: four columns for each toy type together, grouped by breed
# run this code following the previous - don't clear R's memory (or reload data.tbl, breed.ids, toy.ids, num.dogs)

# now have a fake dataset, so can shift to plotting. The code first sets some parameters used for both plots, then generates each plot separately.
windows(width=3, height=3);  # specify plot window size, in inches. quartz() is the mac equivalent.
# the code will run without this, but the plot size (and so needed boxplot widths, font sizes, etc will vary.

# example layout commands - they go here, between windows() and par().
# layout(matrix(c(1,2), c(1,2)));    # two plots side-by-side; see with layout.show(2); 
# layout(matrix(c(1,1,2), c(1,3)));   # two plots side-by-side, first twice as wide as the second   # layout.show(2); 
# layout(matrix(c(1,1, 2, 3, 4, 5), nrow=2, byrow=TRUE));   # layout.show(5);  # two-row layout, 2 plots in top row, 3 in second.

par(mar=c(2, 2.5, 1.5, 0.75), mgp=c(1.1, 0.2, 0), tcl=-0.2);  # specify general margins; I think these are nice to start & the defaults too wide
# look at help for par() for many more options; this is a bit of the help.
# mar: c(bottom, left, top, right), number of lines of margin. default is c(5, 4, 4, 2) + 0.1.
# mgp: margin line (in mex units) for the axis title, axis labels and axis line. mgp[1] is title, mgp[2:3] is axis. default is c(3, 1, 0).
# The length of tick marks as a fraction of the height of a line of text. The default value is -0.5; setting tcl = NA sets tck = -0.01

# plotting-related variables used to simplify plotting code below. I generally fiddle with the cx, shifts, etc. variables to make the plots readable.
toy.cols <- c('firebrick', 'darkmagenta', 'forestgreen', 'grey40');   # color for each toy (toy.ids order); www.stat.columbia.edu/~tzheng/files/Rcolor.pdf
shifts <- c(-0.15, -0.05, 0.05, 0.15);  # x offsets.
  # I find it easier to hard-code where the boxplots/columns will fall (via shifts) rather than calculating the plotting locations on the fly. 

# figure out range of data so can set plot axis limits (we know range since made the fake data above, but usually don't)
# > summary(data.tbl$play.min)
#    Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
#  0.1278  9.0993 20.8342 23.4309 35.9075 59.6288       4 
y.lim <- c(min(data.tbl$play.min, na.rm=TRUE), max(data.tbl$play.min, na.rm=TRUE)+7);  # y-axis limits. +7 at top to give room for legend
x.lim <- c(0.7, (length(breed.ids)+0.3));   # x-axis limits; one for each of the two breeds, so same as x.lim <- c(0.7, 2.3);

# make the blank plot
plot(x=0, y=0, xlim=x.lim, ylim=y.lim, xaxt='n', col='white', xlab="", ylab="time (minutes)", main="", cex.lab=0.8, cex.axis=0.8);
mtext(side=3, text="toy dataset", line=0.15, cex=0.9);    # plot title (on top)
axis(side=1, at=1:length(breed.ids), labels=breed.ids, cex.axis=0.8);   # put on the x-axis breed labels
grid(nx=NA, ny=NULL, col='darkgrey');     # add horizontal gridlines

# add a column of points for each toy & breed
for (tid in 1:length(toy.ids)) {
  for (bid in 1:length(breed.ids)) {   # tid <- 3;  bid <- 1;  
    vals <- data.tbl$play.min[which(data.tbl$breed.id == breed.ids[bid] & data.tbl$toy.id== toy.ids[tid])];  # get the data to plot.
    if (length(vals) != num.dogs) { stop("length(vals) != num.dogs"); }  # error-checking code: only should be num.dogs in each measure
    
    points(x=jitter(rep(bid+shifts[tid], length(vals))), y=vals, col=toy.cols[tid], cex=1.2);   # add points, w/jitter to reduce overplotting
  }
}
legend(x='top', legend=toy.ids, fill=toy.cols, horiz=FALSE, cex=0.7, bg='white', box.col='white');   # add a legend
box();  # finally, redraw the box around the outside to be nice and neat

##############################################################################################################################################################
# dot plot: two columns for each breed together, grouped by toy. (overwrites previous plot unless do windows() again)
# run this code following the previous - don't clear R's memory (if do, reload data.tbl, breed.ids, toy.ids, num.dogs, y.lim)
# if you call another plot() with a open window it will blank out the first plot and start over. If you don't want to lose the first plot,
# call window(), layout(), and par() again to open a new plotting window.

b.shifts <- c(-0.15, 0.15);  # x offsets for the two breeds (how much to shift to the left or right)
b.cols <- c("black", "burlywood4");
x.lim <- c(0.5, (length(toy.ids)+0.5));   # x-axis limits; one for each of the toy types
# same y-axis limits as before

# set up the blank plot
plot(x=0, y=0, xlim=x.lim, ylim=y.lim, xaxt='n', col='white', xlab="", ylab="time (minutes)", main="", cex.lab=0.8, cex.axis=0.8);
mtext(side=3, text="toy dataset", line=0.15, cex=0.9);    # plot title (on top)
axis(side=1, at=1:length(toy.ids), labels=toy.ids, cex.axis=0.8);   # put on the x-axis labels
grid(nx=NA, ny=NULL, col='darkgrey');     # horizontal grid lines

# add column of points for each toy & breed
for (tid in 1:length(toy.ids)) {
  for (bid in 1:length(breed.ids)) {   # tid <- 3;  bid <- 1;  
    vals <- data.tbl$play.min[which(data.tbl$breed.id == breed.ids[bid] & data.tbl$toy.id== toy.ids[tid])];  # get the data to plot.
    if (length(vals) != num.dogs) { stop("length(vals) != num.dogs"); }  # error-checking code: only should be num.dogs in each measure
    
    points(x=jitter(rep(tid+b.shifts[bid], length(vals))), y=vals, col=b.cols[bid], cex=1.2);   # add points, w/jitter to reduce overplotting
  }
}
legend(x='top', legend=breed.ids, fill=b.cols, horiz=TRUE, cex=0.7, bty='n'); # set font size a bit smaller to fit
box();  # redraw the box around the outside to be nice and neat

##############################################################################################################################################################
# boxplot version: four columns for each toy type together, grouped by breed
# make sure you reset x.lim if running all the code in order.

# these variables are still in memory from above: if you call another plot() with a open window it will blank out the first plot and start over.
# calling windows() again will make a new plot window.
# windows(width=3, height=3);  # specify plot window size, in inches. quartz() is the mac equivalent.
# par(mar=c(2, 2.5, 1.5, 0.75), mgp=c(1.1, 0.2, 0), tcl=-0.2);  # specify general margins; I think these are nice to start & the defaults too wide
# y.lim <- c(min(data.tbl$play.min, na.rm=TRUE), max(data.tbl$play.min, na.rm=TRUE)+7);  # y-axis limits. +7 at top to give room for legend
x.lim <- c(0.7, (length(breed.ids)+0.3));   # x-axis limits; one for each of the two breeds
# toy.cols <- c('firebrick', 'darkmagenta', 'forestgreen', 'grey40');   # color to plot each of the toys (toy.ids order)
# shifts <- c(-0.15, -0.05, 0.05, 0.15);  # x offsets.

plot(x=0, y=0, xlim=x.lim, ylim=y.lim, xaxt='n', col='white', xlab="", ylab="time (minutes)", main="", cex.lab=0.8, cex.axis=0.8);
mtext(side=3, text="toy dataset", line=0.15, cex=0.9);    # plot title (on top)
axis(side=1, at=1:length(breed.ids), labels=breed.ids, cex.axis=0.8);   # put on the x-axis breed labels
grid(nx=NA, ny=NULL, col='darkgrey');   

for (tid in 1:length(toy.ids)) {
  for (bid in 1:length(breed.ids)) {   # tid <- 3;  bid <- 1;  
    vals <- data.tbl$play.min[which(data.tbl$breed.id == breed.ids[bid] & data.tbl$toy.id== toy.ids[tid])];  # find the rows with the data we'll plot.
    if (length(vals) != num.dogs) { stop("length(vals) != num.dogs"); }  # error-checking code: only should be num.dogs in each measure
    vals <- vals[which(!is.na(vals))];   # jitter fails with NAs (and can't plot NAs) so take out here
    
    boxplot(vals, at=bid+shifts[tid], col=toy.cols[tid], add=TRUE, xaxt='n', yaxt='n', bty='n', boxwex=0.15, cex=0.7); # draw the boxplot
    # setting xaxt, yaxt, and bty to 'n' is so that R doesn't redraw the axis labels each time a boxplot is added to the plot
    # when making lots of boxplots, sometimes R doesn't guess the box width (boxwex) or outlier dot (cex) size properly.
    # The default for these is 1; bigger numbers makes wider boxes and bigger dots; smaller, smaller.
  }
}

legend(x='top', legend=toy.ids, fill=toy.cols, horiz=FALSE, cex=0.7, bg='white', box.col='white'); # set font size a bit smaller to fit
box();  # redraw the box around the outside to be nice and neat

##############################################################################################################################################################
# barplot version

# most variables are still in memory from above; I changed y.lim a bit to fit better, and added bar.half for convienience
# windows(width=3, height=3);  # specify plot window size, in inches. quartz() is the mac equivalent.
# par(mar=c(2, 2.5, 1.5, 0.75), mgp=c(1.1, 0.2, 0), tcl=-0.2);  # specify general margins; I think these are nice to start & the defaults too wide
# x.lim <- c(0.7, (length(breed.ids)+0.3));   # x-axis limits; one for each of the two breeds
# toy.cols <- c('firebrick', 'darkmagenta', 'forestgreen', 'grey40');   # color to plot each of the toys (toy.ids order)
# shifts <- c(-0.15, -0.05, 0.05, 0.15);  # x offsets.
bar.half <- 0.05;   # unique(diff(shifts))/2: half width want the bars to be
y.lim <- c(0, 45);  # y-axis limits. shrink a bit to show bars clearly

# start the blank plot
plot(x=0, y=0, xlim=x.lim, ylim=y.lim, xaxt='n', col='white', xlab="", ylab="time (minutes)", main="", cex.lab=0.8, cex.axis=0.8, yaxs='i');
mtext(side=3, text="toy dataset", line=0.15, cex=0.9);    # plot title (on top)
axis(side=1, at=1:length(breed.ids), labels=breed.ids, cex.axis=0.8);   # put on the x-axis breed labels
grid(nx=NA, ny=NULL, col='darkgrey');   

for (tid in 1:length(toy.ids)) {
  for (bid in 1:length(breed.ids)) {   # tid <- 3;  bid <- 1;  
    vals <- data.tbl$play.min[which(data.tbl$breed.id == breed.ids[bid] & data.tbl$toy.id== toy.ids[tid])];  # find the rows with the data we'll plot.
    if (length(vals) != num.dogs) { stop("length(vals) != num.dogs"); }  # error-checking code: only should be num.dogs in each measure
    
    # draw the bar for this toy & breed. right side of each at shifts[tid], left at shifts[tid]-bar.half
    mean.val <- mean(vals, na.rm=TRUE);    # calculate the mean - the top of each bar
    rect(xleft=bid+shifts[tid]-bar.half, xright=bid+shifts[tid]+bar.half, ybottom=y.lim[1], ytop=mean.val, border=NA, col=toy.cols[tid]); 
    
    # add whiskers to show the SEM. want these to come out of the middle of the bar
    sem.val <- sd(vals, na.rm=TRUE)/sqrt(length(vals));   # calculate the SEM
    arrows(x0=bid+shifts[tid], x1=bid+shifts[tid], y0=mean.val, y1=mean.val+sem.val, angle=90, length=0.025);  # up error bar
    arrows(x0=bid+shifts[tid], x1=bid+shifts[tid], y0=mean.val, y1=mean.val-sem.val, angle=90, length=0.025);  # down error bar
  }
}
legend(x='top', legend=toy.ids, fill=toy.cols, horiz=FALSE, cex=0.7, bg='white', box.col='white');  # set font size a bit smaller to fit
box();  # redraw the box around the outside to be nice and neat

######################################################################################################################################################
# with the barplot() function. If you have an open window R will draw into it; if not, R will open a new one.

rm(list=ls());  # clear R's workspace
options(warnPartialMatchDollar=TRUE);    # safety option: warns if column names specified with $ don't match exactly

data.tbl <- read.csv("d:/temp/baseRdemo.csv");   # if saved above, or rerun first block of code to recreate data.tbl

breed.ids <- c("labrador", "dachshund");  # two dog breeds
toy.ids <- c("frisbee", "rope", "bone", "plush");   # each dog has a measurement for each of the four toy types
num.dogs <- 30;   # generate 30 datapoints for each type of dog on each toy

# need to precalculate the means & SEMs, so make a new data.frame to store
mean.tbl <- data.frame(array(NA, c(length(toy.ids)*length(breed.ids),4))); 
colnames(mean.tbl) <- c("breed.id", "toy.id", "play.mean", "play.sem");
ctr <- 1;  # row counter
for (tid in 1:length(toy.ids)) {
  for (bid in 1:length(breed.ids)) {   # tid <- 1;  bid <- 2;  
    vals <- data.tbl$play.min[which(data.tbl$breed.id == breed.ids[bid] & data.tbl$toy.id== toy.ids[tid])];  # data we need
    if (length(vals) != num.dogs) { stop("length(vals) != num.dogs"); }  # error-checking code: only should be num.dogs in each measure
    
    mean.tbl$breed.id[ctr] <- breed.ids[bid];
    mean.tbl$toy.id[ctr] <- toy.ids[tid];
    mean.tbl$play.mean[ctr] <- mean(vals, na.rm=TRUE);   # calculate and store the mean
    mean.tbl$play.sem[ctr] <- sd(vals, na.rm=TRUE)/sqrt(length(vals));   # calculate and store the SEM
    ctr <- ctr + 1;
  }
}
# mean.tbl   # small enough to print the whole thing

# now try some plots
barplot(play.mean~toy.id+breed.id, data=mean.tbl);   # breeds along x-axis, toys stacked
barplot(play.mean~breed.id+toy.id, data=mean.tbl);   # toys along x-axis, breeds stacked (note change of ordering)

toy.cols <- c('firebrick', 'darkmagenta', 'forestgreen', 'grey40');   # color to plot each of the toys (toy.ids order)
barplot(play.mean~toy.id+breed.id, data=mean.tbl, beside=TRUE, col=toy.cols);  # fix stacking, colors

barplot(play.mean~toy.id+breed.id, data=mean.tbl, beside=TRUE, col=toy.cols, legend.text=TRUE);  # adds a legend

######################
# barplot() function, with ordering fixed and error bars added.

# str(mean.tbl);     # see that the breed & toy columns are chr, which are shown in alphabetical order
# relevel the factor columns in mean.tbl
mean.tbl$breed.id <- factor(mean.tbl$breed.id, levels=c("labrador", "dachshund"), ordered=TRUE);
mean.tbl$toy.id <- factor(mean.tbl$toy.id, levels=toy.ids, ordered=TRUE);
# mean.tbl   # looks the same; str(mean.tbl) is different

barplot(play.mean~toy.id+breed.id, data=mean.tbl, beside=TRUE, col=toy.cols, legend.text=TRUE);  # order and color fixed


barplot(play.mean~toy.id+breed.id, data=mean.tbl, beside=TRUE, col=toy.cols, border=NA, ylim=c(0, 45), xlab="", ylab="time (minutes)", main="",
        legend.text=TRUE, args.legend=list(x='top', ncol=2, cex=0.9, bty='n'));   # set lots of parameters
mtext(side=3, text="toy dataset", line=0.2, cex=1.5);    # plot title (on top)
box();   # add a box around the outside


# where to put the error bars?
# str(barplot(play.mean~toy.id+breed.id, data=mean.tbl, beside=TRUE, col=toy.cols, border=NA, ylim=c(0, 45), xlab="", ylab="time (minutes)", main="",
#             legend.text=TRUE, args.legend=list(x='top', ncol=2, cex=0.9, bty='n')))
# num [1:4, 1:2] 1.5 2.5 3.5 4.5 6.5 7.5 8.5 9.5
barplot(play.mean~toy.id+breed.id, data=mean.tbl, beside=TRUE, col=toy.cols, border=NA, ylim=c(0, 45), xlab="", ylab="time (minutes)", main="",
        legend.text=TRUE, args.legend=list(x='top', ncol=2, cex=0.9, bty='n'));   # set lots of parameters

centers <- c(1.5,6.5, 2.5,7.5, 3.5,8.5, 4.5,9.5);  # hard-coded in mean.tbl row order
arrows(x0=centers, x1=centers, y0=mean.tbl$play.mean, y1=mean.tbl$play.mean+mean.tbl$play.sem, angle=90, length=0.025, lwd=2);  # up error bars
arrows(x0=centers, x1=centers, y0=mean.tbl$play.mean, y1=mean.tbl$play.mean-mean.tbl$play.sem, angle=90, length=0.025, lwd=2);  # down error bars

mtext(side=3, text="toy dataset", line=0.2, cex=1.5);    # plot title (on top)
box();   # add a box around the outside

###################################################################################################################################################################


