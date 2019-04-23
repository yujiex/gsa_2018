library("dygraphs")
library("xts")
library("htmlwidgets")
library("readr")

files = list.files(path = "building_energy", pattern = "*.csv")

gsalink_buildings = readr::read_csv("../data/has_energy_ecost.csv") %>%
  .$building

## original 15 min
for (f in files) {
  print(sprintf("--------%s----------", f))
  varname = substr(f, 10, nchar(f)-4)
  building = substr(f, 1, 8)
  ## this part allows only updates in gsalink buildings
  if (!(building %in% gsalink_buildings)) {
    next
  }
  print(varname)
  if (grepl("1_", varname, fixed=TRUE)) {
    next
  }
  p1 = readr::read_csv(sprintf("building_energy/%s", f)) %>%
    na.omit() %>%
    {.}
  print(head(p1))
  timeseries = xts(p1[[varname]], as.POSIXlt(p1$Timestamp,
                                           format="%m/%d/%Y %I:%M:%S %p", tz="EST"))
  dygraph(timeseries, xlab='Time', ylab=varname, main=paste(building, varname)) %>%
    dyRangeSelector() %>%
    saveWidget(sprintf("plot_interval/%s_%s.html", building, varname),
               selfcontained=FALSE, libdir="plot_interval")
}

## copy gsalink buildings to another folder
## get the list of gsalink buildings
## gsalink_buildings = list.files(path="buildling_fault_start_end", pattern = "*.rda")
## gsalink_buildings <- gsub(".rda", "", gsalink_buildings)
gsalink_buildings = readr::read_csv("../data/has_energy_ecost.csv") %>%
  .$building

files = list.files(path = "plot_interval/plot_interval/", pattern = "*.html")

head(files)

## var = "kWh Del Int"
## var = "Natural Gas Vol Int"
var = "Domestic H2O Int gal"
for (f in files) {
  b = substr(f, 1, 8)
  varname = substr(f, 10, nchar(f)-5)
  print(varname)
  if ((b %in% gsalink_buildings) & (varname == var)) {
    print(sprintf("copy building %s", b))
    file.copy(paste0(getwd(), "/plot_interval/plot_interval/", f),
              paste0(getwd(), sprintf("/gsalink_%s/", var), f))
  }
}

## add keyboard navigation
filepath = sprintf("gsalink_%s/", var) # elec
## filepath = "plot_interval/plot_interval/"
files = list.files(path = filepath, pattern = "*.html")
head(files)

nfile = length(files)
nfile

## remove old prev next negivation
## for (i in 1:1) {
##   ## for (i in 1:nfile) {
##   print(files[i])
##   fi <- paste(readLines(sprintf("%s%s", filepath, files[i])))
##   acc <- NULL
##   for (j in 1:length(fi)) {
##     if (substr(fi[j], 1, 12) == "<a id=\"next\"") {
##       next
##     }
##     if (substr(fi[j], 1, 12) == "<a id=\"prev\"") {
##       next
##     }
##     acc <- c(acc, fi[j])
##   }
##   writeLines(paste(acc, collapse='\n'), sprintf("%s%s", filepath, files[i]))
## }

head(f1, n=8)

f1 <- paste(readLines(sprintf("%s%s", filepath, files[1])))

prevfile <- sprintf("<a id=\"prev\" href=\"%s\">prev</a>", files[1])
nextfile <- sprintf("<a id=\"next\" href=\"%s\">next</a>", files[2])
f1_mod <- c(f1[1:6], "<script src=\"navigation.js\" type=\"text/javascript\"></script>", f1[7:16], prevfile, nextfile, f1[17:23])
writeLines(paste(f1_mod, collapse='\n'), sprintf("%s%s", filepath, files[1]))

for (i in 2:(nfile - 1)) {
  print(files[i])
  fi <- paste(readLines(sprintf("%s%s", filepath, files[i])))
  prevfile <- sprintf("<a id=\"prev\" href=\"%s\">prev</a>", files[i - 1])
  nextfile <- sprintf("<a id=\"next\" href=\"%s\">next</a>", files[i + 1])
  fi_mod <- c(fi[1:6], "<script src=\"navigation.js\" type=\"text/javascript\"></script>", fi[7:16], prevfile, nextfile, fi[17:23])
  writeLines(paste(fi_mod, collapse='\n'), sprintf("%s%s", filepath, files[i]))
}

fn <- paste(readLines(sprintf("%s%s", filepath, files[nfile])))
prevfile <- sprintf("<a id=\"prev\" href=\"%s\">prev</a>", files[nfile - 1])
nextfile <- sprintf("<a id=\"next\" href=\"%s\">next</a>", files[nfile])
fn_mod <- c(fn[1:6], "<script src=\"navigation.js\" type=\"text/javascript\"></script>", fn[7:16], prevfile, nextfile, fn[17:23])
writeLines(paste(fn_mod, collapse='\n'), sprintf("%s%s", filepath, files[nfile]))
