#############################################################
## 2023 Colombia Local Elections Model by @PoliticaConDato ##
#############################################################

## Load packages
library(lubridate)
library(reshape2)
library(scales)
library(tidyr)
library(dplyr)
library(ggplot2)
library(kableExtra)
library(png)
library(cowplot)
library(magick)
library(tidyverse)
library(data.table)
library(ggrepel)

#############       POLL MODEL       ################

## Select election
elections <- c("Cali","Bogota")

election <- elections[1]


## Load polls
polls <- read.csv(paste0("https://raw.githubusercontent.com/PoliticaConDato/Local-Elections-2023/main/Polls/Polls_",election,".csv"))

## Clean data
polls <- polls[,!names(polls) %in% c("Start.Date","End.Date","Methodology")]
polls$Date <- mdy(polls$Date)
polls[is.na(polls)] <- 0
polls$Decided <- 1 - polls$Undecided

polls <- reshape2::melt(polls, id=c("ID","Date","Pollster","Rating","Sample","Decided","Error"), variable.name = "Candidate")
polls$value.norm <- polls$value / polls$Decided

## Address multiple scenarios
polls <- group_by(polls, ID, Date, Pollster, Rating, Sample, Error, Candidate)
polls <- summarise(polls, 
                   value = mean(value),
                   value.norm = mean(value.norm)
                   
)

## Weights for model
polls$rating.weight <- polls$Rating / 10
polls$error.weight <- 1-polls$Error*3

## Model dataframe
start.date <- min(polls$Date)
end.date <- max(polls$Date)
date.vec <- seq(start.date, end.date, 1)
cand.vec <- unique(as.character(polls$Candidate))

model.df <- merge(date.vec,cand.vec)
colnames(model.df) <- c("Date","Candidate")

weighted.values <- function(x) {
  
  model.date <- ymd(x[1])
  reduced.polls <- polls[polls$Candidate == x[2],]
  
  reduced.polls$days <- model.date - reduced.polls$Date
  reduced.polls$date.weight <- ifelse(reduced.polls$days < 0, 0, ifelse(reduced.polls$days > 60, 0, 1.2*(1-reduced.polls$days/60)))
  reduced.polls <- reduced.polls[rev(order(reduced.polls$Pollster, reduced.polls$Date)),]
  reduced.polls <- reduced.polls[reduced.polls$date.weight > 0, ]
  reduced.polls <- reduced.polls[!duplicated(reduced.polls$Pollster),]
  reduced.polls$weight <- reduced.polls$rating.weight * reduced.polls$error.weight * reduced.polls$date.weight
  reduced.polls$weighted.value <- reduced.polls$weight*reduced.polls$value 
  reduced.polls$weighted.value.norm <- reduced.polls$weight*reduced.polls$value.norm 
  total.weight <- sum(reduced.polls$weight)
  vote.cols <- reduced.polls[,c("weighted.value","weighted.value.norm")]
  vote.cols <- colSums(vote.cols)
  vote.cols <- vote.cols / total.weight
  return(vote.cols)
}

model.df <- cbind(model.df,t(apply(model.df, 1, weighted.values)))

## Plot model and polls 

colors.Bogota <- c(Undecided = "#808285", Oviedo = "#F029A7", Bolivar = "#800080", Galan = "#C41C0C", Molano = "#1E4B8F", Lara = "#BFAB25", Otros = "#2fbef2")

colors.Cali <- c(Undecided = "#808285", Ortiz = "#282883", Rojas = "#e20e28", Eder = "#f3701b", Torres = "#BFAB25", Otros = "#2fbef2")

group.colors <- get(paste0("colors.",election))

min.graph.date <- mdy("01/01/2023")

filtered.polls <- polls[polls$Date >= min.graph.date,]
filtered.model <- model.df[model.df$Date >= min.graph.date,]


data.plot <- ggplot(filtered.polls, aes(x=Date, y=value, color=Candidate)) +
  geom_point() +
  stat_smooth(aes(fill=Candidate)) +
  scale_fill_manual(values=group.colors) +
  scale_color_manual(values=group.colors) + scale_y_continuous(labels = scales::percent) + 
  theme(legend.position="top", legend.title = element_blank(), legend.box = "horizontal", plot.title = element_text(hjust = 0.5)) + 
  ggtitle(paste0("Polls for 2023 ",election," Local Election")) +
  xlab("Date") + 
  ylab("Vote %") +
  guides(color = guide_legend(nrow = 1, byrow = TRUE)) + 
  geom_line(data = filtered.model, aes(x=Date, y=weighted.value, color=Candidate), linetype = "dashed") 


my_plot_1 <- ggdraw() +
  draw_plot(data.plot) +
  draw_image("https://raw.githubusercontent.com/PoliticaConDato/Elecciones-2022/main/data_2nda/PoliData.png", scale = 0.07, x = 0.475, y = -0.47 ) +
  draw_text("@PoliticaConDato", size = 12, x = 0.85, y = 0.03)

my_plot_1

png(paste0(election,"_Full_Polls.png"), width = 1200, height = 900, res = 120)
my_plot_1
dev.off()

## Plot excluding undecided
data.plot <- ggplot(filtered.polls[filtered.polls$Candidate != "Undecided",], aes(x=Date, y=value.norm, color=Candidate)) +
  geom_point() +
  stat_smooth(aes(fill=Candidate)) +
  scale_fill_manual(values=group.colors) +
  scale_color_manual(values=group.colors) + scale_y_continuous(labels = scales::percent) + 
  theme(legend.position="top", legend.title = element_blank(), legend.box = "horizontal", plot.title = element_text(hjust = 0.5)) + 
  ggtitle(paste0("Polls for 2023 ",election," Local Election")) +
  xlab("Date") + 
  ylab("Vote %") +
  guides(color = guide_legend(nrow = 1, byrow = TRUE)) + 
  geom_line(data = filtered.model[filtered.model$Candidate != "Undecided",], aes(x=Date, y=weighted.value.norm, color=Candidate), linetype = "dashed")  
  #+coord_cartesian(ylim = c(0, .7)) 


my_plot_2 <- ggdraw() +
  draw_plot(data.plot) +
  draw_image("https://raw.githubusercontent.com/PoliticaConDato/Elecciones-2022/main/data_2nda/PoliData.png", scale = 0.07, x = 0.475, y = -0.47 ) +
  draw_text("@PoliticaConDato", size = 12, x = 0.85, y = 0.03)

my_plot_2

png(paste0(election,"_Polls_Excl_Undecided.png"), width = 1200, height = 900, res = 120)
my_plot_2
dev.off()

## Polling output model
poll.model <- model.df[model.df$Date == max(model.df$Date),]
poll.model <- poll.model[,-c(1,3)]
colnames(poll.model) <- c("Candidate","Polls")

## Clean environment
remove(data.plot, filtered.model, filtered.polls, my_plot_1, my_plot_2, start.date, end.date)

#############       TRENDS  MODEL       ################

keywords <- c("Alcalde","2023","Propuestas")
candidates.Cali <- c("Ortiz","Eder","Rojas","Torres")
candidates.Bogota <- c("Oviedo","Bolivar","Galan","Molano","Lara")
candidates <- get(paste0("candidates.",election))

calibration.weight <- as.data.frame(cbind(candidates,rep(1, times = length(candidates))))
colnames(calibration.weight) <- c("Candidate","Calibration")
calibration.weight$Calibration <- as.numeric(calibration.weight$Calibration)

geography <- switch(
  election,
  "Cali" = "CO-VAC",
  "Bogota" = "CO-DC",
  "CO"
)

trend.search <- function(x) {
  searchvector <- unlist(lapply(candidates, function(candidate) paste0(candidate, " ", x)))
  trends <- gtrendsR::gtrends(searchvector, geo = geography, time = "today 3-m", onlyInterest = TRUE)
  trends <- trends$interest_over_time
  trends <- trends[,c(1,2,3)]
  trends$hits <- as.numeric(trends$hits)
  trends$hits[is.na(trends$hits)] <- 0
  trends[is.na(trends)] <- 0
  write.csv(trends, paste0("Polls/",election,"_trends_intent_",x,".csv"))
  
  trends$Candidate <- gsub( " .*$", "", trends$keyword )
  
  trends.t <- trends %>%
    dplyr::arrange((date)) %>% 
    dplyr::group_by(Candidate) %>% 
    dplyr::mutate(mean.7 = frollmean(hits, 7),
                  mean.30 = frollmean(hits, 30)) %>% 
    dplyr::ungroup()
  
  trends.t <- merge(trends.t, calibration.weight, by = "Candidate", all.x = TRUE)
  
  trends.t[,c("mean.7","mean.30")] <- trends.t[,c("mean.7","mean.30")] * trends.t$Calibration
  
  trends.t <- trends.t %>%
    dplyr::arrange((date)) %>% 
    dplyr::group_by(date) %>% 
    dplyr::mutate(total.7 = sum(mean.7),
                  total.30 = sum(mean.30)) %>% 
    dplyr::ungroup()
  
  trends.t$per.7 <- trends.t$mean.7 / trends.t$total.7
  trends.t$per.30 <- trends.t$mean.30 / trends.t$total.30
  
  trends.t$per.7 <-  trends.t$per.7 * (1 - poll.model$Polls[poll.model$Candidate == "Otros"])
  trends.t$per.30 <-  trends.t$per.30 * (1 - poll.model$Polls[poll.model$Candidate == "Otros"])
  trends.t$avg <- (trends.t$per.7 + trends.t$per.30) / 2 
  
  trends <- trends.t[,c("date","Candidate","avg")]
  
  return(trends)
}

trend.names.Cali <- c("Roberto Ortiz","Alejandro Eder","Diana Rojas","Miyerlandi Torres")
trend.names.Bogota <- c("Juan Oviedo","Gustavo Bolivar","Carlos Galan","Diego Molano","Rodrigo Lara")

trend.names <- get(paste0("trend.names.",election))

trend.search.name <- function(x) {
  searchvector <- unlist(lapply(x, paste0))
  
  trends <- gtrendsR::gtrends(searchvector, geo = geography, time = "today 3-m", onlyInterest = TRUE)
  trends <- trends$interest_over_time
  trends <- trends[,c(1,2,3)]
  trends$hits <- as.numeric(trends$hits)
  trends$hits[is.na(trends$hits)] <- 0
  trends[is.na(trends)] <- 0
  write.csv(trends, paste0("Polls/",election,"_trends_intent_names",".csv"))
  
  trends$Candidate <- gsub( "^[^ ]* ", "", trends$keyword )
  
  trends.t <- trends %>%
    dplyr::arrange((date)) %>% 
    dplyr::group_by(Candidate) %>% 
    dplyr::mutate(mean.7 = frollmean(hits, 7),
                  mean.30 = frollmean(hits, 30)) %>% 
    dplyr::ungroup()
  
  trends.t <- merge(trends.t, calibration.weight, by = "Candidate", all.x = TRUE)
  
  trends.t[,c("mean.7","mean.30")] <- trends.t[,c("mean.7","mean.30")] * trends.t$Calibration
  
  trends.t <- trends.t %>%
    dplyr::arrange((date)) %>% 
    dplyr::group_by(date) %>% 
    dplyr::mutate(total.7 = sum(mean.7),
                  total.30 = sum(mean.30)) %>% 
    dplyr::ungroup()
  
  trends.t$per.7 <- trends.t$mean.7 / trends.t$total.7
  trends.t$per.30 <- trends.t$mean.30 / trends.t$total.30
  
  trends.t$per.7 <-  trends.t$per.7 * (1 - poll.model$Polls[poll.model$Candidate == "Otros"])
  trends.t$per.30 <-  trends.t$per.30 * (1 - poll.model$Polls[poll.model$Candidate == "Otros"])
  trends.t$avg <- (trends.t$per.7 + trends.t$per.30) / 2 
  
  trends <- trends.t[,c("date","Candidate","avg")]
  
  return(trends)
}

trends.1 <- trend.search(keywords[1])
trends.2 <- trend.search(keywords[2])
trends.3 <- trend.search(keywords[3])
trends.4 <- trend.search.name(trend.names)


## Clean up trends
trends.1 <- na.omit(trends.1)
trends.2 <- na.omit(trends.2)
trends.3 <- na.omit(trends.3)
trends.4 <- na.omit(trends.4)
trends.5 <- trends.4
trends.6 <- trends.4

trends <- list(trends.1, trends.2, trends.3, trends.4, trends.5, trends.6)
trends <- Reduce(function(x, y) merge(x, y, all=TRUE, by = c("date","Candidate")), trends)
trends$value <- rowMeans(trends[,c(3,4,5,6,7,8)], na.rm = TRUE)
trends <- trends[,-c(3,4,5,6,7,8)]

trends.filtered <- trends[trends$date > mdy("01/01/2023"),]


## Plot Google Trends
data.plot <- ggplot(trends.filtered, aes(x=date, y=value, color=Candidate)) +
  geom_point() +
  stat_smooth(aes(fill=Candidate)) +
  scale_fill_manual(values=group.colors) +
  scale_color_manual(values=group.colors) + scale_y_continuous(labels = scales::percent) + 
  theme(legend.position="top", legend.title = element_blank(), legend.box = "horizontal", plot.title = element_text(hjust = 0.5)) + 
  ggtitle(paste0("Google Trends for 2023 ",election," Local Election")) +
  xlab("Date") + 
  ylab("Search Interest %") +
  guides(color = guide_legend(nrow = 1, byrow = TRUE)) 


my_plot_3 <- ggdraw() +
  draw_plot(data.plot) +
  draw_image("https://raw.githubusercontent.com/PoliticaConDato/Elecciones-2022/main/data_2nda/PoliData.png", scale = 0.07, x = 0.475, y = -0.47 ) +
  draw_text("@PoliticaConDato", size = 12, x = 0.85, y = 0.03)

my_plot_3

png(paste0(election,"_Google_Trends.png"), width = 1200, height = 900, res = 120)
my_plot_3
dev.off()

## Prepare trends model output

trends.model <- trends[trends$date == max(trends$date),]
trends.model <- trends.model[,-c(1)]
colnames(trends.model) <- c("Candidate","Trends")


## Clear environment
remove(calibration.weight, data.plot, my_plot_3, trends.1, trends.2, trends.3, trends.4, trends.5, trends.6, trends.filtered, trends.model.1, trends.model.2, trends.model.3, group.colors, keywords, min.graph.date)


#############       ENSEMBLE  MODEL       ################

ensemble.model <- poll.model[poll.model$Candidate != "Undecided",]
ensemble.model <- merge(ensemble.model, trends.model, by = "Candidate", all.x = TRUE)
ensemble.model$Trends[ensemble.model$Candidate == "Otros"] <- ensemble.model$Polls[ensemble.model$Candidate == "Otros"]

ensemble.model$Ensemble <- ensemble.model$Polls*0.75 + ensemble.model$Trends*0.25
#ensemble.model <- ensemble.model[,c(2,4)]


## Create confidence interval 

model.date <- max(polls$Date)
reduced.polls <- polls
reduced.polls$days <- model.date - reduced.polls$Date 
reduced.polls$date.weight <- ifelse(reduced.polls$days < 0, 0, ifelse(reduced.polls$days > 60, 0, 1.2*(1-reduced.polls$days/60)))
reduced.polls <- reduced.polls[rev(order(reduced.polls$Pollster, reduced.polls$Date)),]
reduced.polls <- reduced.polls[reduced.polls$date.weight > 0, ]
reduced.polls <- reduced.polls[!duplicated(reduced.polls$Pollster),]
reduced.polls$weight <- reduced.polls$rating.weight * reduced.polls$error.weight * reduced.polls$date.weight
total.weight <- sum(reduced.polls$weight)
reduced.polls$weight <- reduced.polls$weight / total.weight
reduced.polls <- reduced.polls[,c("ID","weight")]

new.polls <- merge(polls, reduced.polls, by = "ID")
new.polls <- new.polls[,c("Sample","value.norm","weight","Candidate")]

candidates <- cand.vec[1:length(cand.vec)-1]


for(i in 1:length(candidates)) {
  
  poll.loop <- new.polls[new.polls$Candidate == candidates[i],]
  polls.binom <- apply(poll.loop[poll.loop$Candidate == candidates[i],c(1,2,3)], 1, function(x) rbinom(n = x[3]*1000000, size = x[1], prob = x[2]))
  output.vec <- as.numeric(unlist(polls.binom[1])) / poll.loop$Sample[1]
  
  for(j in 2:nrow(poll.loop)) {
    output.vec <- c(output.vec,as.numeric(unlist(polls.binom[j])) / poll.loop$Sample[j])
    
  }
  
  quant <- as.numeric(quantile(output.vec, c(0.05, 0.95)))
  quant <- quant*100
  inter <- paste0(round(quant[1],1),"-",round(quant[2],1))
  
  ensemble.model$Inter[ensemble.model$Candidate == candidates[i]] <- inter
  
  assign(paste("vec", candidates[i], sep = "."), output.vec)
  
}

#############       OUTPUT       ################
ensemble.model[,c("Polls","Trends","Ensemble")] <- ensemble.model[,c("Polls","Trends","Ensemble")]*100
ensemble.model <- arrange(ensemble.model, desc(Ensemble))


kable(ensemble.model, "html",
      digits=1,
      caption = paste0(election," 2023 Elections Model (% votes)")) %>%
  kable_styling(full_width = F) %>%
  footnote(number = c("Modeler: PoliData","Twitter: @PoliticaConDato",paste0("Date: ", format(Sys.Date(), "%Y-%m-%d"))))



##### FUTURE WORK BELOW #########

##### PROBABILISTIC MODEL #####

## Build dataframe
cand.vecs <- mget(paste0("vec.",candidates))
df <- as.data.frame(do.call(cbind, cand.vecs))
#df <- as.data.frame(cbind(vec.Ortiz, vec.Rojas, vec.Eder, vec.Torres, vec.Otros))
df <- df[ , order(names(df))]
candidates.sorted <- sort(candidates)
colnames(df) <- candidates.sorted
df <- reshape2::melt(df, variable.name = "Candidate")


colors.Bogota <- c(Bolivar = "#800080",Galan = "#C41C0C",  Lara = "#BFAB25", Molano = "#1E4B8F", Otros = "#2fbef2",  Oviedo = "#F029A7")

colors.Cali <- c(Eder = "#f3701b", Ortiz = "#282883", Otros = "#2fbef2", Rojas = "#e20e28" , Torres = "#BFAB25")

group.colors <- get(paste0("colors.",election))


## Plot histograms
ggplot(df, aes(x=value, fill=Candidate)) +
  geom_histogram( color='#e9ecef', alpha=0.6, position='identity') + 
  scale_fill_manual(values=group.colors) 
  
output.mar <- 2*(output.vec) -1 

# Histogram
poll.short <- poll.model[1:length(candidates),]
poll.short <- poll.short[order(poll.short$Candidate),]

data.hist <- df %>%
  filter(Candidate %in% candidates) %>%
  ggplot( aes(x=value, fill=Candidate)) +
  geom_density(alpha=0.6, color=NA) +
  scale_fill_manual(values=group.colors) +
  ggtitle("Vote distribution") +
  theme(legend.position="top", legend.title = element_blank(), legend.box = "horizontal", plot.title = element_text(hjust = 0.5), 
        axis.title.y = element_blank(), axis.text.y = element_blank(), axis.ticks.y = element_blank()) +
  ylab("") +
  xlab("% of votes") +
  scale_x_continuous(labels = scales::percent)  +
  geom_vline(data = poll.short, aes(xintercept = Polls, colour = Candidate),colour = group.colors, linetype = "dashed") +
  geom_text_repel(data = poll.short, aes(Polls,30, label = paste0(round(Polls*100,1),"%")), color = group.colors)


my_plot_4 <- ggdraw() +
  draw_plot(data.hist) +
  draw_image("https://raw.githubusercontent.com/PoliticaConDato/Elecciones-2022/main/data_2nda/PoliData.png", scale = 0.07, x = 0.475, y = -0.47 ) +
  draw_text("@PoliticaConDato", size = 12, x = 0.85, y = 0.03)

my_plot_4

png(paste0(election,"_Probability.png"), width = 1200, height = 900, res = 120)
my_plot_4
dev.off()
