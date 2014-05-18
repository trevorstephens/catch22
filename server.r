library(ggplot2)
library(reshape2)
library(scales)
library(cluster)
library(shiny)
library(maps)
library(geosphere)

# Read in character appearances CSV
catch22 <- read.csv("catch22.csv", stringsAsFactors = FALSE)
# Labels for chapter axes
labs <- c("The Texan","Clevinger","Havermeyer","Doc Daneeka","Chief White Halfoat","Hungry Joe","McWatt","Lieutenant Scheisskopf",
          "Major Major Major Major","Wintergreen","Captain Black","Bologna","Major — de Coverley","Kid Sampson","Piltchard & Wren",
          "Luciana","The Soldier In White","The Soldier Who Saw Everything Twice","Colonel Cathcart","Corporal Whitcomb","General Dreedle",
          "Milo The Mayor","Nately's Old Man","Milo","The Chaplain","Aaarfy","Nurse Duckett","Dobbs","Peckem","Dunbar","Mrs. Daneeka",
          "Yo-Yo's Roomies","Nately's Whore","Thanksgiving","Milo The Militant","The Cellar","General Scheisskopf","Kid Sister",
          "The Eternal City","Catch-22","Snowden","Yossarian")
# Read in locations CSV
catch22geo <- read.csv("catch22geo.csv", stringsAsFactors=F)
# Order and remove duplicates
catch22geo <- catch22geo[order(catch22geo$Time),]
catch22geo$prev <- c('none', catch22geo$Location[1:(nrow(catch22geo)-1)])
catch22geo <- catch22geo[catch22geo$Location != catch22geo$prev,]
row.names(catch22geo) <- NULL
catch22geo$prev <- NULL
# Generate great circles for awesomeness
catch22paths <- data.frame(Time=NULL, lon=NULL)
for (i in 2:nrow(catch22geo)) {
  temp <- data.frame(gcIntermediate(c(catch22geo$Lon[i-1], catch22geo$Lat[i-1]),
                                    c(catch22geo$Lon[i], catch22geo$Lat[i]),
                                    n=20, addStartEnd=T))
  temp <- cbind(time=rep(catch22geo$Time[i], 22), temp)
  catch22paths <- rbind(catch22paths, temp)
}
# POS tagged
catch22pos <- read.csv("catch22pos.csv", stringsAsFactors = FALSE, fileEncoding = "iso-8859-1")
# Easter egg for over-subsetters
yoyo <- read.csv("yoyo.csv", stringsAsFactors = FALSE)


getAppearances <- function(chapters,characters,orders,colours,clusters) {
  
  # Don't overwrite the golden base dataframe!
  catch22_in <- catch22
  labs_in <- labs
  
  # Only generate clusters if required
  if (orders == 'Cluster' | colours == 'Cluster') {
    # Generate frequency per chapter dataframe
    catch222 <- catch22_in
    catch222$Chapter <- floor(catch222$Chapter)
    catch222 <- data.frame(table(catch222$Chapter, catch222$Character))
    names(catch222) <- c('Chapter','Character','Freq')
    # Generate colocation matrix (wide format)
    colocation <- catch222[catch222$Freq != 0,]
    colocation <- data.frame(crossprod(table(colocation[1:2])))
    colocation$Character <- row.names(colocation)
    row.names(colocation) <- NULL
    # Cluster characters based on being in similar chapters
    catch2222 <- catch222
    catch2222$Freq <- as.numeric(catch2222$Freq > 0)
    catch2222 <- dcast(catch2222, Character ~ Chapter, value.var='Freq')
    ag <- agnes(catch2222[,-1], method="complete", stand=F)
    # Cut clusters out of dendogram based on number requested
    catch2222$cluster <- cutree(ag, k=clusters)
  }
  
  # Do ordering
  if (orders == 'Cluster') {
    orders <- data.frame(table(catch22_in$Character))
    orders$Var1 <- as.character(orders$Var1)
    orders$cluster <- sapply(orders$Var1, FUN=function(x) {catch2222$cluster[catch2222$Character == x]})
    levs <- orders[with(orders, order(-cluster, Freq)), ]$Var1
    catch22_in$Character <- factor(catch22_in$Character, levels=levs)
  } else if (orders == 'Frequency') {
    orders <- data.frame(table(catch22_in$Character))
    orders$Var1 <- as.character(orders$Var1)
    levs <- orders$Var1[order(orders$Freq)]
    catch22_in$Character <- factor(catch22_in$Character, levels=levs)
  } else {
    orders <- data.frame(table(catch22_in$Character))
    orders$Var1 <- as.character(orders$Var1)
    levs <- orders$Var1[rev(order(orders$Var1))]
    catch22_in$Character <- factor(catch22_in$Character, levels=levs)
  }
    
  # Do colouring
  if (colours == 'Cluster') {
    catch22_in$cols <- sapply(catch22_in$Character, FUN=function(x) {catch2222$cluster[catch2222$Character == x]})
    catch22_in$cols <- as.factor(catch22_in$cols)
  } else {
    catch22_in$cols <- sapply(catch22$Character, FUN=function(x) {log(log(log(orders$Freq[orders$Var1 == x])))})
  }
  
  # Brushing
  orders <- data.frame(table(catch22_in$Character))
  orders$Var1 <- as.character(orders$Var1)
  characters <- (orders$Var1[rev(order(orders$Freq))])[characters[1]:characters[2]]
  catch22_in <- catch22_in[catch22_in$Chapter >= chapters[1] &
                           catch22_in$Chapter <= (chapters[2] + 1) &
                           catch22_in$Character %in% characters,]
  
  num_chars <- sum(data.frame(table(catch22_in$Character))$Freq != 0)
  size <- min(12, exp(4.97 - 0.96*log(num_chars)))
  
  # Make plot!
  if (nrow(catch22_in) == 0) {
    p <- BSOD()
  } else {
    p <- ggplot(catch22_in, aes(x=Chapter, y=Character, colour=cols)) +
      geom_point(size=size, shape='|', alpha=0.8) +
      scale_x_continuous(limits=c(chapters[1],(chapters[2] + 1)), expand=c(0,0), breaks=(1:42)+0.5, labels=labs) +
      ylab(NULL) + xlab('Chapter') +
      theme(axis.text.x = element_text(colour = "black", angle = 45, hjust = 1, vjust=1.03),
            axis.text.y = element_text(colour = "black"),
            axis.title.x = element_text(vjust=5),
            plot.title = element_text(vjust=1)) +
      theme(axis.ticks.y = element_blank(),
            axis.ticks.x = element_blank()) +
      theme(plot.background = element_rect(fill = "transparent",colour = NA)) +
      theme(panel.grid.minor = element_line(colour = "white", size = 1),
            panel.grid.major = element_blank()) +
      scale_size_continuous(guide = FALSE) +
      scale_alpha_continuous(guide = FALSE)

    # Do colouring on plot
    if (colours == 'Cluster') {
      #p <- p + scale_colour_discrete(guide = FALSE)
      p <- p + scale_color_brewer(type='qual', palette='Set1', guide = FALSE)
    } else {
      p <- p + scale_colour_gradient(high='#B40404', low='#C65454', guide = FALSE)
    }
  }
  return(p)
}


getCooccurrences <- function(chapters,characters,orders,colours,clusters) {
  
  # Don't overwrite the golden base dataframe!
  catch22_in <- catch22
  
  # Only generate clusters if required
  if (orders == 'Cluster' | colours == 'Cluster') {
    # Generate frequency per chapter dataframe
    catch222 <- catch22_in
    catch222$Chapter <- floor(catch222$Chapter)
    catch222 <- data.frame(table(catch222$Chapter, catch222$Character))
    names(catch222) <- c('Chapter','Character','Freq')
    # Generate colocation matrix (wide format)
    colocation <- catch222[catch222$Freq != 0,]
    colocation <- data.frame(crossprod(table(colocation[1:2])))
    colocation$Character <- row.names(colocation)
    row.names(colocation) <- NULL
    # Cluster characters based on being in similar chapters
    catch2222 <- catch222
    catch2222$Freq <- as.numeric(catch2222$Freq > 0)
    catch2222 <- dcast(catch2222, Character ~ Chapter, value.var='Freq')
    ag <- agnes(catch2222[,-1], method="complete", stand=F)
    # Cut clusters out of dendogram based on number requested
    catch2222$cluster <- cutree(ag, k=clusters)
  }
  
  catch222 <- catch22
  catch222$Chapter <- floor(catch222$Chapter)
  catch222 <- data.frame(table(catch222$Chapter, catch222$Character))
  names(catch222) <- c('Chapter','Character','Freq')
  catch222$Chapter <- as.numeric(catch222$Chapter)
  # Brushing
  catch222 <- catch222[catch222$Chapter >= chapters[1] &
                       catch222$Chapter <= (chapters[2] + 1),]
  
  colocation <- catch222[catch222$Freq != 0,]
  colocation <- data.frame(crossprod(table(colocation[1:2])))
  colocation$Character <- row.names(colocation)
  row.names(colocation) <- NULL
  colocation <- melt(colocation, id=c('Character'))
  colocation$variable <- rep(colocation$Character[1:28], each=28)
  # Brushing
  charorders <- data.frame(table(catch22_in$Character))
  charorders$Var1 <- as.character(charorders$Var1)
  characters <- (charorders$Var1[rev(order(charorders$Freq))])[characters[1]:characters[2]]
  
  colocation <- colocation[colocation$Character %in% characters &
                           colocation$variable %in% characters,]

  # Only generate clusters if required
  if (orders == 'Cluster' | colours == 'Cluster') {
    colocation$cluster1 <- sapply(colocation$Character, FUN=function(x) {catch2222$cluster[as.character(catch2222$Character) == as.character(x)]})
    colocation$cluster2 <- sapply(colocation$variable, FUN=function(x) {catch2222$cluster[as.character(catch2222$Character) == as.character(x)]})
    colocation$cluster <- as.numeric(colocation$cluster1 == colocation$cluster2) * colocation$cluster1
    colocation$cluster1 <- NULL
    colocation$cluster2 <- NULL
  } else {
    colocation$cluster <- 1
  }
  
  colocation$alpha <- log10(9*(colocation$value / max(colocation$value))+1)
  
  # Do ordering
  if (orders == 'Cluster') {
    orders <- data.frame(table(catch22_in$Character))
    orders$Var1 <- as.character(orders$Var1)
    orders$cluster <- sapply(orders$Var1, FUN=function(x) {catch2222$cluster[catch2222$Character == x]})
    levs <- orders[with(orders, order(-cluster, Freq)), ]$Var1
    colocation$Character <- factor(colocation$Character, levels=rev(levs))
    colocation$variable <- factor(colocation$variable, levels=levs)
  } else if (orders == 'Frequency') {
    orders <- data.frame(table(catch22_in$Character))
    orders$Var1 <- as.character(orders$Var1)
    levs <- orders$Var1[order(orders$Freq)]
    colocation$Character <- factor(colocation$Character, levels=rev(levs))
    colocation$variable <- factor(colocation$variable, levels=levs)
  } else {
    orders <- data.frame(table(catch22_in$Character))
    orders$Var1 <- as.character(orders$Var1)
    levs <- orders$Var1[rev(order(orders$Var1))]
    colocation$Character <- factor(colocation$Character, levels=rev(levs))
    colocation$variable <- factor(colocation$variable, levels=levs)
  }
  
  # Do colouring
  if (colours == 'Cluster') {
    n_cols <- clusters
    #cols <- hcl(h=seq(15, 375, length=n_cols+1), l=65, c=100)[1:n_cols]
    cols <- brewer_pal(type='qual', palette='Set1')(n_cols)
    cols <- c('grey30', cols)
  } else {
    cols <- rep('#B40404', (clusters + 1))
  }
  
  # Make plot!
  p <- ggplot(colocation, aes(x=Character, y=variable, alpha=alpha)) + 
    geom_tile(aes(fill=factor(cluster)), colour='white') + 
    ylab(NULL) + xlab(NULL) +
    theme(axis.text.x = element_text(colour = "black", angle = 45, hjust = 1, vjust=1.03),
          axis.text.y = element_text(colour = "black"),
          axis.ticks.y = element_blank(),
          axis.ticks.x = element_blank(),
          panel.grid.minor = element_line(colour = "white", size = 1),
          panel.grid.major = element_blank()) +
    scale_fill_manual(values = cols, guide = FALSE) +
    scale_alpha_continuous(guide = FALSE) + 
    coord_fixed(ratio=1)
  
  return(p)
}


getTravels <- function(chapters) {
  
  # Brushing
  catch22geo_sub <- catch22geo[(catch22geo$Time > chapters[1]) & (catch22geo$Time < (chapters[2] + 1)),]
  catch22paths_sub <- catch22paths[(catch22paths$time > chapters[1]) & (catch22paths$time < (chapters[2] + 1)),]
  catch22paths_sub$alpha <- seq(0.4, 0.7, length.out=nrow(catch22paths_sub))
  
  # Make plot!
  p <- ggplot() + borders("world", colour="black", fill="lightyellow") + 
    ylab(NULL) + xlab(NULL) +
    theme(legend.position="none") + 
    theme(panel.background = element_rect(fill='skyblue')) +
    theme(panel.grid.minor = element_blank(),
          panel.grid.major = element_blank(),
          axis.ticks.y = element_blank(),
          axis.ticks.x = element_blank()) +
    scale_x_continuous(breaks=NULL) + scale_y_continuous(breaks=NULL) +
    coord_cartesian(xlim = c(-10,38), ylim=c(27,51))

  # Only try to draw locations and paths if we have any
  if (nrow(catch22geo_sub) != 0) {
    p <- p + geom_point(data=catch22geo_sub, aes(x = Lon, y = Lat), size=3, colour='red') +
      geom_point(data=catch22paths_sub[1,], aes(x = lon, y = lat), size=3, colour='red') +
      geom_path(data=catch22paths_sub, aes(x = lon, y = lat, alpha=alpha), size=.7, colour='red')
  }
  
  return(p)
}


getPos <- function(chapters,numwords,wordtype,verbtype,wordplot) {
  
  # Determine which word type(s) are required
  if (wordtype == 'Verb') {
    if (verbtype == 'All') {
      wordtype = c('V','VD','VG','VN')
    } else {
      wordtype = verbtype
    }
  }
  
  # Don't overwrite the golden base dataframe! And do required filtering
  catch22pos2 <- catch22pos[catch22pos$POS %in% wordtype,]
  catch22pos2 <- catch22pos2[catch22pos2$Time > chapters[1] & catch22pos2$Time < (chapters[2] + 1),]
  
  # Check if we have anything to plot... BSOD
  if (nrow(catch22pos2) == 0) {
    p <- BSOD()
  } else {
    # Find the top words and filter, then bin into chapters
    catch22pos22 <- data.frame(table(catch22pos2$Word))
    catch22pos22 <- catch22pos22[rev(order(catch22pos22$Freq)),]
    tops <- catch22pos22$Var1[1:numwords]
    catch22pos2 <- catch22pos2[catch22pos2$Word %in% tops,]
    catch22pos2$Time <- floor(catch22pos2$Time)
    # Order the words as factors base on frequency for proper display in stacked area
    orders <- data.frame(table(catch22pos2$Word))
    orders$Var1 <- as.character(orders$Var1)
    levs <- orders$Var1[order(orders$Freq)]
    # Count number of words per bin
    catch22pos2 <- data.frame(table(catch22pos2$Time,catch22pos2$Word))
    catch22pos2$Var2 <- factor(catch22pos2$Var2, levels=levs)
    catch22pos2$Var1 <- as.numeric(as.character(catch22pos2$Var1))
    names(catch22pos2) <- c('Chapter', 'Word', 'Freq')
    # Funny stuff to fill in missing chpaters (ie. Yossarian wasn't present)
    catch22pos2 <- dcast(catch22pos2, Word ~ Chapter, value.var='Freq')
    for (i in chapters[1]:chapters[2]) {
      if (!(i %in% names(catch22pos2))) {
        catch22pos2[,paste(i)] <- 0
      }
    }
    catch22pos2 <- melt(catch22pos2, id='Word')
    names(catch22pos2) <- c('Word', 'Chapter', 'Freq')
    catch22pos2$Chapter <- as.numeric(as.character(catch22pos2$Chapter))
    # Normalize the absolute word frequencies by the number of time Yossarian was in that chapter
    catch22_in <- catch22[catch22$Character == 'Yossarian',]
    catch22_in$Chapter <- floor(catch22_in$Chapter)
    yoyos <- data.frame(table(catch22_in$Chapter))
    yoyos$Var1 <- as.numeric(as.character(yoyos$Var1))
    catch22pos2$normed <- as.numeric(apply(catch22pos2, 1, FUN=function(x){
      as.numeric(as.character(x[3])) / yoyos$Freq[yoyos$Var1 == as.numeric(as.character(x[2]))]
    }))
    catch22pos2$normed[is.na(catch22pos2$normed)] <- 0
    # Just a bit of padding at the top of the plot for the legend
    y_max <- 1.1 * max(aggregate(normed~Chapter, data=catch22pos2, FUN=sum)$normed)

    # Make plot!
    p <- ggplot(catch22pos2, aes(Chapter, normed, colour=Word, fill=Word)) + 
      scale_color_brewer(type='qual', palette='Set1', guide = FALSE) +
      scale_fill_brewer(type='qual', palette='Set1') +
      scale_y_continuous(limits=c(0,y_max), expand=c(0,0)) +
      ylab('Relative Word Frequency') + xlab('Chapter') +
      theme(axis.text.x = element_text(colour = "black", angle = 45, hjust = 1),
            axis.text.y = element_text(colour = "black"),
            axis.title.y = element_text(vjust=.2),
            axis.title.x = element_text(vjust=5),
            axis.ticks.y = element_blank(),
            axis.ticks.x = element_blank(),
            panel.grid.minor = element_blank()) +
      theme(plot.background = element_rect(fill = "transparent",colour = NA)) +
      guides(fill = guide_legend(reverse=TRUE)) +
      theme(legend.title = element_blank(),
            legend.background = element_blank(),
            legend.direction = "horizontal", 
            legend.position = c(0, 1),
            legend.justification = c(0, 1),
            legend.text = element_text(size = 11, face = 'bold'),
            legend.key = element_rect(fill = NA, colour = "white", size = 1))
    
    # If we only have a single chapter to draw, do a bar graph instead of stacked area
    if (chapters[1] == chapters[2] | wordplot == 'Bar') {
      p <- p + geom_bar(stat="identity", alpha=0.8) +
        scale_x_continuous(expand=c(0,0.1), breaks=(1:42), labels=labs)
    } else {
      p <- p + geom_area(aes(order = Word), alpha=0.8) +
        scale_x_continuous(limits=chapters, expand=c(0,0), breaks=(1:42), labels=labs)
    }
  }

  return(p)
}


# An easter egg for those who over-subset
BSOD <- function() {
  p <- ggplot(yoyo)+
    geom_polygon(aes(X272 - 10, -X609 - 10, group = NULL), fill = 'black') +
    geom_polygon(aes(X272, -X609, group = NULL), fill = 'red') +
    geom_point(data=data.frame(x=370, y=-560), aes(x=x, y=y, group=NULL), colour='black', size=3) +
    theme(panel.background = element_rect(fill='blue4')) +
    coord_fixed(ratio=1) +
    ylab(NULL) + xlab(NULL) +
    theme(panel.grid.minor = element_blank(),
          panel.grid.major = element_blank(),
          axis.ticks.y = element_blank(),
          axis.ticks.x = element_blank()) +
    scale_x_continuous(limits=c(200,1500), breaks=NULL) + 
    scale_y_continuous(limits=c(-1200,-225), breaks=NULL)
  return(p)
}


shinyServer(function(input, output) {
    # How to exit if running in R
    cat("Press \"ESC\" to exit...\n")
    
    # A bunch of reactive functions to fetch user inputs
    getChapters <- reactive({return(input$chapters)})
    getCharacters <- reactive({return(input$characters)})
    getOrders <- reactive({return(input$orders)})
    getColours <- reactive({return(input$colours)})
    getClusters <- reactive({return(input$clusters)})
    getNumWords <- reactive({return(input$numwords)})
    getWordType <- reactive({return(input$wordtype)})
    getVerbType <- reactive({return(input$verbtype)})
    getWordPlot <- reactive({return(input$wordplot)})
    
    # Some functions to output the appropriate graphic
    output$appearances <- renderPlot({print(getAppearances(getChapters(),
                                                           getCharacters(),
                                                           getOrders(),
                                                           getColours(),
                                                           getClusters()))},
                                     width = 800, height = 600)
    output$cooccurrences <- renderPlot({print(getCooccurrences(getChapters(),
                                                               getCharacters(),
                                                               getOrders(),
                                                               getColours(),
                                                               getClusters()))},
                                       width = 800, height = 600)
    output$travels <- renderPlot({print(getTravels(getChapters()))},
                                 width = 800, height = 600)
    output$pos <- renderPlot({print(getPos(getChapters(),
                                           getNumWords(),
                                           getWordType(),
                                           getVerbType(),
                                           getWordPlot()))},
                             width = 800, height = 600)
})
