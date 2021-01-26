library(bibliometrix)
library(tidyverse)

file <- "soil_necromass_fungi.bib"
M <- convert2df(file = file, dbsource = "isi", format = "bibtex")
results <- biblioAnalysis(M, sep = ";")
S <- summary(object = results, k = 10, pause = FALSE)

topAU <- authorProdOverTime(M, k = 10, graph = TRUE)
# broken whyy

# Create keyword co-occurrences network
NetMatrix <- biblioNetwork(M, analysis = "co-occurrences", network = "keywords", sep = ";")
# Plot the network
net=networkPlot(NetMatrix, normalize="association", weighted=T, n = 30, Title = "Keyword Co-occurrences", type = "fruchterman", size=T,edgesize = 5,labelsize=0.7)

threeFieldsPlot(M, fields=c("DE","AU","SC"),n=c(10,10,10))
# SC, SO, CR all interesting for 3rd field tag

# CR (cited references), SO (journal), SC (category)
M["MILTNER A, 2012, BIOGEOCHEMISTRY", "DI"]

# themes <- thematicEvolution(M, field = "ID", years = c(2010,2015), n = 250, minFreq = 2, size = 0.5, stemming = FALSE, n.labels = 1, repel = TRUE)
# plotThematicEvolution(themes$Nodes, themes$Edges, measure = "inclusion", min.flow = 0)

### SYSTEMATIC MAPPING

library(revtools)
recs <- read_bibliography("soil_necromass_fungi.bib")
alleff <- allocate_effort(reviewers = c("India", "GroupMember1", "GroupMember2"), effort = c(0.33,0.34,0.33), proportion_checked = 0.2, quiet = FALSE)
distribute_tasks(recs, alleff)

india_papers <- read_bibliography("reviewer_India.csv")
screen_abstracts(india_papers)

# read in an aggregate files
screened <- c("IM_done.csv", "GM1_finished.csv", "rGM2_yes.csv")
aggfiles <- aggregate_tasks(file_names = screened, selection_column = "screened_abstracts", reviewer_names = c("India", "GroupMember1", "GroupMember2"))
write.csv(aggfiles, "agg_screening_results.csv", row.names = F)

aggfiles <- read.csv("agg_screening_results.csv")

# find files that were only selected, or which had conflicts
sel <- rowSums(aggfiles[,54:56] == "selected", na.rm = T)
excl <- rowSums(aggfiles[,54:56] == "excluded", na.rm = T)
aggfiles2 <- cbind(aggfiles, sel, excl)

#definitely selected: ONLY selected (NOT excluded) by 1 or more people
def_sel <- aggfiles[c(aggfiles2$sel>0 & aggfiles2$excl==0),]
# conflicts: selected by one, excluded by another
conflict <- aggfiles[c(aggfiles2$sel>0 & aggfiles2$excl>0),]
library(tidyverse)
