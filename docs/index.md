---
title: "Activity 3: Bibliometrics and Systematic Review"
author: "Dr. India Mansour"
date: "January 2021"
output: 
  html_document: 
    keep_md: yes
knit: (function(input_file, encoding) {
  out_dir <- 'docs';
  rmarkdown::render(input_file,
 encoding=encoding,
 output_file=file.path(dirname(input_file), out_dir, 'index.html'))})
---



## Overview

This activity is an introduction to some techniques and packages that can be used to conduct a systematic review of the literature. We are going to use these techniques to investigate literature about **soil carbon**. We will be using the 'research weaving' framework, proposed by Nakagawa *et al.* 2019, including bibliometrics and systematic mapping. From their abstract (emphasis mine):

> "Research weaving provides a novel framework that **combines bibliometrics and systematic mapping** to inform the development of a field, the influence of research papers and their interconnections, and to visualize content across and within publications."

> "Research weaving has the potential to provide a more efficient, in-depth, and broad synthesis of a research field, to identify **research biases, gaps, and limitations**. Such insights have the potential to inform ecological and environmental **policy** and **communicate research findings to the general public** in more effective ways then are typically done in current research syntheses."

You can learn more about research weaving by reading their paper [here](https://doi.org/10.1016/j.tree.2018.11.007).

You will begin by developing a research question, then using Web of Science [WoS](https://apps.webofknowledge.com/WOS_GeneralSearch_input.do?product=WOS&search_mode=GeneralSearch&SID=D5U7NnnEhob17JVozzU&preferencesSaved=) to conduct a search of the literature. You must have a VPN set up in order to use WoS from home -- if you haven't done so yet, follow the instructions from the FU library [here](https://www.zedat.fu-berlin.de/VPN).

Remember that your group must have your research question and search string approved before continuing! 

You will then export the search results as a .bib file. This will be used to conduct an exploratory bibliometrics analysis using the `bibliometrix` package  (perhaps you remember this if your took Ökologie Basismodul in June). 

You will then develop inclusion/exclusion criteria and use the `revtools` package to screen literature.

## Developing a research question and search string

### 1. Find a topic with your group

Your first task is to conduct a literature search using Web of Science (WoS) relating to **carbon cycling, soil organic matter and/or decomposition in soils**. You may wish, for example, to explore literature about necromass, to investigate carbon dynamics in desert soils, or to investigate how drought affects soil organic matter degradation. Please discuss your topic with your group and find a topic that is interesting for all of you. 

### 2. Come up with a research question

You should then come up with a research question. This should be something general enough that it can be addressed by a literature search. For example, "How does drought affect soil organic matter decomposition?" or "How do fungi contribute to necromass dynamics in soil?".

### 3. Start searching...and develop a search string

You will have to create your own search string, with hits in the range of 100-300 articles. Your goal is to create a search string that will capture *all* of the papers that are relevant for your research topic. 

Here are some examples of search strings:

> ("soil organic matter"  OR "soil organic carbon"  OR "organic matter")  AND (decomposition  OR degradation)  AND drought

this search string uses quotation marks "" to ensure that only articles with the entire phrase "soil organic matter" appear in the results. It also uses multiple words, e.g. 'decomposition' and 'degradation' to describe the same thing. This increases the chances of finding all the relevant articles.

> (soil  AND necromass AND fung*)

this search string uses an asterisk * so that all results starting with 'fung' are shown (e.g. fungi, fungus, fungicide)

You can read about the operators (e.g. AND and OR) that you must use in your search string [here](https://images.webofknowledge.com/images/help/WOS/hs_search_operators.html) and there are more [WoS search tips here](https://clarivate.libguides.com/woscc/searchtips).

### 4. Refine your search and your search string

There is *a lot* of literature about soil carbon -- make sure that you refine your search to get fewer results. You should first try to reduce the number of results by making your research question more specific. For example, focus on a specific type of ecosystem (tropical forest or grassland). If necessary, you can further reduce the number of search results by using the 'Refine Results' categories on WoS. For example, you might narrow the years of the search or exclude review papers. Write down what you do! And remember, these decisions will later influence how you interpret your results!

**Once you have decided on a topic, you must have it approved by me before you and your group continue.**

## Bibliometric Analysis

Once your topic is approved, you may begin the bibliometric analysis. 

### 1. Produce a BibTeX file in Web of Science

I have recorded a tutorial video showing how to conduct a literature search in the Web of Science and export your results as a .bib file. This is available on [VBrick](https://youtu.be/4wwoNNRR94A) and Blackboard. Follow along with the video and produce your BibTeX file.

### 2. Begin your bibliometric analysis

Now you can begin working in R. Create a new R script for the project. If you have not installed the `bibliometrix` package yet, please do so now, like this:
`install.packages("bibliometrix", dependencies=TRUE)` and then load the package: 


```r
library(bibliometrix)
```

[Documentation can be found here](https://cran.r-project.org/web/packages/bibliometrix/bibliometrix.pdf).
Once you have created a .bib file, you must then load the data in R using the `bibliometrix` package and conduct a bibliometrics analysis. Follow along with [this tutorial](https://www.bibliometrix.org/vignettes/Introduction_to_bibliometrix.html). We will begin with the section [Data loading and converting](https://www.bibliometrix.org/vignettes/Introduction_to_bibliometrix.html#data-loading-and-converting). You might have to wait a few minutes for the bibliographic dataframe to be generated...check the console!

Once the dataframe has loaded, check it out. What are the column names?
Hint: try using `colnames()`.
These 2-letter codes are called 'field tags' and there is a complete list [here](https://www.bibliometrix.org/documents/Field_Tags_bibliometrix.pdf). You will need to use these later.

### 3. Perform a descriptive analysis

Continue following the tutorial by performing a descriptive [bibliometric analysis](https://www.bibliometrix.org/vignettes/Introduction_to_bibliometrix.html#bibliometric-analysis). Run the `summary` function. Look at the results of the summary in the console. Write down the timespan and the number of sources and documents. What are the 5 top-cited papers in your collection? Copy/paste the DOIs into Web of Science and check out the abstracts (hint: some of these might be good papers to cite in the introduction of your Protokoll). Now run the `plot` function. Save the plots showing 'Most Productive Countries' and 'Annual Scientific Production'.



You can quickly display the DOI or publication year (or any information in the dataframe) by giving the paper/row name and the field code, like this:


```r
M["MILTNER A, 2012, BIOGEOCHEMISTRY", "DI"]
```

```
## [1] "10.1007/s10533-011-9658-z"
```

```r
M["MILTNER A, 2012, BIOGEOCHEMISTRY", "PY"]
```

```
## [1] 2012
```


### 4. Investigate authors, keywords and their relationships

Now let's find out how your research topic developed over time. Follow along with the tutorial section [Top-Authors’ Productivity over the Time](https://www.bibliometrix.org/vignettes/Introduction_to_bibliometrix.html#top-authors-productivity-over-the-time). Save this plot (for some datasets this does not work well -- if it doesn't work, do not despair, use the 'most productive authors' plot from before). Who are the authors that are publishing the most nowadays? Who were the authors publishing the most early on? Are there any authors that have been publishing regularly for a long period of time? (Hint: you should check out some of these authors for references for your Protokoll.)

Now let's look at how keywords in the papers are connected. This will give us some insight about how sub-topics within your topic are related to each other. Follow the tutorial to plot [keyword co-occurrences](https://www.bibliometrix.org/vignettes/Introduction_to_bibliometrix.html#keyword-co-occurrences). Are there clusters of keywords? What ideas are connected to one another? What does this tell you about your topic? Do this reveal something about the way that groups of people think/study this topic?

![](/Users/fastloris/Dropbox/Vertiefungsmodul course materials/WS2021/Praktikum/VM_biblio/docs/index_files/figure-html/unnamed-chunk-4-1.png)<!-- -->

Finally, we will investigate the relationships between authors, keywords and WoS categories. You can choose any 3 field tags that you would like -- looking at journals (sources) or common cited references could also be interesting. We will do this using a three-fields plot, with the function `threeFieldsPlot`. Search in help search bar in R to read about this function and see an example of its use. Try clicking on one of the keywords or authors in the plot below.

<!--html_preserve--><div id="htmlwidget-60c44418223549d8f688" style="width:900px;height:600px;" class="sankeyNetwork html-widget"></div>
<script type="application/json" data-for="htmlwidget-60c44418223549d8f688">{"x":{"links":{"source":[0,3,4,5,6,7,9,0,1,2,3,8,9,0,1,2,3,8,9,0,3,4,5,6,7,0,3,4,5,6,7,9,0,3,4,5,6,7,9,0,2,4,5,4,5,6,3,5,9,10,11,12,13,14,15,17,18,19,10,11,12,14,15,16,17,19,11,12,16,10,19,12,14],"target":[10,10,10,10,10,10,10,11,11,11,11,11,11,12,12,12,12,12,12,13,13,13,13,13,13,14,14,14,14,14,14,14,15,15,15,15,15,15,15,16,17,17,17,18,18,18,19,19,19,20,20,20,20,20,20,20,20,20,21,21,21,21,21,21,21,21,22,22,22,24,24,25,29],"value":[1,2,1,2,2,2,1,1,3,3,1,4,1,1,2,2,2,2,1,1,2,2,1,1,1,1,1,3,1,1,2,1,2,1,1,2,2,1,1,1,1,1,2,1,3,2,1,2,1,5,2,1,5,1,4,2,3,2,2,3,4,3,1,1,1,1,1,2,3,2,1,1,1],"group":[null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null]},"nodes":{"name":["necromass","fungi","decomposition","soil organic matter","amino sugars","microbial necromass","soil organic carbon","microbial","carbon cycling","microbial biomass","liang c","fernandez cw","kennedy pg","kuzyakov y","yang y","zhang x","read dj","li x","he h","wang x","agriculture","environmental sciences \\& ecology","plant sciences","","biodiversity \\& conservation","mycology","","","","physical geography"],"group":["necromass","fungi","decomposition","soil organic matter","amino sugars","microbial necromass","soil organic carbon","microbial","carbon cycling","microbial biomass","liang c","fernandez cw","kennedy pg","kuzyakov y","yang y","zhang x","read dj","li x","he h","wang x","agriculture","environmental sciences \\& ecology","plant sciences","","biodiversity \\& conservation","mycology","","","","physical geography"]},"options":{"NodeID":"Nodes","NodeGroup":"group","LinkGroup":"group","colourScale":"d3.scaleOrdinal(d3.schemeCategory20);","fontSize":12,"fontFamily":null,"nodeWidth":30,"nodePadding":10,"units":"","margin":{"top":null,"right":null,"bottom":null,"left":null},"iterations":32,"sinksRight":true}},"evals":[],"jsHooks":[]}</script><!--/html_preserve-->

Make sure to save all of your results and plots from this exercise. When you are finished, I would recommend spending some time as a group writing a few sentences about each plot -- how do you interpret the results that you've generated? This will make it easier for you to write your Protokoll later.

## Systematic Mapping

The next part of this activity involves screening your papers and extracting some information from them to create a systematic map. 

### 1. Inclusion/Exclusion Criteria

The first step is to create your inclusion/exclusion criteria. In other words, what are the characteristics of papers that you will screen?

You should revisit your research question and decide what kind of articles and data are useful to answer your question. Taking my soil/necromass/fungi search as an example, I might be interested in how fungal diversity and activity affects the decomposition or stabilization of necromass in soil. So, I would only include papers that study both necromass and fungal communities (diversity or functions). I would exclude studies that only investigate bacteria and not fungi or that investigate leaf litter but not necromass.

Make sure that you write down your inclusion/exclusion criteria!! This is very important for reproducibility and must be written in the 'Methods' section of your Protokoll.

### 2. Dividing up the papers between your group

Now, back to R, but not for long! We will use `revtools` to screen the abstracts.
Load the revtools package and your BibTeX file:


```r
library(revtools)
## recs <- read_bibliography("soil_necromass_fungi.bib")
```

Now you're going to split the papers up between group members using the `allocate_effort` function. This lets you decide the proportion of articles per person. To have a robust systematic review, it is necessary for some of the articles to be screened by more than one person. This way you can make sure that everyone is applying the inclusion/exclusion criteria in the same way.


```r
alleff <- allocate_effort(reviewers = c("India", "GroupMember1", "GroupMember2"), effort = c(0.33,0.34,0.33), proportion_checked = 0.2, quiet = FALSE)
```

```
## Proportion of articles per reviewer:
## India: 0.39
## GroupMember1: 0.4
## GroupMember2: 0.4
## Proportion of articles reviewed by:
## 1 person: 0.81
## 2 people: 0.19
## 3 people: 0
```

```r
## distribute_tasks(recs, alleff)
```

Some R magic happened behind the scenes when you ran the second line of code. Take a look in your working directory (or in the "Files" tab in RStudio). You should find that there is now a .csv file for each member of your group! One group member should send all the files created in this step to the other group members. 

### 3. Abstract Screening

Now you're ready to begin abstract screening. Don't worry, you won't be in RStudio too much longer. Now you get to use the shiny web interface! Load your file in R and then use the `screen_abstracts` function. A new window will open up where you can screen your abstracts. Read the title and abstract of the paper, and, based on your inclusion/exclusion criteria, decide whether to include or exclude the article. Click on "Select" or "Exclude" as appropriate. When you are finished, make sure to save your data!! It will not save automatically!


```r
india_papers <- read_bibliography("reviewer_India.csv")
screen_abstracts(india_papers)
```

### 4. Creating a file with the selected articles

After everyone has finished screening articles, you must now put the files back together again. This can be done using the `aggregate` function in revtools. Make sure that the number of columns and column names match in each .csv file, or this won't work.


```r
screened <- c("IM_done.csv", "GM1_finished.csv", "GM2_yes.csv")
aggfiles <- aggregate_tasks(file_names = screened, selection_column = "screened_abstracts", reviewer_names = c("India", "GroupMember1", "GroupMember2"))
write.csv(aggfiles, "agg_screening_results.csv", row.names = F)
```

Now we will look for articles that were selected by one or more people. These will be articles that we will use for systematic mapping. We must also look at articles where there were 'conflicts' -- where one person 'selected' and another person 'excluded' the same article. Your group should talk about each article with conflicts and make a decision about whether or not to include this in the systematic mapping.


```r
sel <- rowSums(aggfiles[,54:56] == "selected", na.rm = T)
excl <- rowSums(aggfiles[,54:56] == "excluded", na.rm = T)
aggfiles2 <- cbind(aggfiles, sel, excl)
#definitely selected: ONLY selected (NOT excluded) by 1 or more people
def_sel <- aggfiles[c(aggfiles2$sel>0 & aggfiles2$excl==0),]
# conflicts: selected by one, excluded by another
conflict <- aggfiles[c(aggfiles2$sel>0 & aggfiles2$excl>0),]
```

The dataframe def_sel contains the selected papers. If needed, you can add rows from the 'conflict' dataframe to the def_sel dataframe using 'rbind'. After you have discussed all the 'conflict' articles and decided whether to include them or not, you will create a .csv file with all the selected papers.


```r
write.csv(def_sel, "selected_articles.csv", row.names = F)
```

### 5. Data Extraction

Now you will begin extracting data from your articles. You should choose a few pieces of information that are relevant for *your* topic. Some ideas might include: article type, ecosystem type, organism studied...Try not to make this too complicated, or it will be very time-consuming. Examples of questions that you might try to answer with the data that you extract: Were most of the studies conducted in temperate grasslands or tropical forests? Did the researchers mostly focus on soil microbes or soil animals?

Open "selected_articles.csv" using Excel, GoogleSheets, or whatever works for you and your group. Delete all columns except the publication year, journal, title, authors and DOI. Create new column names with the name of the data that you and your group will extract from the articles. Work together to extract data from the articles. You will have to search for them using the DOI. (Sometimes it works best when you copy the DOI and the string "doi" in Google.)

### 6. Data visualization

Use the skills that you gained during exercise 1 to create 1-2 figures using using ggplot. Your goal here is to show some information about your group of articles using the data that you extracted from the articles.

Help! --> Refer back to [R for Data Science](https://r4ds.had.co.nz/data-visualisation.html). The [R Graph Gallery](https://www.r-graph-gallery.com/index.html) has information about how to create a ton of different type of figures in R.

### 7. Congratulations, you made it! Now it's Protokoll time.

Okay, now you've got some results! It's time to discuss your outputs with your group and work on writing your Protokoll. You will need to include the following sections:

- Introduction: explain some background information about your topic, cite 3-5 peer-reviewed articles

- Methods: include your search string, any refinements that you made to your search in WoS, your inclusion/exclusion criteria and information about the packages that you used in R (remember to cite the package authors!)

- Results: provide some general information (how many WoS results did you have? How many articles remained after screening?); include all the bibliometrics and systematic mapping figures that you produced along with 1-2 sentences describing each figure

- Discussion: write about how your interpret your results. This could be a good place to compare your findings to an earlier review paper on a similar topic.

Cheers!

## References

Aria, M. & Cuccurullo, C. (2017) bibliometrix: An R-tool for comprehensive science mapping analysis, Journal of Informetrics, 11(4), pp 959-975, Elsevier.

MJ Westgate (2019) revtools: An R package to support article screening for evidence synthesis. Research Synthesis Methods http://doi.org/10.1002/jrsm.1374 




