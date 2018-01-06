# GroundControl

<p>Welcome to the "Ground Control" project.<br/><br/>

My goal is pretty simple. I want to investigate all aspects of the NFL running game to an extent that is unreasonably excessive. <br/><br/>

You will not need to know one iota of statistics in order to follow <a href="http://forever-peace.github.io/Ground_Control/">the website</a>. We will provide everything you need to know as it comes up, in plain English.</p>

<h2>The Plan</h2>
<p>I have been poking around NFL rushing data since early 2016. It was time for me to write it up for other folks to enjoy too. <a href="http://forever-peace.github.io/Ground_Control/contents/#chapters">The chapters</a> will introduce at least one major new idea, and will generally be pretty long. Most chapters will also have <a href="http://forever-peace.github.io/Ground_Control/contents/#apps">an "app" or two</a> that will allow everybody the explore that topic themselves (and will hopefully make it easy to discover cool things for the rest of us to check out). All apps are programmed in Shiny - they're snippets of packaged visualization, analysis, and simulation code wrapped up in a pretty UI and distributed for free (everything is open-source), run through the R statistical program. And finally, between chapters, I will occasionally post <a href="http://forever-peace.github.io/Ground_Control/contents/#quick-hits">"quick hits" of cool stats or stories</a> that are related to the chapter topic.</p>

<p>A chapter summary of the main points can be found at the end of each of the major posts.<br/><br/>

The table of contents (<a href="http://forever-peace.github.io/Ground_Control/contents/">HERE</a>) will be updated with links to the new chapters.<br/><br/>

All figures can be batch-downloaded <a href="https://github.com/Forever-Peace/Ground_Control/tree/master/img">through the github page for the website</a> if you want to download the lot of them.</p>

<h2>The Data</h2>
<p>Unless otherwise specified, I will be working with every regular-season rushing attempt by a running back from the six years between 2010 and 2015. Every other position has been removed (for now). In all, I have a database of about 71,000 individual rushing attempts.<br/><br/>

The data is drawn from the official NFL JSON feed, through the wonderful <a href="https://github.com/BurntSushi/nfldb/wiki">nfldb</a> python package. This should reflect the official scorekeeping of the NFL.<br/><br/>

This gives me access to play-by-play data on a whole host of features, from field position to down and distance to stadium to clock time. What I do NOT currently have access to is anything relating to play charting: formations, path taken, location of initial contact, broken tackles etc. Drop a line if you can hook me up.</p>

<h2>The Commitment to Open Data</h2>
<p>I will be using <a href="http://www.howtogeek.com/180167/htg-explains-what-is-github-and-what-do-geeks-use-it-for/">GitHub</a> to publish the scripts, files, and data used for this project. There are two "branches" to this project: one for the website (yes, the website is also open access - you can see the source code that generated every page, or even push recommended typo fixes and clarifications), and one for the data and analysis scripts.<br/><br/>

The entirety of the data is already available, in full, on the <a href="https://github.com/Forever-Peace/GroundControl">Ground Control GitHub master Repository</a> (the "rushing_data_stack.csv" in the main folder). It, and the apps, have now recently been updated for the 2016 season.<br/><br/>

Every time I post a chapter, I will also publish the "R" script I used to generate the major findings. This github repository contains those scripts. Direct links to chapter scripts will be kept in a rolling list in the table of contents for the website (<a href="http://forever-peace.github.io/GroundControl/contents/#scripts">HERE</a>).<br/><br/>

Further, all of the interactive apps will also be available, for free, through GitHub. The source code will be available under the "Chapters/shinyapps" folder, and the apps themselves can be downloaded and run automatically with just a single command (see below).</p>

<h2>Using the Interactive Apps</h2>
<p>Because website hosting logistics can be difficult and expensive, all apps will be distributed through GitHub. Using them is extremely easy, even if you have minimal computer knowledge. More detailed step-by-step instructions are <a href="http://forever-peace.github.io/Ground_Control/apps/install_apps/">here</a>. But briefly, here are the prerequisites:<br/>
1) <a href="https://mran.revolutionanalytics.com/open/">Download and install R from this link.</a> R is a free statistical analysis software.<br/>
2) <a href="https://www.rstudio.com/products/rstudio/download/">Download and install RStudio from this link.</a> RStudio is a useful interface for R, but more importantly, it enables built-in automatic support for the plugin the apps are built with, called “shiny”.<br/>
3) Update the packages you need. Open RStudio, and in the console, enter:</p>
<pre><code>install.packages("ggplot2")
install.packages("shiny")
install.packages("reshape2")
install.packages("FNN")
</code></pre>
<p></p>That’s it. After you do these two things, whenever I publish an app, I will give you two lines of code. <b>To run the app, you'll just need to start RStudio, then copy-paste that code into the console and hit enter.</b> The app will download and run automatically, from within the RStudio program.</p>

<p>Here is an example from the <a href="http://forever-peace.github.io/Ground_Control/ch2/">chapter 2</a> <a href="http://forever-peace.github.io/Ground_Control/apps/rbdist/">"player distribution" app</a>:</p>
<pre><code>library("shiny")
runGitHub("Forever-Peace/GroundControl", subdir = "Chapters/shinyapps/rb_dist/")
</code></pre>

<p>The first line activates the "shiny" plugin that runs the app. The second line downloads the app and runs it through RStudio. You can go to the Github page to see exactly the code that is run for the app if you'd like to make sure there is no funny business (in this case, <a href="https://github.com/Forever-Peace/GroundControl/tree/master/Chapters/shinyapps/rb_dist">here</a>). </p>
