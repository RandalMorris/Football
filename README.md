# Fantasy Football Projections

Orginal source from Fantasyfootballanalytics.net github

Fix and Modified by Randal Morris(Randal.Morris@uah.edu)

What this does.
	It will scrape projection data from multiple sources then create a projection based on those sources.
	Week by week and seasonal projections available for fantasy and Daily Fantasy games.
	Value Gap Analysis to idenify over and under rated players

What it doesn't do.
	Guarentee winners.

What I have done to make this work/better.
	Orginal code was ok at best. Was outdated and not updated to sources websites.
	I cleaned up the code to make it work...seasonal data
	I used a rough weekly data scrape file included that someone created to expand the week by week.
	Used K-means instead of Mclust's model based for clustering
	
What needs done still.
	Optimize code
	Add more sources
