# rodr

This repository contains part of my RStudio experience, i.e., my R portfolio. None of these files are pulled from my computer since the versions here are simplified.

# campaign_cap.R

The first file "campaign_cap.R" tries to replicate the paper "Money and Politics: The Effects of Campaign Spending Limits on Political Competition and Incumbency Advantage" by Ferraz et. al. It was developed with the help of Prof. Dimas Fazio. <!--print a few reproduction outputs and graphs-->

# amazon.R

The second file "amazon.R" contains an analysis of deforestation of the Brazilian Amazon rainforest, part of my master's dissertation. Here, I will try to give an overall perspective on how deforestation behaves on The Amazon rainforest and give some explanations for these patterns. The source of the deforestation increase is PRODES, a Brazilian federal sub-organization that produces information regarding deforestation inside the so-called "Legal Amazon" i.e. municipalities that have Amazon rainforest inside them.

With a simple graph, let's see how deforestation has behaved in the region, since 2006:

![def](https://user-images.githubusercontent.com/51092062/216448491-071e5621-d084-47ef-8618-97fc915fbf31.png)

As a table, how deforestation has behaved from 2006 to 2020:

| Year | Deforestation |
|:----:|:-------------:|
| 2006 |    10899.4    |
| 2007 |    11501.9    |
| 2008 |    13301.7    |
| 2009 |    6551.6     |
| 2010 |    6336.2     |
| 2011 |    5608.2     |
| 2012 |    4455.7     |
| 2013 |    5411.0     |
| 2014 |    5126.7     |
| 2015 |    6143.7     |
| 2016 |    7266.8     |
| 2017 |    7299.2     |
| 2018 |    7386.0     |
| 2019 |    11043.9    |
| 2020 |    10587.9    |

Now looking at how this deforestation has behaved geographically since 2006: ![def_increase_mun](https://user-images.githubusercontent.com/51092062/216614481-030c1ccd-d2f0-4552-b77a-7e0a3ebaeac2.png)

It is easy to see the (in)famous *Brazilian agricultural frontier*: ![def_increase_mun_arch](https://user-images.githubusercontent.com/51092062/216621837-3c51c06e-1231-4acb-ab3f-9fc00f8443c9.png)

You don't have to trust my word! Let's see where there is new soy plantations. Let's stick with municipalities that have a trend of increase of areas for soy. What I'm going to do is see for all municipalities if their trends are increasing or decreasing. As done on the .R, if decreasing, I dropped the municipality. Below I plot the list of municipalities with soy production rising:
![soy](https://user-images.githubusercontent.com/51092062/217083509-e4f29609-a7f5-4150-9477-088da34e110f.png)

Now, it's easy to see how this area is where we have the most part of the municipalities with higher deforestation rate:
![frontiermove](https://user-images.githubusercontent.com/51092062/217083689-6e553eaa-306c-4f0e-9ef9-0301a7e8f93d.png)


Now let's go to the "developed world" and study the export (import, from an European country perspective) of Brazilian primary products: ![primary](https://user-images.githubusercontent.com/51092062/216446120-b742edea-fb8e-4f0d-ac59-94993944693f.png)

As visible, a few countries "carry" this burden, such as Spain, Netherlands, France, and Russia. Quantifying how this global chain of primary products affects deforestation directly is, however, out of the boundaries of the present exploration. Still, we can suppose (and the literature corroborates that) that, at least to some degree, the food on the plate of a Spanish comes with CO2 and biodiversity loss.

# summit.R

The last file "summit.R" tries to see if there is some relationship between environmental conferences/summits and the raise of radical leftist perspectives regarding environmental issues.

None of the .xlsx, .txt and .csv used on these codes are in the repository. If you want, you can request them to me!
