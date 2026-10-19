# Assessing online sentiment towards the Mojave desert tortoise using Natural Language Processing
### Authors: Madeline P. Standen, L. Mae Lacey, Alexander J. Robillard, Amy C. Collins, Kaitlyn Shaw, James Danoff-Burg, Elissa Olimpi, Justin Suraci, Brett G. Dickson
An [National Fish and Wildlife Foundation](https://www.nfwf.org/) project in collaboration with the [Conservation Science Partners](https://www.csp-inc.org/) [T&E Lab](https://csp-inc.org/t-e-lab/) and the [Living Desert Zoo and Gardens](https://www.livingdesert.org/) using natural language processing ("NLP") for analysis of content posted on Twitter/X and online news articles related to the Mojave desert tortoise (</i>Gopherus agassizii</i>).

## 🐢x🐦: Repository Structure <a name="repo"></a>
```bash
├── GDELT-article-extraction # Article extraction workflows
├── Tweet-extraction  # Tweet scraping 
├── Preprocessing-and-analyses  # Data preprocessing, analyses, and figures
├── .gitignore
└── README.md
```

## :information_source: About this Project <a name="about"></a>
Public attitudes towards a species can determine a community’s willingness to support conservation management actions. As such, outreach can play a central role in determining conservation outcomes. User-generated, open source content represents a rich resource for studying public perceptions of and engagement with conservation topics, and can provide actionable insights for designing outreach efforts. Here we analyze social media posts on Twitter/X (hereafter ‘Twitter’) and news articles published from 2012-2024 related to the Mojave desert tortoise (</i>Gopherus agassizii</i>, hereafter ‘tortoise’). This work represents a practical framework that is broadly transferable and can be leveraged across conservation contexts to evaluate public responses to policy decisions, identify emerging narratives, and detect key events or inflection points in conservation discourse. Ultimately, by embedding NLP-driven assessments into conservation practice, we can help transform outreach from static communications into a more dynamic, responsive, and data-informed dialogue.

## :computer: Analysis
### Step 1: Compile relevant social media posts
We successfully collected Twitter/X (hereafter ‘Twitter’) data using the Twitter API (Roesslein 2020). Our process involved authenticating with the Twitter API and systematically retrieving tweets based on predefined keywords with dates ranging from January 2012 to August 2024. At a minimum, data included tweet text, unique user id, and date. Search queries excluded retweets and non-English tweets. Specific keywords used were “Mojave desert tortoise”, “Gopherus agassizii”, “desert tortoise”, and related variations. We initially pulled 24,380 tweets. Following data filtering and cleaning, our final dataset included 19,192 tweets. 

### Step 2: Compile relevant news articles
To gather news media data we leveraged the Global Database of Events, Language, and Tone (GDELT) library (GDELT Project, 2026), which allowed us to scrape all web sources in the GDELT database for article links relevant to the Mojave desert tortoise. If a URL provided by GDELT did not return any information (i.e., the link was broken), we queried the Wayback Machine API (archive.org) for an archived snapshot of that article. We collected the full text of articles posted from 2013 (the year in which daily article data became available from GDELT) through 2024. Specific keywords used for querying articles were “Mojave desert tortoise”, “Gopherus agassizii”, “desert tortoise”, “tortoise”, “Mojave”, “desert”, and related variations. We initially accessed 90,921 articles. Following data filtering and cleaning, our final dataset included 956 articles.

### Step 3: Perform sentiment analysis
To analyze the sentiment of tweets and news articles, we employed a sentiment analysis model called the Valence Aware Dictionary and sEntiment Reasoner (VADER) (Hutto and Gilbert 2014). We specifically chose VADER for its effectiveness in analyzing social media content (Fink et al. 2020). The VADER lexicon (i.e., dictionary of words with assigned sentiment scores) consists of 6,338 English words, ASCII emoticons, initialisms, and slang terms with assigned valence scores for sentiment intensity ranging from -4 to +4. VADER can provide a compound score that reflects the overall sentiment of the text and applies “context-awareness” to its sentiment scoring by considering valence shifters (e.g., “not” in the phrase “not good”), punctuation, capitalization, and degree modifiers. Scores which range from -0.05 to 0.05 are considered neutral, while scores above 0.05 or below -0.05 are considered ‘positive’ or ‘negative’, respectively. After calculating the raw VADER score, we categorized the sentiment of each tweet and article as negative (score ≤ −0.05), neutral (−0.05 < score < 0.05), or positive (score ≥ 0.05). To better understand how sentiment relates to stance and attitude in this dataset, we conducted a qualitative content analysis on a subset of 50 tweets (0.2% of the full Twitter dataset) which contained high frequency terms. Two authors processed tweets independently based on metrics of ‘stance’ (1 = pro-tortoise, 0 = neutral/ambiguous, -1 = anti-tortoise) and ‘attitude’ (1 = optimistic, 0 = neutral/ambiguous, -1 = pessimistic). 

### Step 4: Perform topic modeling
We used Latent Derelict Allocation (LDA, Blei et al. 2003) modeling to identify which narratives about the tortoise were most salient across online discourse. LDA models use mixtures of words present in a body of texts to identify distinct topics and assign a topic to each text. We created separate LDA models for articles and tweets, and used these models to assign each article and tweet to one of seven platform-specific topics. 

### Step 5: Quantify baseline sentiment and temporal trends in sentiment, engagement, and activity
To quantify the baseline sentiment of Twitter posts and news articles regarding the tortoise, we used a pairwise Chi-square test to compare the number of tweets and articles in each sentiment category (positive, neutral, negative). Mann-Kendall trend tests were conducted to assess the presence of weekly trends in sentiment, engagement, and activity across Twitter and news outlets from 2012–2024. To examine trends of online content within salient topics, we visualized monthly trends in proportional contribution and deviation from mean sentiment by topic, for both tweets and news articles. Finally, to identify key events impacting online discourse through time, we identified weeks that were outliers for sentiment, engagement, or activity and examined content of tweets and articles posted in those weeks.


## References
Blei, David M., Andrew Y. Ng, and Michael I. Jordan. 2003. “Latent Dirichlet Allocation.” Journal of Machine Learning Research, no. 3: 993–1022.

Fink, Christoph, Anna Hausmann, and Enrico Di Minin. 2020. “Online Sentiment towards Iconic Species.” Biological Conservation 241 (January): 108289. https://doi.org/10.1016/j.biocon.2019.108289.

GDELT Project. 2026. “Global Database of Events, Language, and Tone (GDELT).” https://www.gdeltproject.org/.

Hutto, C., and Eric Gilbert. 2014. “VADER: A Parsimonious Rule-Based Model for Sentiment Analysis of Social Media Text.” Proceedings of the International AAAI Conference on Web and Social Media 8 (1): 216–25. https://doi.org/10.1609/icwsm.v8i1.14550.

Roesslein, J. 2020. “Tweepy: An Easy-to-Use Python Library for Accessing the Twitter API.” GitHub repository. Version v2. Tweepy. https://github.com/tweepy/tweepy.
