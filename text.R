#This is a file to contain lengthy text for the UI

app_title_text<-"US Covid 19 Deaths Dashboard"

app_purpose_text<-"The purpose of this app is to allow users to explore the CDC dataset on Covid deaths, carryout some basic exploratory data analysis, generate supervised learning models including a linear regression model, a regression tree model and a random forest model, to make predictions about Covid 19 deaths, and to subset and download data from the dataset as desired."

about_data_text<-"The data for this app comes from the CDC's api and is a dataset focused on weekly counts of deaths by jurisdiction, age group, week, and year for covid a a few other respiratory illnesses. More information about the data can be found at the [CDC COVID Deaths Data Page](https://data.cdc.gov/NCHS/Provisional-Death-Counts-for-Influenza-Pneumonia-a/ynw2-4viq). As the data is being sourced from an active dataset that is updated often, the data will be updated each time a user accesses the app."

tab_purpose_text<-"There are three main tabs for the app. The Data Exploration tab will allow users to create numerical and graphical summaries based on their desired input. The modeling page tab contains information about the models that the user will build and will allow the user to fit three different supervised learning models, as well as to enter new data values to get predictions on the models they have created. The final tab is the Data tab, and it will allow the user to scroll through the dataset, subset the data, and download the full or subsetted dataset."

lm_info_text1<-"Linear regression provides a relatively simple way to predict a quantitative response. In a this app we will be preforming a multiple linear regression (MLR) where the user will have the ability to select what variables to include in their model. Note that the MLR model being built with this app will not give the user the ability to input interaction terms. The basic equation for the MLR model the you will build is shown below: $$Y_i = B_0 + B_1x1_i + B_2x2_i + B_3x3_i ... + Ei$$ "




lm_info_text2<- "There are many benefits to linear models. One of the largest benefits is that they are easy for most people to understand and they make the process of actually making predictions fairly easy. Many people have some experience with linear models already, and that can make them an ideal model candidate. This familiarity and ease of use also means that producing a linear model is usually cheaper and easier than other methods. Additionally, if the relationship between the predictor variables and the response is well approximated by the linear relationship the linear model will likely outperform other models that are not built around this linear structure.

The biggest drawback of linear regression is that it often has difficulty fitting complex datasets and relationships that are non-linear in nature.  Linear regression can also be more impacted by outliers in the dataset than other methods. It can also be easy to overfit a linear model, thereby decreasing the model's predictive power on new data."


tree_info_text<-"Regression tree models are built to generate predictions on continuous response variables. There are two main steps in building the regression tree model. The first step is to divide the predictor space into regions, with each region having a different prediction. In the second step we use the mean of observations within the prediction space as the prediction value.

Regression tree models have several benefits. First, trees are fairly simple to understand and they provide easy to interpret output. Predictors do not need to be scaled for regression trees, although they can be scaled if desired. Trees can be constructed without the need to include statistical assumptions. Tree models also offer built in variable selection, without the need for the user to input specific interaction terms as was necessary in building a linear model. Regression trees are able to account for interaction between variables by the nature of how the model is constructed. In building a regression tree each split is determined by minimizing the RSS. For each step of the way, the regression tree model identifies the optimal split for the data at that point. At each step an inequality for a single variable is considered where the split occurs. Later on in the branching another variable will be considered. So, if we were looking at the impact of college education and income on percent registered to vote in an area the best split for the first node of the tree may be college educated less than 30% vs greater than 30%. The next split may use income less than $50,000 vs greater than $50,000 and so on, each time seeking to minimize the RSS value. In this way the regression tree model is very flexible and able to handle interaction among predictor variables without having to explicitly tell it to do so. 

There are several drawbacks to regression tree models as well. As a whole, regression trees tend to have high variance, where small changes in data can vastly change the resulting tree. They can be more impacted by outliers and even small changes in the data, than linear methods. This can lead to lower predictive ability as compared to linear methods, particularly when dealing with data that fits a linear model well. Additionally, there is no optimal algorithm for building regression trees. Trees use a greedy algorithm for splitting, looking at the best split at each step, but not for the model as a whole. There is also usually a need to prune the tree back  to prevent overfitting and to lower variance in order to make better predictions before the final model is determined."

rf_info_text<-"A random forest model is an ensemble model used to improve upon the weaknesses of a single tree and bagged tree model. A single tree model can have a high variance and can change dramatically based on small changes in the data. Ensemble methods can help to deal with this issue by creating many single tress and creating an average prediction based on all of the predictions of the trees that were created. The goal of this is to reduce the variance of the model, thereby increasing our model's predictive ability.

Random forest models create multiple trees from bootstrap samples and average the results, which provides a reduction in variability of predictions when compared to a single tree model. Random forests use a random subset of the predictors for each bootstrap sample/tree fit. At each step along the way the random forest model is only using a random subset (*m*) of predictors to choose from when splitting the data, as opposed to the entire set of *p* predictors in the data set. One of the core strengths of the random forest model is its ability to decorrelate the trees that are produced, which helps to decrease the resulting variance in our final model.  

The random forest model helps to avoid a problem that can arise if there is a single very strong predictor that exists in the data set. If there is a single strong predictor in the data set, every bootstrap tree is likely to use that predictor for its first split, resulting in bagged tree predictions that are more correlated to one another, and thereby giving a smaller reduction in the variance from aggregation. The same issue arises when there are a large number of highly correlated predictors in the data set as well, and this problem can be addressed by creating a random forest model with a small *m* value. As the goal of ensemble methods is to reduce variance in order to increase prediction success, when there is one strong predictor, or several correlated predictors, such as what we see in this dataset, we will likely want to select a random forest model over a bagged tree model.  

The random forest model essentially decorrelates our single trees. In the random forest model at each split the model is only allowed to consider a random subset of our predictor variables to use for that split. By creating a random subset of variables for the model to choose from, we prevent all models from converging on a few very similar trees, particularly when you have one strong predictor in your dataset. We can then use the trees created to generate predictions based on the average response of all trees in our model, leading to decreased variance as compared to both a single tree and bagged tree fit. 

A big disadvantage  of random forest, is that these methods can be costly to build, train and implement and will likely be more difficult to interpret than a linear model. 

In training the random forest model we are using cross-validation.  We are also using values of `1:3` for `mtry` to find the optimal model. Lowest RMSE value was used to identify the optimal model. We will train a final model to obtain the test MSE based on the test set."

model_options_text<-"Select your desired options below. When you have selected the options that you want for your models click on the button that says RUN ALL MODELS"

data_split_text<-"What percent of the dataset do you want to use to create your training data set? Select a value between 1% and 99% to decide what portion of the data you would like to use to train your model. The remaining data in the original data set will be used for the test data set to see how well your model performs on predicting new data. NOTE: The data set from the CDC is fairly large, and as a result the deafault value for the data split is to send 15% of the observations to the training set. You are free to choose any value between 1 and 99 that you would like for your data split, but larger training sets will result in increased computational time in the model training stage."

lm_vars_text<-"Select all variables that you want to include in your MLR model. Note that we are building a MLR model with no interaction terms. If no variables are selected a MLR model will be built using all variables."

tree_vars_text<-"Select all variables that you want to include in your regression tree model. If no variables are selected a regression tree model will be built using all variables."

rf_vars_text<-"Select all variables that you want to include in your random forest model. If no variables are selected a random forest model will be built using all variables."








