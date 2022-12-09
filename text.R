#This is a file to contain lengthy text for the UI

app_title_text<-"US Covid 19 Deaths Dashboard"

app_purpose_text<-"This will be a description of the purpose of the app"

about_data_text<-"Briefly discuss the data and its source - providing a link to more information about the data"

tab_purpose_text<-"Tell the user the purpose of each tab (page) of the app"

lm_info_text<-"Linear regression provides a relatively simple way to predict a quantitative response. In a simple linear regression we use a single predictor $X$ to predict a response $Y$. In a simple linear model we have two unknown constants, $Beta_0$ represents the intercept and $Beta_1$ represents the slope. $Beta_0$ is the expected value of $Y$ when $X=0$. $Beta_1$ is the average change in $Y$ that is associated with an increase of one-unit of $X$. In linear regression we use our training data to produce estimated values for $Beta_0$ and $Beta_1$ which can then be used to make predictions on our test data."

tree_info_text<-"You should explain these three modeling approaches,the benefits of each, and the drawbacks of each. You should include some type of math type in the explanation (you’ll need to include mathJax)."

rf_info_text<-"Random forest models create multiple trees from bootstrap samples and average the results, which provides a reduction in variability of predictions when compared to a single tree model. Random forests use a random subset of the predictors for each bootstrap sample/tree fit. At each step along the way the random forest model is only using a random subset (*m*) of predictors to choose from when splitting the data, as opposed to the entire set of *p* predictors in the data set. One of the core strengths of the random forest model is it's ability to decorrelate the trees that are produced, which helps to decrease the resulting variance in our final model.  

The random forest model helps to avoid a problem that can arise if there is a single very strong predictor that exists in the data set. If there is a single strong predictor in the data set, every bootstrap tree is likely to use that predictor for its first split, resulting in bagged tree predictions that are more correlated to one another, and thereby giving a smaller reduction in the variance from aggregation. The same issue arises when there are a large number of highly correlated predictors in the data set as well, and this problem can be addressed by creating a random forest model with a small *m* value. As the goal of ensemble methods is to reduce variance in order to increase prediction success, when there is one strong predictor, or several correlated predictors, we will likely want to select a random forest model over a bagged tree model."

model_options_text<-"Select your desired options below. When you have selected the options that you want for your models click on the button that says RUN ALL MODELS"

data_split_text<-"What percent of the dataset do you want to use to create your training data set? Select a value between 1% and 99% to decide what portion of the data you would like to use to train your model. The remaining data in the original data set will be used for the test data set to see how well your model performs on predicting new data. NOTE: The data set from the CDC is fairly large, and as a result the deafault value for the data split is to send 15% of the observations to the training set. You are free to choose any value between 1 and 99 that you would like for your data split, but larger training sets will result in increased computational time in the model training stage."

lm_vars_text<-"Select all variables that you want to include in your MLR model. Note that we are building a MLR model with no interaction terms. If no variables are selected a MLR model will be built using all variables."

tree_vars_text<-"Select all variables that you want to include in your regression tree model. If no variables are selected a regression tree model will be built using all variables."

rf_vars_text<-"Select all variables that you want to include in your random forest model. If no variables are selected a random forest model will be built using all variables."
