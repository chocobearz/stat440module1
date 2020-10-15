# Import Algorithm
from sklearn.neighbors import KNeighborsRegressor

# Read in the dataset
dataset = pd.read_csv('data\level1.csv', encoding='latin')

# Separate the features (X) and target (y)
# Reshape so each item is a list of features/targets
X = dataset['YearsExperience'].values.reshape(-1, 1)
y = dataset['Salary'].values.reshape(-1, 1)

# Split into training, test sets (80%, 20%)
X_train_val, X_test, y_train_val, y_test=train_test_split(X, y, test_size=0.2, random_state=0)

# Split train_val sets into separate training and validation sets
# Final split is 60% train, 20% validation, and 20% testing
X_train, X_val, y_train, y_val = train_test_split(X_train_val, y_train_val, test_size=0.25, random_state=0)

#@title Solution
# Initialize the knn regression model
knn_regressor = KNeighborsRegressor()

# Train the model on the training data
knn_regressor.fit(X_train, y_train)

# Find Score (max is 1)
print("Training Score: {}".format(knn_regressor.score(X_train, y_train)))
print("Validation Score: {}".format(knn_regressor.score(X_val, y_val)))

# Create table to show differences in predicted salary
results = pd.DataFrame()
results['Experience'] = X_val.reshape(-1)
results['True Salary'] = y_val.reshape(-1)
results['Predicted Salary'] = knn_regressor.predict(X_val).reshape(-1)
results

# Initialize the knn regression model
knn_regressor = KNeighborsRegressor(n_neighbors=2) # THIS LINE

# Train the model on the training data
knn_regressor.fit(X_train, y_train)

# Find Score (max is 1)
print("Training Score: {}".format(knn_regressor.score(X_train, y_train)))
print("Validation Score: {}".format(knn_regressor.score(X_val, y_val)))

# Use the trained model to predict values on the test set
y_predict = knn_regressor.predict(X_test)

# Find Score (Max is 1)
print("Testing Score: {}".format(knn_regressor.score(X_test, y_test)))