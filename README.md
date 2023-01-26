# Clustering-Comparison-between-methods

The data set has details of 1008 used cars along with a set of variables: Brand, Car model, Resale price, Mileage, Seat capacity, Vehicle type, fuel type, transmission, parking sensor, airbag, cruise control, keyless entry, alloy wheel, ABS, Climate control, Rear AC vent and Power Steering. 

The following analysis is considered:
- Deciding the distance measure for this dataset is crucial, since there is a mixture of categorical and numerical variables. We use the gower metric for this case, more details are provided in the documentation 
- Hierarchical clustering is applied on the entire dataset and cluster profiling is carried out 
- K-means and hierarchical clustering results are compared where k-means is applied on only the numerical variables 
- Comparison is made based on the following metrics : W/B Ratio, Within Sum of Squares, Calinski Harabasz Index, Dunn Index
