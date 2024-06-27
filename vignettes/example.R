library(compactness)

## Load test data
shp = "data/20110727_q2_congressional_final_draft.shp" # California 2011 Congressional district maps
namecol = "DISTRICT"
shp = read_shapefiles(shp, namecol)

## Alternate test data
## shp = "CnclDist_July2012.shp"  # LA City Council districts
## shp = read_shapefiles(shp, "DISTRICT")

# Generate features
features = generate_features(shp)

# Generate predictions and SEs
predictions = generate_predictions(features, namecol)
