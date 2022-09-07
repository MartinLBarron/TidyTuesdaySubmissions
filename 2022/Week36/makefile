
all: data/inventories.rds data/inventory_sets.rds data/sets.rds

data/inventories.rds data/inventory_sets.rds data/sets.rds : src/get_data.r
	R CMD BATCH src/get_data.r

clean:
	rm data/*.rds; rm output/*.*

