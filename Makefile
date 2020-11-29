YEAR=Y2020
year:
	mkdir -p "./src/Advent/${YEAR}/Day${day}"
	mkdir -p "./test/Advent/${YEAR}/Day${day}"

	@./bin/gen/year ${YEAR}

	@echo "Generated year, add new executable in package.yaml"
day:
	mkdir -p "./src/Advent/${YEAR}/Day${day}"
	mkdir -p "./test/Advent/${YEAR}/Day${day}"

	@./bin/gen/day ${YEAR} ${day} 1
	@./bin/gen/day ${YEAR} ${day} 2

	@echo "Modules generated"
