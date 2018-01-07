.PHONY: docs check build test build_win coverage

docs:
	Rscript -e "devtools::document(); devtools::check_man()"

test:
	Rscript -e "devtools::test()"

build:
	Rscript -e "devtools::build()"

check:
	Rscript -e "devtools::check()"

build_win:
	Rscript -e "devtools::build_win()"

coverage:
	Rscript -e "covr::coveralls()"
