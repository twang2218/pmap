.PHONY: docs check build test build_win coverage docker-image docker-launch docker-check docker-docs docker-test docker-build docker-release docker-coverage

docs:
	Rscript -e "devtools::document(); devtools::check_man()"

test:
	Rscript -e "devtools::test()"

build:
	Rscript -e "devtools::build()"

release: check
	Rscript -e "options(repos = c(CRAN = 'https://cran.rstudio.com/')); devtools::release(check = FALSE, spelling = NULL)"

check:
	Rscript -e "devtools::check()"

build_win:
	Rscript -e "devtools::build_win()"

coverage:
	Rscript -e "covr::coveralls()"

## Docker related tasks

docker-image:
	docker build --build-arg R_BASE_VERSION=4.1.1 -t pmap .

docker-launch: docker-image
	docker ps --format '{{.Names}}' | grep -q pmap || \
		docker run -itd --rm --name pmap pmap sleep infinity

docker-stop:
	docker ps --format '{{.Names}}' | grep -q pmap && \
		docker stop pmap

docker-shell: docker-launch
	docker exec -it pmap bash

docker-check: docker-launch
	docker exec -it pmap make check

docker-docs: docker-launch
	docker exec -it pmap make docs
	docker cp pmap:/opt/src/DESCRIPTION ./
	docker cp pmap:/opt/src/NAMESPACE ./
	docker cp pmap:/opt/src/man ./

docker-test: docker-launch
	docker exec -it pmap make test

docker-build: docker-launch
	docker exec -it pmap make build

docker-release: docker-launch
	docker exec -it pmap make release

docker-coverage: docker-launch
	docker exec -it pmap make coverage
