.PHONY: help build

APP_VSN ?= `grep 'version' package.json | cut -d '"' -f4`
BUILD ?= `git rev-parse --short HEAD`
APP_NAME ?= frontend
IMAGE_NAME ?= bespiral/frontend

help:
	@echo "$(APP_NAME):$(APP_VSN)-$(BUILD)"
	@perl -nle'print $& if m{^[a-zA-Z_-]+:.*?## .*$$}' $(MAKEFILE_LIST) | sort | awk 'BEGIN {FS = ":.*?## "}; {printf "\033[36m%-30s\033[0m %s\n", $$1, $$2}'

build: ## Build the Docker image
	docker build \
		-t $(IMAGE_NAME):$(APP_VSN)-$(BUILD) \
		-t $(IMAGE_NAME):latest .

push: ## Push the image to docker repository
	docker push $(IMAGE_NAME):$(APP_VSN)-$(BUILD)
	docker push $(IMAGE_NAME):latest

run: ## Run the app in Docker
	docker run \
		--expose 80 -p 80:80 \
		--name bespiral-frontend \
		--rm -it $(IMAGE_NAME):latest
