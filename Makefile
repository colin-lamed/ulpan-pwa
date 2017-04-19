#

all: build

build:
	set -e

	# Build the client
	stack build --stack-yaml=client/stack.yaml

	# Copy over the javascript
	rm -f server/static/all.js
	cp $(shell stack path --stack-yaml=client/stack.yaml --local-install-root)/bin/ulpan-client.jsexe/all.js server/static/js/all.js

	# Build the server
	stack build --stack-yaml=server/stack.yaml

clean:
	stack clean --stack-yaml=interface/stack.yaml
	stack clean --stack-yaml=client/stack.yaml
	stack clean --stack-yaml=server/stack.yaml

run: build
	 cd server; stack exec ulpan Development
