setup:
	mkdir -p dist

build: setup
	stack run

clean:
	stack clean && rm -r dist

watch: setup
	ghcid '--command=stack ghci'\
		--warnings\
		--test Main.main\
		--reload resources\
		--reload public\
		--restart package.yaml\
		--restart stack.yaml\
		-h 0 -w 0
