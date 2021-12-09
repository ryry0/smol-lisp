SOURCES=$(wildcard *.fs)

all: build

build:
	dotnet build
run:
	dotnet run

format:
	fantomas $(SOURCES)

fmt: format lint

lint:
	for i in $(SOURCES) ; do \
		dotnet fsharplint lint $$i ; \
	done
