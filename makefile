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
	dotnet fsharplint lint $(SOURCES)
