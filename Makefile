.PHONY: benchmarks


all: docs test


# Benchmarks
benchmarks:
	(cd benchmarks; elm make Benchmarks.elm --output benchmarks.html)


# Generate documentation
docs:
	elm make --docs=docs.json


# Tests are in the Elm code as documentation tests.
# Uses https://github.com/icidasset/elm-proofread
test:
	@elm-proofread src/Binary.elm
