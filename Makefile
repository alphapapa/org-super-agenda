.PHONY: test update-test-results

test:
	cask exec test/run

update-test-results:
	cask exec test/run --update
