# Makefile for psachin.gitlab.io

# Usage:
# `make` or `make publish`: Publish files using available Emacs configuration.
# `make publish_no_init`: Publish files without using Emacs configuration.
# `make clean`: Delete existing public/ directory and cached file under ~/.org-timestamps/

# Local testing:
# `python -m http.server --directory=public/`          <-- (The '--directory' flag is available from Python 3.7)

.PHONY: all publish publish_no_init

all: publish

publish: publish.el
	@echo "Publishing... with current Emacs configurations."
	~/github/emacs/src/emacs --batch --load publish.el --funcall org-publish-all

publish_no_init: publish.el
	@echo "Publishing... with --no-init."
	~/github/emacs/src/emacs --batch --no-init --load publish.el --funcall org-publish-all

clean:
	@echo "Cleaning up.."
	rm -rv public/*
	rm -rv ~/.org-timestamps/*
