# Makefile for psachin.gitlab.io

.PHONY: all publish publish_no_init

EMACS =

ifndef EMACS
EMACS = "emacs"
endif

all: publish

publish: publish.el
	@echo "Publishing... with current Emacs configurations."
	${EMACS} --batch --load publish.el --funcall org-publish-all

publish_no_init: publish.el
	@echo "Publishing... with --no-init."
	${EMACS} --batch --no-init --load publish.el --funcall org-publish-all

clean:
	@echo "Cleaning up.."
	@rm -rvf *.elc
	@rm -rvf public
	@rm -rvf ~/.org-timestamps/*
