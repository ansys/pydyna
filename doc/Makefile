# Minimal makefile for Sphinx documentation
#

# You can set these variables from the command line, and also
# from the environment for the first two.
SPHINXJOBS    ?= auto
SPHINXOPTS    = -j $(SPHINXJOBS)
SPHINXBUILD   = sphinx-build
SOURCEDIR     = source
BUILDDIR      = _build
APIDIR        = api
KEYWORDDIR    = $(SOURCEDIR)/keyword_classes
KEYWORDBUILD   = $(KEYWORDDIR)/_build/html

# Put it first so that "make" without argument is like "make help".
help:
	@$(SPHINXBUILD) -M help "$(SOURCEDIR)" "$(BUILDDIR)" $(SPHINXOPTS) $(O)

.PHONY: help Makefile clean pdf keyword_classes

keyword_classes:
	@echo "🔧 Building keywordclasses..."
	$(SPHINXBUILD) -b html "$(KEYWORDDIR)" "$(KEYWORDBUILD)" $(SPHINXOPTS) $(O)

# Catch-all target: route all unknown targets to Sphinx using the new
# "make mode" option.  $(O) is meant as a shortcut for $(SPHINXOPTS).
%: Makefile
	@echo "⏱️  Starting Sphinx build with -j $(SPHINXJOBS)..."
	@echo "Build started at $$(date '+%Y-%m-%d %H:%M:%S')"
	@time $(SPHINXBUILD) -M $@ "$(SOURCEDIR)" "$(BUILDDIR)" $(SPHINXOPTS) $(O)
	@echo "Build finished at $$(date '+%Y-%m-%d %H:%M:%S')"

clean:
	rm -rf $(BUILDDIR)/*
	rm -rf source/examples/gallery_examples
	find . -type d -name $(APIDIR) -exec rm -rf {} +

pdf:
	@$(SPHINXBUILD) -M latex "$(SOURCEDIR)" "$(BUILDDIR)" $(SPHINXOPTS) $(O)
	cd $(BUILDDIR)/latex && latexmk -r latexmkrc -pdf *.tex -interaction=nonstopmode || true
	(test -f $(BUILDDIR)/latex/*.pdf && echo pdf exists) || exit 1
