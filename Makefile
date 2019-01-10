book: index.Rmd
	rm -f borderlands.Rmd
	Rscript -e 'bookdown::render_book("index.Rmd")'
	rm -r docs/widgets
	mv widgets docs/.
