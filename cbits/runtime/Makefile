.PHONY: all clean sampler


all: sample.pdf
	evince $< || open $<

sampler:
	make -C src
	cp src/sampler $@

sample.pdf: histogram.R datafile
	R --no-save -f $<

datafile: sampler
	./sampler > $@

clean:
	-rm datafile datafile.png



