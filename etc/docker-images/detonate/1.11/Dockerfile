FROM pveber/bistro-base:stretch

RUN apt-get update && apt-get install -y bowtie
RUN wget "http://deweylab.biostat.wisc.edu/detonate/detonate-1.11-precompiled.tar.gz"
RUN tar xvfz detonate-1.11-precompiled.tar.gz
RUN cd detonate-1.11-precompiled/rsem-eval && cp rsem-build-read-index rsem-eval-calculate-score rsem-eval-estimate-transcript-length-distribution rsem-eval-run-em rsem-extract-reference-transcripts rsem-parse-alignments rsem-plot-model rsem-preref rsem-sam-validator rsem-scan-for-paired-end-reads rsem-simulate-reads rsem-synthesis-reference-transcripts /usr/bin
RUN cd detonate-1.11-precompiled/ref-eval && cp ref-eval ref-eval-estimate-true-assembly /usr/bin
