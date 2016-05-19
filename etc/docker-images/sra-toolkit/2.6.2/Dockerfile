FROM pveber/bistro-base:latest

RUN \
  wget http://ftp-private.ncbi.nlm.nih.gov/sra/sdk/2.6.2/sratoolkit.2.6.2-ubuntu64.tar.gz && \
  tar xvfz sratoolkit.2.6.2-ubuntu64.tar.gz && \
  cd sratoolkit.2.6.2-ubuntu64 && \
  cp -r bin/* /usr/bin && \
  cd .. && rm -rf sratoolkit*
