FROM pveber/bistro-base:jessie

RUN \
  wget https://ftp-private.ncbi.nlm.nih.gov/sra/sdk/2.8.0/sratoolkit.2.8.0-ubuntu64.tar.gz && \
  tar xvfz sratoolkit.2.8.0-ubuntu64.tar.gz && \
  cd sratoolkit.2.8.0-ubuntu64 && \
  cp -r bin/* /usr/bin && \
  cd .. && rm -rf sratoolkit*
