FROM pveber/bistro-base:jessie

RUN \
  apt-get update && apt-get install -y --fix-missing gcc zlib1g-dev

RUN \
  wget -O seq-gen.tar.gz "http://tree.bio.ed.ac.uk/download.php?id=85" && \
  tar xf seq-gen.tar.gz && rm seq-gen.tar.gz #&&
RUN  cd Seq-Gen.v1.3.3/source && make && cp seq-gen /usr/bin
