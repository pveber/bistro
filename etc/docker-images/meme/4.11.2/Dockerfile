FROM pveber/bistro-base:latest

RUN \
  apt-get update && apt-get install -y zlib1g-dev libexpat1-dev python-dev \
  automake autoconf libtool ghostscript libc6 libopenmpi-dev openmpi-common openmpi-bin

RUN \
  wget http://meme-suite.org/meme-software/4.11.2/meme_4.11.2_1.tar.gz && \
  tar xf meme_4.11.2_1.tar.gz && \
  rm meme_4.11.2_1.tar.gz && cd meme_4.11.2 && \
  cd scripts &&  perl dependencies.pl && cpan Log::Log4perl XML::Simple

RUN \
  cd meme_4.11.2 &&  ./configure --prefix=/usr --with-url=http://meme-suite.org --enable-build-libxml2 --enable-build-libxslt && \
  make && make install && make clean
