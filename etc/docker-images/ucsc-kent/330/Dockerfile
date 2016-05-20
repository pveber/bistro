FROM pveber/bistro-base:latest

RUN apt-get install -y libmysqlclient-dev libssl-dev libpng-dev

RUN wget https://raw.githubusercontent.com/pveber/compbio-scripts/master/kent-tree-install/330/kent-tree-install.sh -O - | bash -s /usr
