# Prerequisites:
sudo apt-get install git perl python3 make autoconf g++ flex bison ccache
sudo apt-get install libgoogle-perftools-dev numactl perl-doc
sudo apt-get install libfl2  # Ubuntu only (ignore if gives error)
sudo apt-get install libfl-dev  # Ubuntu only (ignore if gives error)
sudo apt-get install zlibc zlib1g zlib1g-dev  # Ubuntu only (ignore if gives error)

cd ~ # install everything in home dir by default
wget https://github.com/verilator/verilator/archive/refs/heads/master.zip # get zip file, instead of git clone
unzip master.zip
cd ~/verilator-master

#
## Every time you need to build:
unsetenv VERILATOR_ROOT  # For csh; ignore error if on bash
unset VERILATOR_ROOT  # For bash
cd verilator

#git pull         # Make sure git repository is up-to-date
#git tag          # See what versions exist
##git checkout master      # Use development branch (e.g. recent bug fixes)
##git checkout stable      # Use most recent stable release
##git checkout v{version}  # Switch to specified release version

# installation
autoconf         # Create ./configure script
./configure      # Configure and create Makefile
make -j `nproc`  # Build Verilator itself (if error, try just 'make')
sudo make install