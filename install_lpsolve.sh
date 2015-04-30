#! /bin/bash

# Installs lp_solve and its Java wrapper. This script only works for 64-bit Linux platforms
# Note that it will add a line to the end of your .bashrc file and add a 'lib' folder to
# your home directory.

# In order to set the environment up correctly so that lp_solve can b used immediately,
# it is advised to run 'source ~/.bashrc' after this script finishes.
mkdir -p ~/lib/lpsolve
cd ~/lib/lpsolve
mkdir downloads
cd downloads

# Fetch the lp_solve C library
wget http://downloads.sourceforge.net/project/lpsolve/lpsolve/5.5.2.0/lp_solve_5.5.2.0_dev_ux64.tar.gz
tar -xvzf lp_solve_5.5.2.0_dev_ux64.tar.gz
mv liblpsolve55.so ..

# Fetch the lp_solve Java wrapper
wget http://downloads.sourceforge.net/project/lpsolve/lpsolve/5.5.2.0/lp_solve_5.5.2.0_java.zip
unzip lp_solve_5.5.2.0_java.zip
mv lp_solve_5.5_java/lib/ux64/liblpsolve55j.so ..

# Clean Up
cd ..
rm -rf downloads

# We need to set the LD_LIBRARY_PATH environment variable so Java can find the new libraraies
echo "export LD_LIBRARY_PATH=\"$HOME/lib/lpsolve:\"\$LD_LIBRARY_PATH" >> ~/.bashrc
