# Script

```shell script
# So by circumstances you managed to have elektra on your system?
# Don't worry, we will show you how to handle this correctly

cd repositories/libelektra
mkdir build
cd build
cmake ..
sudo make uninstall
cd ../..
rm -rf libelektra

# installed it via package manager?  don't worry!
sudo apt-get purge libelektra

# sometimes even the best people install shitty software ¯\_(ツ)_/¯
```