#!/bin/bash

# set the source and target folders
src_folder="src/include/*(.)"
tgt_folder="src/include/elektra"

# create the target folder if it doesn't exist
#mkdir -p $tgt_folder

# go through all header files in the source folder
for file in *(.)
do
  # print the current file name
  echo "Processing file: $file"

  # check if the file is mentioned in the CMakeLists.txt file
  if grep -q $file CMakeLists.txt
  then
    # if it is mentioned, move it to the target folder
    mv $file $tgt_folder
    # print a success message
    echo "File moved to $tgt_folder"
  else
    # print a message if the file is not mentioned in CMakeLists.txt
    echo "File not found in CMakeLists.txt"
  fi
done
