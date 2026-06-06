#!/bin/bash

# Check if a .tar.gz file name is passed as an argument
if [ "$#" -ne 1 ]; then
    echo "Usage: $0 <file.tar.gz>"
    exit 1
fi

# Get the file name without the .tar.gz extension
filename=$(basename "$1" .tar.gz)

# Extract the .tar.gz file
tar -xzvf "$1"

# Enter the directory obtained from extraction
cd "$filename" || { echo "Error: unable to access directory $filename"; exit 1; }

# Copy the build_deb.sh script to the current directory
cp ./script/build_deb.sh .

# Make the script executable and run it
chmod +x build_deb.sh
./build_deb.sh

# Copy the .deb file one level above
deb_file=$(find build_deb -name "*.deb" | head -n 1)
if [ -n "$deb_file" ]; then
    cp "$deb_file" ../
    echo "Package $deb_file copied to ../"
else
    echo "Error: no .deb file found."
fi