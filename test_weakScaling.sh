#!/bin/bash  

#Clear any json blocks
rm -rf blocks.json
echo Cleaned blocks.json

#Iterate over different partition counts
for i in 1 2 4 8 12 16 20 24
do
    ./testGen.py testBlocks.txt $[4000*i] $i   
    spark-submit --master local[$i] --driver-memory 20G target/scala-2.10/CS267-final-assembly-1.0.jar testBlocks.txt $i 1000000
    rm -rf blocks.json
    echo Tested $i cores
done
