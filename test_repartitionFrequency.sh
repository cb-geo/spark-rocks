#!/bin/bash  

#Clear any json blocks
rm -rf blocks.json
echo Cleaned blocks.json
#Generate blocks 42875 blocks:
./testGen.py testBlocks.txt 40000

#Iterate over different partition counts
for i in 1 2 5 10 15 20 30 50 75 200
do
    spark-submit --master local[16] --driver-memory 20G target/scala-2.10/CS267-final-assembly-1.0.jar testBlocks.txt 8 $i
    rm -rf blocks.json
    echo Tested $i partitions
done
