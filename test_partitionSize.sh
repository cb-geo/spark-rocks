#!/bin/bash  

#Clear any json blocks
rm -rf blocks.json
echo Cleaned blocks.json
#Generate blocks 42875 blocks:
./testGen.py testBlocks.txt 40000

#Iterate over different partition counts
for i in 4 8 12 18 24 36 48 96
do
    spark-submit --master local[12] --driver-memory 20G target/scala-2.10/CS267-final-assembly-1.0.jar testBlocks.txt $i 15
    rm -rf blocks.json
    echo Tested $i partitions
done
