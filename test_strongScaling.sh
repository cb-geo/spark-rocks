#!/bin/bash  

#Clear any json blocks
rm -rf blocks.json
echo Cleaned blocks.json

#Generate 64000 blocks
./testGen.py testBlocks.txt 64000    

#Iterate over different partition counts
for j in 8 16 24
do
    #Iterate over different number of cores
    for i in 1 2 4 8 12 16 20 24
    do
	spark-submit --master local[$i] --driver-memory 20G target/scala-2.10/CS267-final-assembly-1.0.jar testBlocks.txt $j 15
	rm -rf blocks.json
	echo Tested $j partitions with $i cores
    done
done
