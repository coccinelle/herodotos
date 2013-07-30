#!/bin/bash
cd data-sql/increment
for f in `ls` 
do
   echo "\i $f" | psql stable-2.6.32
done;   
