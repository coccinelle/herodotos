#!/bin/bash
cd results/linuxes/stable
for new in `ls *.new.org`
do
  /home/manseur/herodotos/herodotos/herodotos --prefix /var/linuxes/stable/ --parse_org $new --to-sql > ../../../data-sql/${new%new.org}sql
done;
