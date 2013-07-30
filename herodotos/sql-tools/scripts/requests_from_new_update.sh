#!/bin/bash
cd results/linuxes/stable
for new in `ls *.new.org`
do
  /home/manseur/herodotos/herodotos/herodotos --prefix /var/linuxes/stable/ --parse_org $new --to-sql-update $1 > ../../../data-sql/increment/${new%new.org}sql
done;
