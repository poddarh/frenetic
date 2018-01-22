#!/bin/bash

for f in $(ls topo/ --sort size --reverse)
do
	echo "Topology: $(basename ${f%.dot})";
	./Test.d.byte "topo/$f";
	echo "";
done
