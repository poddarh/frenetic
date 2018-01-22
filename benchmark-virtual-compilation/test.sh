#!/bin/bash
set -e
mkdir -p kat-files
cd kat-files

while read p; do
	echo ""
	echo "----- Topology: $p -----------------------"

	{
	echo ""
	echo "-> bigswitch: "
	../../Main.native dot-to-virtual ../topo/$p.dot 1
	../../frenetic.native dump virtual vpol.kat
	rm -r *

	echo ""
	echo "-> barbell: "
	../../Main.native dot-to-virtual ../topo/$p.dot 2
	../../frenetic.native dump virtual vpol.kat
	rm -r *
	} || {
		echo ""
		echo "--- Error ---"
		echo ""
	}

done <../topo.txt
