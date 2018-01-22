#!/bin/bash

for f in topo/*
do
	./PortlessPolicyGenerator.d.byte "$f" > "./policies/$(basename ${f%.dot}).pol" 2>policy_generation_error.log
	echo "Generated policy for the topology: $(basename ${f%.dot})"
done
