#!/bin/bash

make --directory ../

cd bigswitch/
../../Main.native dot-to-virtual ../physical.dot 1
../../frenetic.native dump virtual --print-global-pol --peg pinout.kat --ping pinout.kat --ptopo ptopo.kat --veg vinout.kat --ving vinout.kat --ving-pol vingpol.kat --vrel vrel.kat --vtopo vtopo.kat vpol.kat | tee output
cd ../

cd barbell/
../../Main.native dot-to-virtual ../physical.dot 2
../../frenetic.native dump virtual --print-global-pol --peg pinout.kat --ping pinout.kat --ptopo ptopo.kat --veg vinout.kat --ving vinout.kat --ving-pol vingpol.kat --vrel vrel.kat --vtopo vtopo.kat vpol.kat | tee output
cd ../

# cp bigswitch/peg.kat peg.kat
# cp bigswitch/ping.kat ping.kat

# cp bigswitch/global_pol.kat bigswitch_pol.kat
# cp barbell/global_pol.kat barbell_pol.kat

../TestPolicy.native bigswitch/global_pol.kat barbell/global_pol.kat bigswitch/pinout.kat bigswitch/pinout.kat physical.dot