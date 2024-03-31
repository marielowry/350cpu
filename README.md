# Processor
## Marie Lowry (mrl62)

## Description of Design
This design implements a 5-stage pipelined processor which operates at a speed of 50MHz to implement a word-addressed MIPS ISA. 

## Bypassing
Check for read after write dependencies and resolve the hazards by inserting the output from either the X/M latch or M/W latch into 

## Stalling
multiplier and divider stalling
Multiplier stalls 17 cycles
Divider stalls 33 cycles

## Optimizations
Multiplier implements modified booths
Divider uses non-restoring division

## Bugs
The stalls for multdiv are triggered twice, which means that all instructions execute correcty but not within the number of cycles in the gradescope autograder. I implemented revised logic for multdiv in subsequent submissions but was not able to debug before the deadline. 