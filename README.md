# RPNCalc

A simple command line reverse polish notation calculator.

Supports both an interactive "repl" like mode, and a batch
mode taken argument.

## Usage
RPN starts interactive calc mode: press enter after every value/operator

RPN CALCULATION -- runs the CALCULATION and prints the result non interactively

## Example
### Batch Mode
    RPN "1 8 3 + 2 ^ -"
    -120.0

Or Alternatively, in

### Interactive Mode
    $ RPN
    RPN>> 1
    1:    1.0
    RPN>> 8
    2:    1.0
    1:    8.0
    RPN>> 3
    3:    1.0
    2:    8.0
    1:    3.0
    RPN>> +
    2:    1.0
    1:    11.0
    RPN>> 2
    3:    1.0
    2:    11.0
    1:    2.0
    RPN>> ^
    2:    1.0
    1:    121.0
    RPN>> -
    1:    -120.0
    RPN>> quit
    $ 

## TODO
 * add support for entering more than one value per line in interactive mode
 * variables

## DONE
 * deal with functions that take more or less than 2 arguments
 * more math functions: sin, sqrt, etc. -- work in progress
 * added clear functions which wipes the stack
