# RPNCalc

A simple command line reverse polish notation calculator.

Supports both an interactive "repl" like mode, and a batch
mode taken argument.

## Usage
RPN -- starts interactive calc mode: press enter after every value/operator

RPN CALCULATION -- runs the CALCULATION and prints the result non interactively

## Example
### Batch Mode
    RPN "1 8 3 + 2 ^ -"
    -120.0

### Interactive Mode
    $ RPN
    RPN>> 1
    1:    1.0
    RPN>> 8
    1:    8.0
    2:    1.0
    RPN>> 3
    1:    3.0
    2:    8.0
    3:    1.0
    RPN>> +
    1:    11.0
    2:    1.0
    RPN>> 2
    1:    2.0
    2:    11.0
    3:    1.0
    RPN>> ^
    1:    121.0
    2:    1.0
    RPN>> -
    1:    -120.0
    RPN>> quit
    $ 

## TODO
 * add support for entering more than one value per line in interactive mode
 * variables
 * more math functions: sin, sqrt, etc.
