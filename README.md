[![Build Status](https://travis-ci.org/obcode/plexams.svg?branch=master)](https://travis-ci.org/obcode/plexams)

# plexams

A Haskell tool for planning exams with a gui build on top of
[Electron](https://electron.atom.io/).

# Requirements

-   [Haskell](https://www.haskell.org/downloads)

-   [Electron](https://electron.atom.io/).

        $ npm install electron

# Quickstart

## Install

In the top directory do

    $ stack install

Then, change to the `elexams` directory and do

    $ npm install

## Run

Start the server in the directory containing all config files

    $ plexams-server

Start the elexams-app in the `elexams`-directory

    $ npm start

# Plexams --- Usage

```
plexams

Usage: plexams COMMAND [-p|--planManip PLANMANIPFILE] [--rooms ROOMSFILE]
               [-r|--registrations REGISTRATIONSFILE]
               [-l|--overlaps OVERLAPSFILE] [-c|--constraints CONSTRAINTSFILE]
               [-s|--students STUDENTSFILE] [--handicaps HANDICAPSFILE]
               [--invigilators INVIGILATORSFILE]
               [--add-invigilations ADDINVIGILATIONSFILE] [-o|--output OUTFILE]
               [-c|--config CONFIGFILE] [--no-validation] [-v|--verbose]
  Tool for planning exams

Available options:
  -p,--planManip PLANMANIPFILE
                           import file containing exam2slot manipulations
  --rooms ROOMSFILE        import file containing room2slot manipulations
  -r,--registrations REGISTRATIONSFILE
                           import file containing registrations
  -l,--overlaps OVERLAPSFILE
                           import file containing overlaps
  -c,--constraints CONSTRAINTSFILE
                           import file containing constraints
  -s,--students STUDENTSFILE
                           import file containing registrations for each mtknr
  --handicaps HANDICAPSFILE
                           import file containing handicap information
  --invigilators INVIGILATORSFILE
                           import file containing invigilator information
  --add-invigilations ADDINVIGILATIONSFILE
                           import file containing invigilator mappings
  -o,--output OUTFILE      output to file instead of stdout
  -c,--config CONFIGFILE   file containing
                           semesterconfig (default: "plexams.yaml")
  --no-validation          turn of validation
  -v,--verbose             turn on verbosity
  -h,--help                Show this help text

Available commands:
  markdown                 the plan as markdown document
  html                     the plan as an HTML table
  stats                    statistics
  validate                 validation of current plan
  query                    query plan
  export                   export current plan for ZPA
  config                   print the current config
  generate                 generate part of the plan
  generate-rooms           generate rooms for the schedule
  generate-invigilations   generate invigilations for the schedule
```

# Plexams-Helper --- Usage

```
plexams-helper

Usage: plexams-helper COMMAND (-g|--group GROUP) (-i|--infile INFILE)
                      [-o|--outfile OUTFILE]
  Tool for preparing input files for plexams

Available options:
  -g,--group GROUP         student group (one of IB, IC, IF, IG, IN, IS, GO)
  -i,--infile INFILE       input from file
  -o,--outfile OUTFILE     write to file (instead of stdout)
  -h,--help                Show this help text

Available commands:
  registrations            prepare a registration file
  overlaps                 prepare a overlaps file
  students                 prepare a students file
```

# Cabal Packages

`plexams-core`
:   the core library

`plexams-cli`
:   a command line interface

`plexams-generators`
:   automagically generate part of the plan

`plexams-server`
:   http-server for elexams
