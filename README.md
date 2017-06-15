[![Build Status](https://travis-ci.org/obcode/plexams.svg?branch=master)](https://travis-ci.org/obcode/plexams)
[![Build Status](https://terraform.cs.hm.edu/jenkins/buildStatus/icon?job=plexams)](https://terraform.cs.hm.edu/jenkins/job/plexams)
[![Build status](https://ci.appveyor.com/api/projects/status/eraclkgf9kctilm7?svg=true)](https://ci.appveyor.com/project/obcode/plexams)

# plexams

A Haskell tool for planning exams with a gui build on top of
[Electron](https://electron.atom.io/).

# Requirements

-   [Haskell](https://www.haskell.org/downloads)

-   [Electron](https://electron.atom.io/).

        $ npm install electron

# Quickstart

In the top directory do

    $ stack install

Then, change to the `elexams` directory and do

    $ npm install

Start the app with

    $ npm start

# Cabal Packages

`plexams-core`
:   the core library

`plexams-cli`
:   a command line interface

`plexams-gui`
:   a graphical user interface

`plexams-generators`
:   automagically generate part of the plan

`plexams-server`
:   http-server for elexams
