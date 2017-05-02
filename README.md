# Haskis

A basic fit-the-falling-blocks game also known by a rather famous trademarked name.

Programming: Ciro Durán <ciro.duran@gmail.com>
Graphics: Kenney.nl Puzzle Pack - http://kenney.nl/assets/puzzle-pack

## What is Haskis?

Haskis is a game where you fit falling blocks. You've probably played this before,
thanks to Alexey Pajitnov. The cool thing about Haskis is that it's writen in
Haskell. This is an attempt to write classic games in this functional language,
with the help of the ever-standing SDL2 framework.

The initial focus is not in writing the most elegant Haskell, but make it work,
as all games are created. When the game works correctly, a second pass will be
made where we will refactor the code, where possible.

## Instructions for compilation

* Install stack
* Run: stack build
* Run: stack exec

## Directory structure

* assets/ - Where the game images and sounds live
* src/ - Code
