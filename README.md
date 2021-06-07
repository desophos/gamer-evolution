# Overview

gamer-evolution is a framework for evolving game-theoretic agents. The agents are represented by finite state machines and evolved with a genetic algorithm in which the fitness function is the game being played. It is designed to support any game, although it's currently limited to 2-player games. `app/Main.hs` includes an example usage: graphing a population's average fitness over generations of evolution.

# Background

This is a rewrite from scratch of my old project [winning-the-ipd](https://github.com/desophos/winning-the-ipd). I decided to rebuild it because it was broken, the code was poorly architected, and it was a good excuse to learn Haskell.
