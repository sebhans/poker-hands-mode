# Poker Hands Kata in Elisp
This Emacs mode implements the [Poker Hands Kata](https://codingdojo.org/kata/PokerHands/).


## Usage
Open a buffer with lines conforming to the following format (as described in the kata).

```
Black: 2H 3D 5S 9C KD  White: 2C 3H 4S 8C AH
```

With point on a line, press `C-c C-c` to evaluate the game. The result will be appended to the line like this:

```
Black: 2H 3D 5S 9C KD  White: 2C 3H 4S 8C AH | White wins with high card: ace
```
