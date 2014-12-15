Mancala
=======
To run: <br>
$ ghc playMancala <br>
$ ./playMancala

During gameplay, PlayerA owns the bottom row of pits and the right 
store (pit indices 0-5; store index 6), while PlayerB owns the top row and left
store (pit indices 7-12; store index 13). To move, enter a pit index at the prompt ': '.  

The game ends when either player's row of pits is completely emptied, and the player
with the most stones in his/her store wins!

COMPATABILITY: ghc does NOT compile on version 7.4.1. Use 7.6.3 instead.  
