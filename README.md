# General notes

For the most part, this module follows cabal conventions.

# Building

	cabal sandbox init
	cabal install --enable tests

# Running
For some port number N > 1000:

    CHAT_SERVER_PORT="N" cabal run
	nc localhost N

# Testing

	cabal test

# Example session

## Running in a bad environment
	➜  cmsc223-chat-server git:(master) ✗ cabal run
	Preprocessing library chat-1.0.0...
	In-place registering chat-1.0.0...
	Preprocessing executable 'chat' for chat-1.0.0...
	Running chat...
	Environment variable CHAT_SERVER_PORT not set.


## Running in a good environment
	➜  cmsc223-chat-server git:(master) ✗ CHAT_SERVER_PORT="5050" cabal run
	Preprocessing library chat-1.0.0...
	In-place registering chat-1.0.0...
	Preprocessing executable 'chat' for chat-1.0.0...
	Running chat...


	➜  cmsc223-chat-server git:(master) ✗ nc localhost 5050
	1 has joined
	2 has joined
	2: Hello world
	Hello, 2!
	2: Hey, 1.
	Goodbe, 2!
	^D

	➜  cmsc223-chat-server git:(master) ✗ nc localhost 5050
	2 has joined
	Hello world
	1: Hello, 2!
	Hey, 1.
	1: Goodbe, 2!
	1 has left

Note that we expect clients to send ^D when they are done, like
civilized Unix users. See my Piazza post:
[https://piazza.com/class/i7xhvnzdujoa2?cid=43](https://piazza.com/class/i7xhvnzdujoa2?cid=43).
