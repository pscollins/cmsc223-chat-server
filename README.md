# General notes

For the most part, this module follows cabal conventions.

# Building

	cabal sandbox init
	cabal install --enable tests

# Running

    CHAT_SERVER_PORT="5050" cabal run
	telnet localhost 5050

# Testing

	cabal test

# Example session
