

telework-daemon: daemon.hs redis.hs spawn.hs
	ghc -o telework-daemon -O2 --make daemon.hs -XScopedTypeVariables


