

daemon: daemon.hs redis.hs spawn.hs
	ghc -O2 --make daemon.hs -XScopedTypeVariables




