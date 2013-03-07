

telework-daemon: src/daemon.hs src/worker.hs src/auto.hs src/redis.hs src/spawn.hs src/hostinfo.hs src/common.hs
	cd src && ghc -o ../telework-daemon -O2 --make daemon.hs -XScopedTypeVariables


