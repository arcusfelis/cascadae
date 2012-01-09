Another webui for etorrent.

__License__: [Apache License, Version 2.0](http://www.apache.org/licenses/LICENSE-2.0.html)

__Author__: Uvarov Michael ([`freeakk@gmail.com`](mailto:freeakk@gmail.com))


Installation and building
=========================


Cascadae uses git submodules. That is why use git clone with the `--recursive`
parameter.

```
git clone --recursive https://github.com/freeakk/cascadae.git
cd cascadae
make
./start-dev.sh
```

Run etorrent.
Open http://127.0.0.1:1080


Structure
=========

Cascadae - Erlang part of the program
-------------------------------------

http://github.com/freeakk/cascadae 

`priv/rhyacotriton` - symbolic link to the `build` directory of the 
Rhyacotriton. The builded version does not require installation of qooxdoo.



Rhyacotriton - Javascript part of the program
---------------------------------------------

Uses [qooxdoo](qooxdoo.org).
Source code is [here](http://github.com/freeakk/rhyacotriton).

The builded version of the code is 
[here](http://github.com/freeakk/rhyacotriton-build).
Don't edit it :) 
Also, it is a submodule of cascadae.



Etorrent bittorrent client
--------------------------

http://github.com/jlouis/etorrent  

