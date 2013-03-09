Another webui for etorrent.

__License__: [Apache License, Version 2.0](http://www.apache.org/licenses/LICENSE-2.0.html)

__Author__: Uvarov Michael ([`arcusfelis@gmail.com`](mailto:arcusfelis@gmail.com))


Installation and building
=========================

Run etorrent.
Open http://127.0.0.1:1080


Structure
=========

Cascadae - Erlang part of the program
-------------------------------------

http://github.com/arcusfelis/cascadae 

`priv/rhyacotriton` - symbolic link to the `build` directory of the 
Rhyacotriton. The builded version does not require installation of qooxdoo.



Rhyacotriton - Javascript part of the program
---------------------------------------------

Uses [qooxdoo](qooxdoo.org).
Source code is [here](http://github.com/arcusfelis/rhyacotriton).

The builded version of the code is 
[here](http://github.com/arcusfelis/rhyacotriton-build).
Don't edit it :) 
Also, it is a submodule of cascadae.



Etorrent bittorrent client
--------------------------

http://github.com/jlouis/etorrent  


Event prefixes
--------------

* r - the event called remotelly (from Erlang);
* d - data event.
