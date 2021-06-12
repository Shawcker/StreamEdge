StreamEdge
==========

StreamEdge is a GRiSP application providing tools to facilitate the development of stream processing applications at the edge. It uses sensors and actuators connected on GRiSP boards as a source for the streams.


GRiSP project:
* [Github](https://github.com/grisp/grisp/)
* [Website](https://grisp.org)

Build
-----

    $ rebar3 compile

Deploy
------

    $ rebar3 grisp deploy -n streamedge -v 0.1.0

Emulate
-------

    $ rebar3 as emulation shell --sname my_dev_node --setcookie MyCookie test