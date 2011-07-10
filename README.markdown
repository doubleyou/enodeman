###Description

enodeman is a web-based Erlang node inspecting tool


###Requirements

enodeman requires Riak as data storage. The easiest way to set it up is to install Riak to your machine and use its default settings.
If you don't know how to install Riak, please follow [these instructions](http://wiki.basho.com/Installation-and-Setup.html).

There's also a possibility to use any other running Riak server, by editing Riak settings at etc/app.config.


###Installation and building

After all the requirements are met, just clone this project and type

    make


### Using enodeman

Just open your browser and type:
    http://localhost:8080/
