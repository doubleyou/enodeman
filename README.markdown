###Description

enodeman is a web-based Erlang node inspecting tool


###Requirements

enodeman requires running Riak as data storage. The easiest way to set it up is to install Riak to your machine and use its default settings.
If you don't know how to install Riak, please follow [these instructions](http://wiki.basho.com/Installation-and-Setup.html).

There's also a possibility to use any other running Riak server, by editing Riak settings at etc/app.config.


###Installation and building

After all the requirements are met, just clone this project and type

    make


### Using enodeman

Just open your browser and type:
    http://localhost:8080/

### FAQ

Q: Hey, it's almost etop.
A: Not really, we also display basic node statistics, and expect to add a lot of more amazing features in the future.

Q: What's the license?
A: MIT.

Q: I want to see extended information about processes (messages queue, backtrace, etc).
A: We plan to add this soon, as well as an ability to track the exact process' statistics.

Q: I don't see a damn thing in my IE!
A: Something's terribly wrong with you man. No, seriously.
