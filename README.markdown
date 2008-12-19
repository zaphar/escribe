OVERVIEW
======

escribe is an erlang application that tracks and records online content for you. It will provide a web interface for you to direct it to feeds and will consume those feeds periodically storing the content for you to locally.

BUILD & INSTALL
===============

To build this application from the root run make && make boot. To start the application type:
    
    erl -pa ebin -pa support -boot escribe

This should drop you into an erlang shell prompt.

TODO
====

    Handlers for various protocols
    Handlers for various feed types
    Web Interface to manage the content
