# Heaver (inspired by [Cleaver](http://jdan.github.io/cleaver))

30-second Slideshows for Hackers.

## Quick Start

Compile the binary:

`make`

And run it like so:

`./heaver path/to/file.md`

## Options

    title: Basic Example
    author:
      name: Geoffrey Vedernikoff
      twitter: yefim
      url: http://yef.im
    output: basic.html
    progress: true
    controls: true
    encoding: utf-8

Heavers supports several basic options that allow you to further customize the look and feel of your presentation, including author info, progress bar, and arrow controls.

## Overview



## Dependencies

* [split](http://hackage.haskell.org/package/split-0.2.1.1)
* [pandoc](http://hackage.haskell.org/package/pandoc-1.12.2.1)
* [yaml-light](http://hackage.haskell.org/package/yaml-light-0.1.4)
* [hastache](http://hackage.haskell.org/package/hastache)

## Contributors

* Lewis Ellis (ellisl)
* Geoffrey Vedernikoff (veg)
