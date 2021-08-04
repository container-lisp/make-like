# make-like
> An opinionated application template builder for LIKE applications

`make-like` is an application template builder for LIKE (Lisp In Kubernetes +
Emacs) applications.

It will create the basic foundation for LIKE applications, including:
* Makefile support for
  * Running the application locally
  * Running the application as a container, managed by podman, allowing you to add additional services (postgres, etc) to the pod.
* Github Actions support for CI testing
* Smart caching of quicklisp contents in the case of container builds

The application code itself is minimal, but provides some useful foundation elements:
* Prometheus metrics support
* Support for TOML-style config.ini files for application configuration
* Git tag-based application version strings
* Sly/Slime support when run in podman-managed containers
* easy-route preconfigured with health-check and static image/js/css content routes

# Tutorial

Create a `likefile` with contents similar to this:

    (make-like:make-application
      :app-name "simple"
      :author "Anthony Green <green@moxielogic.com"
      :description "A simple sample application"
      :source-header
    ";;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: SAMPLE; Base: 10 -*-
    ;;;
    ;;; Copyright (C) 2021  Anthony Green <green@moxielogic.com>
    ;;;
    ;;; This program is free software: you can redistribute it and/or
    ;;; modify it under the terms of the GNU Affero General Public License
    ;;; as published by the Free Software Foundation, either version 3 of
    ;;; the License, or (at your option) any later version.
    ;;;
    ;;; This program is distributed in the hope that it will be useful,
    ;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
    ;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
    ;;; Affero General Public License for more details.
    ;;;
    ;;; You should have received a copy of the GNU Affero General Public
    ;;; License along with this program.  If not, see
    ;;; <http://www.gnu.org/licenses/>."
      :github-account "atgreen"
      :container-registry "quay.io/moxielogic")

Run the `make-like` command:

    $ make-like likefile
    $ cd simple/
    $ find .
    .
    ./static
    ./static/js
    ./static/images
    ./static/css
    ./dist
    ./src
    ./src/package.lisp
    ./src/simple.lisp
    ./src/simple.asd
    ./README.md
    ./.gitignore
    ./.github
    ./.github/workflows
    ./.github/workflows/build.yml
    ./build
    ./build/Dockerfile.base
    ./build/Dockerfile
    ./test
    ./test/config.ini
    ./test/test.sh
    ./test/podman-start.sh
    ./test/test-pod.yml
    ./Makefile

The `make` targets look like this:

    $ make
    Supported targets:
     clean          - clean the source tree
     podman-start   - run in podman using test/test-pod.yml
     podman-stop    - stop the test pod
     run            - run locally

To run the application in a container, do this:

    $ make podman-start
    [ LOTS OF OUTPUT REDACTED ]
    Waiting for http://localhost:8080/health
    ===========================================================================

    Application: http://localhost:8080

    Prometheus : http://localhost:9101/metrics

    Use emacs to 'sly-connect' to localhost port 4005

    Run 'make podman-stop' to stop the test pod

    ===========================================================================
    This is SBCL 2.1.7, an implementation of ANSI Common Lisp.
    More information about SBCL is available at <http://www.sbcl.org/>.

    SBCL is free software, provided as is, with absolutely no warranty.
    It is mostly in the public domain; some portions are provided under
    BSD-style licenses.  See the CREDITS and COPYING files in the
    distribution for more information.
    To load "slynk":
      Load 1 ASDF system:
        slynk
    ; Loading "slynk"
    SLYNK's ASDF loader finished.
    ;; Slynk started at port: 4005.
    To load "simple":
      Load 1 ASDF system:
        simple
    ; Loading "simple"
    .........
     <INFO> [18:20:45] simple simple.lisp (start-server) -
      Starting simple version APP_VERSION
     <INFO> [18:20:45] simple simple.lisp (start-server) - Starting server
     <INFO> [18:20:47] simple simple.lisp (acceptor-dispatch-request application) - SIMPLE::*HTTP-REQUESTS-COUNTER*: #<PROMETHEUS:COUNTER name: http_requests_total {10153A6073}>
    10.0.2.100 - [2021-08-04 18:20:47] "GET /health HTTP/1.1" 200 5 "-" "curl/7.76.1"

Now just fire up emacs, run `sly-connect` against localhost, port 4005, and get cracking on your next masterpiece!
