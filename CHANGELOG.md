# CHANGELOG

## 0.4.0 (2018-12-30)

* Revert swich from proto2 to proto3
* Verify authenticity of signed payload
* Add `repo_verify_origin` config option

## 0.3.0 (2018-12-14)

* Swich from proto2 to proto3
* Fix `hex_http:body` type
* Fix `hex_http:request` spec

## 0.2.1 (2018-12-13)

* Always return the whole response tuple from requests
* Guess rebar3 as build tool over rebar
* Return compressed tarball from hex_tarball:create_docs/1
* Add package identity to registry records

## 0.2.0 (2018-09-01)

* Accept `{filename(), filename()}` in files list for tarballs
* Support httpc profile through adapter configs
* Update generated protobuf files with OTP 21 compatibility

## 0.1.1 (2018-08-11)

* Use `hex_core` project name in user agent string

## 0.1.0 (2018-08-08)

* First release
